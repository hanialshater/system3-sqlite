(ns sqlclj.exec
  "Statement execution: DDL, DML with constraints, and the per-line driver output."
  (:require [clojure.string :as str]
            [sqlclj.values :as v]
            [sqlclj.lexer :as lx]
            [sqlclj.parser :as p]
            [sqlclj.engine :as e :refer [err lc]]))

(set! *warn-on-reflection* true)

(def empty-db {:tables {} :indexes {} :views {} :changes 0 :index-seq 0})

(defn- new-ctx [db sql]
  (e/->Ctx db sql nil (atom 0) nil nil nil))

;; ====================================================================== CREATE TABLE

(def strict-types {"INT" :integer "INTEGER" :integer "REAL" :real "TEXT" :text "BLOB" :blob "ANY" :any})

(defn- compile-row-expr
  "Compiles an expression over a single table row (for CHECK, index expressions, partial-index WHERE).
  Returns (fn [row]) -> value."
  [db sql tbl ast]
  (let [ctx (new-ctx db sql)
        slot (e/new-slot! ctx)
        cols (mapv :name (:cols tbl))
        src {:alias (lc (:name tbl)) :tname (:name tbl) :cols cols :lcols (mapv lc cols)
             :affs (mapv :aff (:cols tbl)) :colls (mapv :coll (:cols tbl)) :slot slot :rowid? true
             :ncols (count cols) :hidden #{}}
        ctx (assoc ctx :scope {:sources [src] :parent nil})
        c (e/cx ctx ast)
        f (e/condf c)
        n (inc @(:slots ctx))]
    (assoc c :rowf (fn [row] (let [env (object-array n)] (aset env slot row) (f env))))))

(defn- build-index
  "Index record from a definition. cols: [{:e ast :coll :desc}]"
  [db sql tbl name unique cols where auto]
  (let [ncols (count (:cols tbl))
        icols (mapv (fn [ic]
                      (let [ast (:e ic)
                            ci (when (and (= :col (:op ast)) (nil? (:table ast)))
                                 (let [i (.indexOf ^java.util.List (mapv (comp lc :name) (:cols tbl)) (lc (:name ast)))]
                                   (cond (>= i 0) i
                                         (#{"rowid" "oid" "_rowid_"} (lc (:name ast))) ncols
                                         :else (err (str "no such column: " (:name ast))))))
                            c (when-not ci (compile-row-expr db sql tbl ast))]
                        {:col ci :kf (:rowf c) :name (if ci (if (< ci ncols) (:name (nth (:cols tbl) ci)) "rowid") nil)
                         :coll (if (:coll ic) (e/check-coll (:coll ic))
                                   (if ci (or (when (< ci ncols) (:coll (nth (:cols tbl) ci))) "BINARY")
                                       (or (:coll c) "BINARY")))
                         :desc (:desc ic)}))
                    cols)
        wf (when where
             (let [c (compile-row-expr db sql tbl where)
                   rf (:rowf c)]
               (fn [row] (let [x (rf row)] (and (not (nil? x)) (v/bool-value x))))))]
    {:name name :table (lc (:name tbl)) :unique unique :cols icols :where where :where-ast where :wf wf :auto auto}))

(defn- table-indexes
  "Indexes of a table in constraint-check order (most recently created first)."
  [db tname]
  (->> (vals (:indexes db))
       (filter #(= (:table %) (lc tname)))
       (sort-by :seq >)))

(defn- add-index [db idx]
  (let [s (inc (long (:index-seq db)))]
    (-> db (assoc :index-seq s) (assoc-in [:indexes (lc (:name idx))] (assoc idx :seq s)))))

(defn- create-table [db sql st]
  (let [name (:name st) lname (lc name)]
    (cond
      (or (get-in db [:tables lname]) (get-in db [:views lname]))
      (if (:if-not-exists st) db (err (str (if (get-in db [:views lname]) "view " "table ") name " already exists")))
      (str/starts-with? lname "sqlite_") (err (str "object name reserved for internal use: " name))
      (:as st)
      (let [ctx (new-ctx db sql)
            sub (e/compile-select ctx (:as st))
            rows (e/run-select-rows sub (object-array (inc @(:slots ctx))))
            cols (mapv (fn [n a] {:name n :type (case a :integer "INT" :real "REAL" :text "TEXT" :numeric "NUM" "")
                                  :aff (or a :blob)})
                       (:names sub) (:affs sub))
            tbl {:name name :cols cols :ipk nil :strict false :checks []
                 :rows (into (sorted-map) (map-indexed (fn [i r] [(long (inc i)) (conj (vec r) (long (inc i)))]) rows))}]
        (assoc-in db [:tables lname] tbl))
      :else
      (let [strict (:strict st)
            defs (:cols st)
            _ (let [seen (atom #{})]
                (doseq [d defs]
                  (when (@seen (lc (:name d))) (err (str "duplicate column name: " (:name d))))
                  (swap! seen conj (lc (:name d)))))
            cols (mapv (fn [d]
                         (let [cons (:cons d)
                               t (:type d)
                               aff (if strict
                                     (let [u (when t (str/upper-case t))]
                                       (cond (nil? t) (err (str "missing datatype for " name "." (:name d)))
                                             (strict-types u) (let [a (strict-types u)] (if (= a :any) :blob a))
                                             :else (err (str "unknown datatype for " name "." (:name d) ": \"" t "\""))))
                                     (v/type-affinity t))]
                           {:name (:name d) :type t :aff aff
                            :strict-type (when strict (strict-types (str/upper-case t)))
                            :notnull (some #(= :notnull (:c %)) cons)
                            :default (:e (first (filter #(= :default (:c %)) cons)))
                            :coll (when-let [c (:coll (last (filter #(= :collate (:c %)) cons)))] (e/check-coll c))
                            :pk (first (filter #(= :pk (:c %)) cons))
                            :unique (filter #(= :unique (:c %)) cons)}))
                       defs)
            col-pks (keep-indexed (fn [i c] (when (:pk c) i)) cols)
            tpks (filter #(= :pk (:c %)) (:tcons st))
            _ (when (> (+ (count col-pks) (count tpks)) 1)
                (err (str "table \"" name "\" has more than one primary key")))
            lnames (mapv (comp lc :name) cols)
            col-idx (fn [n] (let [i (.indexOf ^java.util.List lnames (lc n))]
                              (if (neg? i) (err (str "no such column: " n)) i)))
            pk-cols (cond (seq col-pks) [{:i (first col-pks) :desc (= :desc (:dir (:pk (nth cols (first col-pks)))))}]
                          (seq tpks) (mapv (fn [ic]
                                             (when-not (= :col (:op (:e ic)))
                                               (err "expressions prohibited in PRIMARY KEY and UNIQUE constraints"))
                                             {:i (col-idx (:name (:e ic))) :desc (:desc ic) :coll (:coll ic)})
                                           (:cols (first tpks)))
                          :else nil)
            ipk (when (and (= 1 (count pk-cols))
                           (let [c (nth cols (:i (first pk-cols)))]
                             (and (:type c) (= "INTEGER" (str/upper-case (:type c))))
                             )
                           (not (and (seq col-pks) (:desc (first pk-cols)))))
                  (:i (first pk-cols)))
            cols (if ipk (assoc-in cols [ipk :aff] :integer) cols)
            checks (vec (concat
                         (for [[c d] (map vector cols defs), k (:cons d) :when (= :check (:c k))]
                           {:e (:e k) :name (:name k) :text (subs sql (:ps (:e k)) (:pe (:e k)))})
                         (for [k (:tcons st) :when (= :check (:c k))]
                           {:e (:e k) :name (:name k) :text (subs sql (:ps (:e k)) (:pe (:e k)))})))
            tbl {:name name :cols cols :ipk ipk :strict strict :checks [] :rows (sorted-map)
                 :without-rowid (:without-rowid st)}
            db (assoc-in db [:tables lname] tbl)
            ;; compile checks against the table
            tbl (assoc tbl :checks (mapv (fn [ck] (assoc ck :f (:rowf (compile-row-expr db sql tbl (:e ck))))) checks))
            db (assoc-in db [:tables lname] tbl)
            ;; autoindexes: PK (non-ipk) and UNIQUE in declaration order
            uniq-defs (concat
                       (for [[i c] (map-indexed vector cols)
                             :let [items (concat (when (and (:pk c) (not= i ipk)) [[:pk]])
                                                 (map (fn [_] [:u]) (:unique c)))]
                             it items]
                         [{:e {:op :col :name (:name c)} :coll nil :desc false}])
                       (for [k (:tcons st) :when (or (= :unique (:c k)) (and (= :pk (:c k)) (nil? ipk)))]
                         (:cols k)))
            db (reduce (fn [db [n icols]]
                         (let [tbl (get-in db [:tables lname])
                               idx (build-index db sql tbl (str "sqlite_autoindex_" name "_" n) true icols nil true)]
                           ;; skip duplicate identical unique constraints
                           (if (some #(and (:auto %) (= (mapv (juxt :col :coll) (:cols %)) (mapv (juxt :col :coll) (:cols idx))))
                                     (table-indexes db lname))
                             db
                             (add-index db idx))))
                       db (map-indexed (fn [i c] [(inc i) c]) uniq-defs))]
        db))))

;; ====================================================================== constraint checks

(defn- strict-check [tbl ci val]
  (let [c (nth (:cols tbl) ci)
        st (:strict-type c)]
    (if (or (nil? st) (nil? val) (= st :any))
      val
      (let [ok (case st
                 :integer (instance? Long val)
                 :real (instance? Double val)
                 :text (string? val)
                 :blob (v/blob? val))]
        (if ok val
            (err (str "cannot store " (let [tn (str/upper-case (v/type-name val))] (if (= tn "INTEGER") "INT" tn))
                      " value in "
                      (case st :integer (str/upper-case (or (:type c) "INTEGER")) :real "REAL" :text "TEXT" :blob "BLOB")
                      " column " (:name tbl) "." (:name c))))))))

(defn- apply-col-affinity [tbl ci val]
  (v/apply-affinity val (:aff (nth (:cols tbl) ci))))

(defn- key-has-null? [k] (some nil? k))

(defn- find-conflict
  "Returns rowid of an existing row conflicting with row r under unique index idx, excluding rowid self."
  [tbl idx row self]
  (when (or (nil? (:wf idx)) ((:wf idx) row))
    (let [k (e/index-key tbl idx row)]
      (when-not (key-has-null? k)
        (let [colls (mapv :coll (:cols idx))]
          (some (fn [[rid r]]
                  (when (and (not= rid self)
                             (or (nil? (:wf idx)) ((:wf idx) r))
                             (let [k2 (e/index-key tbl idx r)]
                               (every? true? (map (fn [a b c] (and (not (nil? b)) (zero? (v/sql-cmp a b c)))) k k2 colls))))
                    rid))
                (:rows tbl)))))))

(defn- unique-msg [tbl idx]
  (str "UNIQUE constraint failed: "
       (str/join ", " (map (fn [ic] (if (:col ic) (str (:name tbl) "." (:name ic)) "index '" ))
                           (:cols idx)))))

(defn- unique-msg* [tbl idx]
  (if (every? :col (:cols idx))
    (str "UNIQUE constraint failed: " (str/join ", " (map #(str (:name tbl) "." (:name %)) (:cols idx))))
    (str "UNIQUE constraint failed: index '" (:name idx) "'")))

(defn- default-value [db sql tbl ci]
  (let [d (:default (nth (:cols tbl) ci))]
    (when d
      (let [ctx (new-ctx db sql)
            c (e/cx ctx d)]
        ((:f c) (object-array 1))))))

(defn- check-notnull
  "NOT NULL constraints. Returns the row (possibly with REPLACE defaults) or nil for IGNORE."
  [db sql tbl row on-conflict]
  (reduce (fn [row ci]
            (let [c (nth (:cols tbl) ci)]
              (if (and (:notnull c) (nil? (nth row ci)) (not= ci (:ipk tbl)))
                (case on-conflict
                  :ignore (reduced nil)
                  :replace (let [d (when (:default c) (v/apply-affinity (default-value db sql tbl ci) (:aff c)))]
                             (if (nil? d)
                               (err (str "NOT NULL constraint failed: " (:name tbl) "." (:name c)))
                               (assoc row ci d)))
                  (err (str "NOT NULL constraint failed: " (:name tbl) "." (:name c))))
                row)))
          row (range (count (:cols tbl)))))

(defn- check-strict [tbl row]
  (when (:strict tbl)
    (doseq [ci (range (count (:cols tbl)))]
      (when (not= ci (:ipk tbl)) (strict-check tbl ci (nth row ci)))))
  row)

(defn- check-checks
  "CHECK constraints. Returns row, or nil for IGNORE."
  [tbl row on-conflict]
  (let [bad (some (fn [ck] (let [x ((:f ck) row)] (when (and (not (nil? x)) (not (v/bool-value x))) ck)))
                  (:checks tbl))]
    (if bad
      (if (= on-conflict :ignore)
        nil
        (err (str "CHECK constraint failed: " (or (:name bad) (:text bad)))))
      row)))

(defn- check-row
  "NOT NULL, STRICT types, CHECK. Returns [row action] where action is :ok or :ignore."
  [db sql tbl row on-conflict]
  (let [r (check-notnull db sql tbl row on-conflict)
        r (when r (check-strict tbl r))
        r (when r (check-checks tbl r on-conflict))]
    (if r [r :ok] [nil :ignore])))

(defn- put-row [tbl rid row]
  (assoc-in tbl [:rows rid] (assoc row (count (:cols tbl)) rid)))

(defn- resolve-uniques
  "Checks rowid and unique-index conflicts for row (with rowid rid) excluding self.
  mid-check runs after the rowid check and before the index checks (STRICT type checks).
  With delay-ipk the rowid check runs last (an upsert whose first target is an index).
  Returns [tbl' action] where action :ok (tbl' has conflicting rows deleted for REPLACE), :ignore,
  or [:upsert target]."
  ([db tbl rid row self on-conflict upsert-target?]
   (resolve-uniques db tbl rid row self on-conflict upsert-target? nil false))
  ([db tbl rid row self on-conflict upsert-target? mid-check delay-ipk]
   (let [idxs (filter :unique (table-indexes db (:name tbl)))]
     (letfn [(handle [tbl conf msg what]
               (cond
                 (upsert-target? what) [:upsert what]
                 (= on-conflict :ignore) :ignore
                 (= on-conflict :replace) (update tbl :rows dissoc conf)
                 :else (err msg)))
             (rowid-step [tbl]
               (if (and (not= rid self) (contains? (:rows tbl) rid))
                 (handle tbl rid (str "UNIQUE constraint failed: " (:name tbl) "."
                                      (if (:ipk tbl) (:name (nth (:cols tbl) (:ipk tbl))) "rowid"))
                         :rowid)
                 tbl))
             (index-steps [tbl]
               (loop [tbl tbl idxs idxs]
                 (if (empty? idxs)
                   tbl
                   (let [idx (first idxs)
                         conf (find-conflict tbl idx row self)]
                     (if conf
                       (let [r (handle tbl conf (unique-msg* tbl idx) idx)]
                         (if (map? r) (recur r (rest idxs)) r))
                       (recur tbl (rest idxs)))))))]
       (let [r (if delay-ipk tbl (rowid-step tbl))]
         (if-not (map? r)
           [tbl r]
           (do (when mid-check (mid-check))
               (let [r2 (index-steps r)]
                 (if-not (map? r2)
                   [tbl r2]
                   (if delay-ipk
                     (let [r3 (rowid-step r2)] (if (map? r3) [r3 :ok] [tbl r3]))
                     [r2 :ok]))))))))))

;; ====================================================================== INSERT

(defn- table-source [ctx tbl alias]
  (let [slot (e/new-slot! ctx)
        cols (mapv :name (:cols tbl))]
    {:alias (lc (or alias (:name tbl))) :tname (:name tbl) :cols cols :lcols (mapv lc cols)
     :affs (mapv :aff (:cols tbl)) :colls (mapv :coll (:cols tbl)) :slot slot :rowid? true
     :ncols (count cols) :hidden #{}}))

(defn- compile-returning [ctx src items]
  (when (seq items)
    (let [ctx (assoc ctx :scope {:sources [src] :parent nil})
          cols (mapcat (fn [rc]
                         (if (:star rc)
                           (for [i (range (:ncols src))]
                             {:name (nth (:cols src) i)
                              :f (let [sl (long (:slot src)) i (long i)] (fn [^objects env] (nth (aget env sl) i)))})
                           [{:name (or (:alias rc) (if (= :col (:op (:e rc))) (:name (:e rc)) (e/text-of-node ctx (:e rc))))
                             :f (:f (e/cx ctx (:e rc)))}]))
                       items)]
      {:names (mapv :name cols) :fs (mapv :f cols) :slot (:slot src)})))

(defn- next-rowid [tbl]
  (if (empty? (:rows tbl))
    1
    (let [m (long (key (first (rseq (:rows tbl)))))]
      (if (= m Long/MAX_VALUE) (err "database or disk is full") (inc m)))))

(defn- upsert-matcher
  "Returns (fn [conflict-target]) deciding whether an upsert clause applies to a conflict."
  [tbl clause]
  (let [target (:target clause)]
    (fn [what]
      (cond
        (nil? clause) false
        (nil? target) true
        (= what :rowid) (and (:ipk tbl) (= 1 (count (:cols target)))
                             (= (lc (:name (:e (first (:cols target))))) (lc (:name (nth (:cols tbl) (:ipk tbl))))))
        (map? what) (= (set (map (comp lc :name :e) (:cols target)))
                       (set (map (comp lc :name) (:cols what))))
        :else false))))

(defn- exec-insert [db sql st emit prepared!]
  (let [tname (:table st)
        tbl (or (get-in db [:tables (lc tname)])
                (err (str "no such table: " tname)))
        ncols (count (:cols tbl))
        lnames (mapv (comp lc :name) (:cols tbl))
        targets (if (:default st) [] (if (:cols st)
                  (mapv (fn [c] (let [i (.indexOf ^java.util.List lnames (lc c))]
                                  (cond (>= i 0) i
                                        (#{"rowid" "oid" "_rowid_"} (lc c)) (or (:ipk tbl) -1)
                                        :else (err (str "table " (:name tbl) " has no column named " c)))))
                        (:cols st))
                  (vec (range ncols))))
        ctx (new-ctx db sql)
        on-conflict (or (:or st) :abort)
        ;; source rows
        rows (cond
               (:default st) [[]]
               (:rows st) (let [rs (:rows st)]
                            (doseq [r rs]
                              (when (not= (count r) (count targets))
                                (if (:cols st)
                                  (err (str (count r) " values for " (count targets) " columns"))
                                  (err (str "table " (:name tbl) " has " ncols " columns but " (count r) " values were supplied")))))
                            (mapv (fn [r] (mapv #(:f (e/cx ctx %)) r)) rs))
               (:select st) (let [sub (e/compile-select ctx (:select st))]
                              (when (not= (count (:names sub)) (count targets))
                                (if (:cols st)
                                  (err (str (count (:names sub)) " values for " (count targets) " columns"))
                                  (err (str "table " (:name tbl) " has " ncols " columns but " (count (:names sub)) " values were supplied"))))
                              sub))
        upserts (:upsert st)
        ;; upsert compile: scope has the table (existing row) and "excluded"
        up-ctx (when (seq upserts) ctx)
        tsrc (when (seq upserts) (table-source ctx tbl nil))
        xsrc (when (seq upserts) (assoc (table-source ctx tbl "excluded") :rowid? false :qualified-only true))
        up-compiled (when (seq upserts)
                      (let [uctx (assoc ctx :scope {:sources [tsrc xsrc] :parent nil})]
                        (mapv (fn [u]
                                (when (and (:target u) (not (some (fn [idx] ((upsert-matcher tbl u) idx))
                                                                  (cons :rowid (filter :unique (table-indexes db tname))))))
                                  (err "ON CONFLICT clause does not match any PRIMARY KEY or UNIQUE constraint"))
                                (assoc u
                                       :sets-c (when (= :update (:do u))
                                                 (vec (for [[cols ex] (:sets u), c cols]
                                                        [(let [i (.indexOf ^java.util.List lnames (lc c))]
                                                           (if (neg? i) (err (str "no such column: " c)) i))
                                                         (:f (e/cx uctx ex))])))
                                       :where-c (when (:where u) (:f (e/cx uctx (:where u))))
                                       :match (upsert-matcher tbl u)))
                              upserts)))
        ret-src (when (seq (:returning st)) (table-source ctx tbl nil))
        ret (when ret-src (compile-returning ctx ret-src (:returning st)))
        nslots (inc @(:slots ctx))
        env (object-array (+ nslots 2))
        _ (prepared! (:names ret))
        src-rows (if (map? rows) (e/run-select-rows rows env) rows)
        ret-rows (java.util.ArrayList.)]
    (loop [db db rs (seq src-rows) changes 0]
      (if (empty? rs)
        (do (doseq [r ret-rows] (emit r))
            (assoc db :changes changes))
        (let [tbl (get-in db [:tables (lc tname)])
              r (first rs)
              vals (if (map? rows) r (mapv #(% env) r))
              row (vec (repeat (inc ncols) nil))
              ;; defaults first
              provided (set targets)
              row (reduce (fn [row ci]
                            (if (provided ci) row (assoc row ci (default-value db sql tbl ci))))
                          row (range ncols))
              rid-given (atom nil)
              row (reduce (fn [row [ci val]]
                            (if (= ci -1) (do (reset! rid-given val) row) (assoc row ci val)))
                          row (map vector targets vals))
              ipk (:ipk tbl)
              rid-val (if ipk (nth row ipk) @rid-given)
              rid (if (nil? rid-val)
                    (next-rowid tbl)
                    (let [x (v/apply-affinity rid-val :integer)]
                      (if (instance? Long x) x (err "datatype mismatch"))))
              row (if ipk (assoc row ipk rid) row)
              raw-row row
              row (reduce (fn [row ci] (if (= ci ipk) row (assoc row ci (apply-col-affinity tbl ci (nth row ci)))))
                          row (range ncols))
              row (check-notnull db sql tbl row on-conflict)
              has-checks (seq (:checks tbl))
              row (when row (if has-checks (check-checks tbl (check-strict tbl row) on-conflict) row))
              m-up (fn [what] (boolean (some #((:match %) what) up-compiled)))
              first-up (first up-compiled)
              delay-ipk (boolean (and first-up (:target first-up) (not ((:match first-up) :rowid))))
              ipk-hit (when (and row (seq up-compiled) (not delay-ipk) (contains? (:rows tbl) rid) (m-up :rowid)) :rowid)]
          (if (nil? row)
            (recur db (rest rs) changes)
            (let [[tbl2 res] (if ipk-hit
                               [tbl [:upsert ipk-hit]]
                               (resolve-uniques db tbl rid row nil on-conflict m-up
                                                (fn [] (when-not has-checks (check-strict tbl row)))
                                                delay-ipk))]
              (cond
                (= res :ignore) (recur db (rest rs) changes)
                (vector? res)
                ;; upsert
                (let [conf (second res)
                      up (first (filter #((:match %) conf) up-compiled))
                      crid (if (= conf :rowid) rid (find-conflict tbl conf row nil))
                      existing (get-in tbl [:rows crid])]
                  (if (= :nothing (:do up))
                    (recur db (rest rs) changes)
                    (do
                      (aset env (long (:slot tsrc)) existing)
                      (aset env (long (:slot xsrc))
                            (assoc (if has-checks
                                     row
                                     (reduce (fn [r ci] (if (and (= :real (:aff (nth (:cols tbl) ci))) (instance? Long (nth r ci)))
                                                          (assoc r ci (double (long (nth r ci)))) r))
                                             raw-row (range ncols)))
                                   ncols rid))
                      (if (and (:where-c up) (let [w ((:where-c up) env)] (or (nil? w) (not (v/bool-value w)))))
                        (recur db (rest rs) changes)
                        (let [nrow (reduce (fn [r [ci f]] (assoc r ci (f env))) existing (:sets-c up))
                              nrid (if ipk (let [x (v/apply-affinity (nth nrow ipk) :integer)]
                                             (if (instance? Long x) x (err "datatype mismatch")))
                                       crid)
                              nrow (if ipk (assoc nrow ipk nrid) nrow)
                              nrow (reduce (fn [r ci] (if (= ci ipk) r (assoc r ci (apply-col-affinity tbl ci (nth r ci)))))
                                           nrow (map first (:sets-c up)))
                              [nrow _] (check-row db sql tbl nrow :abort)
                              [tblu res2] (resolve-uniques db tbl nrid nrow crid :abort (constantly false))
                              tblu (put-row (update tblu :rows dissoc crid) nrid nrow)]
                          (when ret
                            (aset env (long (:slot ret)) (assoc nrow ncols nrid))
                            (.add ret-rows (mapv #(% env) (:fs ret))))
                          (recur (assoc-in db [:tables (lc tname)] tblu) (rest rs) (inc changes)))))))
                :else
                (let [tbl3 (put-row tbl2 rid row)
                      db2 (assoc-in db [:tables (lc tname)] tbl3)]
                  (when ret
                    (aset env (long (:slot ret)) (assoc row ncols rid))
                    (.add ret-rows (mapv #(% env) (:fs ret))))
                  (recur db2 (rest rs) (inc changes)))))))))))

;; ====================================================================== UPDATE / DELETE

(defn- exec-update [db sql st emit prepared!]
  (let [tname (:table st)
        tbl (or (get-in db [:tables (lc tname)]) (err (str "no such table: " tname)))
        ncols (count (:cols tbl))
        lnames (mapv (comp lc :name) (:cols tbl))
        ctx (new-ctx db sql)
        src (table-source ctx tbl (:alias st))
        _ (when (and (:indexed-by st) (not= :none (:indexed-by st)))
            (when-not (get-in db [:indexes (lc (:indexed-by st))])
              (err (str "no such index: " (:indexed-by st)))))
        sctx (assoc ctx :scope {:sources [src] :parent nil})
        sets (vec (for [[cols ex] (:sets st)
                        :let [f (:f (e/cx sctx ex))]
                        c cols]
                    [(let [i (.indexOf ^java.util.List lnames (lc c))]
                       (cond (>= i 0) i
                             (#{"rowid" "oid" "_rowid_"} (lc c)) (or (:ipk tbl) -1)
                             :else (err (str "no such column: " c))))
                     f]))
        where-f (when (:where st) (e/condf (e/cx sctx (:where st))))
        ret (compile-returning ctx src (:returning st))
        on-conflict (or (:or st) :abort)
        env (object-array (+ 2 @(:slots ctx)))
        slot (long (:slot src))
        _ (prepared! (:names ret))
        targets (vec (filter (fn [[rid row]]
                               (aset env slot row)
                               (or (nil? where-f) (let [w (where-f env)] (and (not (nil? w)) (v/bool-value w)))))
                             (:rows tbl)))
        ret-rows (java.util.ArrayList.)]
    (loop [db db ts targets changes 0]
      (if (empty? ts)
        (do (doseq [r ret-rows] (emit r)) (assoc db :changes changes))
        (let [[rid _] (first ts)
              tbl (get-in db [:tables (lc tname)])
              old (get-in tbl [:rows rid])]
          (if (nil? old)
            (recur db (rest ts) changes)
            (let [_ (aset env slot old)
                  newvals (mapv (fn [[ci f]] [ci (f env)]) sets)
                  row (reduce (fn [row [ci val]] (if (= ci -1) row (assoc row ci val))) old newvals)
                  rid-new (let [rv (some (fn [[ci val]] (when (= ci -1) val)) newvals)
                                rv (if (:ipk tbl) (nth row (:ipk tbl)) (if (some #(= -1 (first %)) newvals) rv rid))]
                            (if (nil? rv)
                              (if (:ipk tbl) (next-rowid tbl) (err "datatype mismatch"))
                              (let [x (v/apply-affinity rv :integer)]
                                (if (instance? Long x) x (err "datatype mismatch")))))
                  row (if (:ipk tbl) (assoc row (:ipk tbl) rid-new) row)
                  changed (set (map first newvals))
                  row (reduce (fn [row ci] (if (or (= ci (:ipk tbl)) (not (changed ci))) row
                                               (assoc row ci (apply-col-affinity tbl ci (nth row ci)))))
                              row (range ncols))
                  has-checks (seq (:checks tbl))
                  row (check-notnull db sql tbl row on-conflict)
                  row (when row (if has-checks (check-checks tbl (check-strict tbl row) on-conflict) row))]
              (if (nil? row)
                (recur db (rest ts) changes)
                (let [[tbl2 res] (resolve-uniques db tbl rid-new row rid on-conflict (constantly false)
                                                  (fn [] (when-not has-checks (check-strict tbl row))) false)]
                  (if (= res :ignore)
                    (recur db (rest ts) changes)
                    (let [tbl3 (put-row (update tbl2 :rows dissoc rid) rid-new row)
                          db2 (assoc-in db [:tables (lc tname)] tbl3)]
                      (when ret
                        (aset env slot (assoc row ncols rid-new))
                        (.add ret-rows (mapv #(% env) (:fs ret))))
                      (recur db2 (rest ts) (inc changes)))))))))))))

(defn- exec-delete [db sql st emit prepared!]
  (let [tname (:table st)
        tbl (or (get-in db [:tables (lc tname)]) (err (str "no such table: " tname)))
        ctx (new-ctx db sql)
        src (table-source ctx tbl (:alias st))
        _ (when (and (:indexed-by st) (not= :none (:indexed-by st)))
            (when-not (get-in db [:indexes (lc (:indexed-by st))])
              (err (str "no such index: " (:indexed-by st)))))
        sctx (assoc ctx :scope {:sources [src] :parent nil})
        where-f (when (:where st) (e/condf (e/cx sctx (:where st))))
        ret (compile-returning ctx src (:returning st))
        env (object-array (+ 2 @(:slots ctx)))
        slot (long (:slot src))
        _ (prepared! (:names ret))
        doomed (vec (filter (fn [[rid row]]
                              (aset env slot row)
                              (or (nil? where-f) (let [w (where-f env)] (and (not (nil? w)) (v/bool-value w)))))
                            (:rows tbl)))]
    (when ret
      (doseq [[_ row] doomed]
        (aset env slot row)
        (emit (mapv #(% env) (:fs ret)))))
    (-> db
        (assoc-in [:tables (lc tname) :rows] (apply dissoc (:rows tbl) (map first doomed)))
        (assoc :changes (count doomed)))))

;; ====================================================================== other DDL

(defn- create-index [db sql st]
  (let [name (:name st) lname (lc name)
        tbl (get-in db [:tables (lc (:table st))])]
    (cond
      (get-in db [:indexes lname]) (if (:if-not-exists st) db (err (str "index " name " already exists")))
      (get-in db [:tables lname]) (err (str "there is already a table named " name))
      (nil? tbl) (err (str "no such table: main." (:table st)))
      (str/starts-with? lname "sqlite_") (err (str "object name reserved for internal use: " name))
      :else
      (let [idx (build-index db sql tbl name (:unique st) (:cols st) (:where st) false)]
        ;; uniqueness of existing data
        (when (:unique st)
          (let [rows (filter #(or (nil? (:wf idx)) ((:wf idx) %)) (e/table-rows tbl))
                colls (mapv :coll (:cols idx))
                ts (java.util.TreeSet. ^java.util.Comparator (e/row-comparator colls))]
            (doseq [r rows]
              (let [k (e/index-key tbl idx r)]
                (when-not (key-has-null? k)
                  (when-not (.add ts k)
                    (err (unique-msg* tbl idx))))))))
        (add-index db idx)))))

(defn- exec-drop [db st]
  (let [lname (lc (:name st))]
    (case (:what st)
      :table (if-let [t (get-in db [:tables lname])]
               (-> db
                   (update :tables dissoc lname)
                   (update :indexes (fn [m] (into {} (remove (fn [[_ i]] (= (:table i) lname)) m)))))
               (if (:if-exists st) db (err (str "no such table: " (:name st)))))
      :index (if-let [i (get-in db [:indexes lname])]
               (if (:auto i)
                 (err "index associated with UNIQUE or PRIMARY KEY constraint cannot be dropped")
                 (update db :indexes dissoc lname))
               (if (:if-exists st) db (err (str "no such index: " (:name st)))))
      :view (if (get-in db [:views lname])
              (update db :views dissoc lname)
              (if (:if-exists st) db (err (str "no such view: " (:name st)))))
      :trigger (if (:if-exists st) db (err (str "no such trigger: " (:name st)))))))

(defn- exec-reindex
  "Returns db, or ::empty when there is nothing to rebuild (SQLite then prepares no statement)."
  [db st]
  (let [n (lc (:name st))
        idxs (vals (:indexes db))
        hit (cond
              (nil? n) (seq idxs)
              (#{"binary" "nocase" "rtrim"} n)
              (some (fn [i] (some #(= (str/upper-case n) (:coll %)) (:cols i))) idxs)
              (get-in db [:tables n]) (some #(= (:table %) n) idxs)
              (get-in db [:indexes n]) true
              :else (err "unable to identify the object to be reindexed"))]
    (if hit db ::empty)))

(defn- exec-alter [db sql st]
  (let [tname (lc (:table st))
        tbl (or (get-in db [:tables tname]) (err (str "no such table: " (:table st))))]
    (case (:action st)
      :rename (let [nn (:to st)]
                (when (get-in db [:tables (lc nn)]) (err (str "there is already another table or index with this name: " nn)))
                (-> db
                    (update :tables dissoc tname)
                    (assoc-in [:tables (lc nn)] (assoc tbl :name nn))
                    (update :indexes (fn [m] (into {} (map (fn [[k i]] [k (if (= (:table i) tname) (assoc i :table (lc nn)) i)]) m))))))
      :add (let [d (:col st)
                 c {:name (:name d) :type (:type d) :aff (v/type-affinity (:type d))
                    :notnull (some #(= :notnull (:c %)) (:cons d))
                    :default (:e (first (filter #(= :default (:c %)) (:cons d))))
                    :coll (when-let [c (:coll (first (filter #(= :collate (:c %)) (:cons d))))] (e/check-coll c))}
                 n (count (:cols tbl))
                 dv (when (:default c) ((:f (e/cx (new-ctx db sql) (:default c))) (object-array 1)))
                 tbl2 (-> tbl
                          (update :cols conj c)
                          (update :rows (fn [rows] (into (sorted-map) (map (fn [[rid r]] [rid (conj (pop r) dv rid)]) rows)))))]
             (assoc-in db [:tables tname] tbl2))
      :rename-col (let [i (.indexOf ^java.util.List (mapv (comp lc :name) (:cols tbl)) (lc (:old st)))]
                    (when (neg? i) (err (str "no such column: \"" (:old st) "\"")))
                    (assoc-in db [:tables tname :cols i :name] (:to st)))
      :drop-col (err "ALTER TABLE DROP COLUMN is not supported"))))

;; ====================================================================== driver

(defn- readonly? [st]
  (#{:select :begin :commit :rollback :pragma} (:kind st)))

(defn- emit-line [^StringBuilder out ^String s]
  (.append out s) (.append out "\n"))

(defn- write-row [^StringBuilder out row]
  (let [n (count row)]
    (dotimes [i n]
      (when (pos? i) (.append out "|"))
      (.append out (v/display (nth row i))))
    (.append out "\n")))

(def ^:dynamic *prepared* (volatile! false))

(defn exec-statement
  "Executes one parsed statement. Writes its output into out. Returns the new db. Throws on error
  (after any rows already written)."
  [db sql st ^StringBuilder out]
  (case (:kind st)
    :select
    (let [ctx (new-ctx db sql)
          sub (e/compile-select ctx (:select st))]
      (emit-line out (str "# " (str/join "|" (:names sub))))
      (let [env (object-array (+ 2 @(:slots ctx)))]
        (if-let [run (:streaming sub)]
          ;; stream rows so that rows before a runtime error are printed
          (let [limit (:limit sub) offset (or (:offset sub) 0)
                cnt (long-array 1)]
            (try
              (run env (fn [row _]
                         (let [i (aget cnt 0)]
                           (aset cnt 0 (inc i))
                           (when (>= i (long (max 0 (long offset))))
                             (if (and limit (>= (long limit) 0) (>= (- i (max 0 (long offset))) (long limit)))
                               (throw (ex-info "stop" {::stop true}))
                               (write-row out row))))))
              (catch clojure.lang.ExceptionInfo ex
                (when-not (::stop (ex-data ex)) (throw ex)))))
          (doseq [row (e/run-select-rows sub env)] (write-row out row))))
      db)
    (:insert :update :delete)
    (let [rows (java.util.ArrayList.)
          f (case (:kind st) :insert exec-insert :update exec-update :delete exec-delete)
          db2 (f db sql st (fn [r] (.add rows r))
                 (fn [names]
                   (when names (emit-line out (str "# " (str/join "|" names))))
                   (vreset! *prepared* true)))]
      (if (seq (:returning st))
        (doseq [r rows] (write-row out r))
        (emit-line out (str "changes: " (:changes db2))))
      db2)
    (let [db2 (case (:kind st)
                :create-table (create-table db sql st)
                :create-index (create-index db sql st)
                :create-view (let [ln (lc (:name st))]
                               (cond (or (get-in db [:views ln]) (get-in db [:tables ln]))
                                     (if (:if-not-exists st) db (err (str (if (get-in db [:views ln]) "view " "table ") (:name st) " already exists")))
                                     :else (do (e/compile-select (new-ctx db sql) (:select st))
                                               (assoc-in db [:views ln] {:name (:name st) :cols (:cols st) :select (:select st)}))))
                :drop (exec-drop db st)
                :reindex (exec-reindex db st)
                :alter (exec-alter db sql st)
                (:begin :commit :rollback :noop :pragma) db)]
      (cond
        (= db2 ::empty) db
        :else (do (when-not (readonly? st)
                    (emit-line out (str "changes: " (:changes db2))))
                  db2)))))

(defn run-line
  "Runs one input line; returns [db' output]."
  [db ^String line]
  (let [out (StringBuilder.)
        ps (p/parse-statements line)]
    (loop [db db]
      (let [st (try (p/next-statement ps)
                    (catch clojure.lang.ExceptionInfo ex
                      (if-let [m (:sql-error (ex-data ex))]
                        (do (emit-line out (str "error: " m)) ::stop)
                        (throw ex))))]
        (cond
          (= st ::stop) [db (.toString out)]
          (nil? st) [db (.toString out)]
          :else
          (let [prep (volatile! false)
                r (try (binding [*prepared* prep] (exec-statement db line st out))
                       (catch clojure.lang.ExceptionInfo ex
                         (if-let [m (:sql-error (ex-data ex))]
                           (do (emit-line out (str "error: " m))
                               (if @prep [::stop (assoc db :changes 0)] ::stop))
                           (throw ex)))
                       (catch StackOverflowError _
                         (emit-line out "error: parser stack overflow") ::stop))]
            (cond
              (= r ::stop) [db (.toString out)]
              (vector? r) [(second r) (.toString out)]
              :else (recur r))))))))

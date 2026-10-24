(ns sqlclj.exec
  "Statement execution: DDL, DML with constraints, and the per-line driver output."
  (:require [clojure.string :as str]
            [sqlclj.values :as v]
            [sqlclj.lexer :as lx]
            [sqlclj.parser :as p]
            [sqlclj.engine :as e :refer [err lc]]
            [sqlclj.schema :as sch]
            [sqlclj.trigger :as trg]
            [sqlclj.pragma :as pragma]))

(set! *warn-on-reflection* true)

(def empty-db {:tables {} :indexes {} :views {} :changes 0 :index-seq 0})

(defn- new-ctx [db sql]
  (cond-> (assoc (e/->Ctx db sql nil (atom 0) nil nil nil) :refs (atom #{}))
    e/*with* (e/add-ctes e/*with*)))

;; ====================================================================== CREATE TABLE

(def strict-types {"INT" :integer "INTEGER" :integer "REAL" :real "TEXT" :text "BLOB" :blob "ANY" :any})

(defn- compile-row-expr
  "Compiles an expression over a single table row (for CHECK, index expressions, partial-index WHERE).
  Returns (fn [row]) -> value. mode :check evaluates as a CHECK constraint (deciding FALSE)."
  ([db sql tbl ast] (compile-row-expr db sql tbl ast nil))
  ([db sql tbl ast mode]
  (let [ctx (new-ctx db sql)
        slot (e/new-slot! ctx)
        cols (mapv :name (:cols tbl))
        src {:alias (lc (:name tbl)) :tname (:name tbl) :cols cols :lcols (mapv lc cols)
             :affs (mapv :aff (:cols tbl)) :colls (mapv :coll (:cols tbl)) :slot slot :rowid? true
             :ncols (count cols) :hidden #{}}
        ctx (assoc ctx :scope {:sources [src] :parent nil})
        c (e/cx ctx ast)
        f (if (= mode :check) (e/condff c) (e/condf c))
        n (inc @(:slots ctx))]
    (assoc c :rowf (fn [row] (let [env (object-array n)] (aset env slot row) (f env)))))))

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
  (let [s (inc (long (:index-seq db)))
        t (get-in db [:tables (:table idx)])
        idx (if (and (:db t) (:auto idx)) (assoc idx :name (str (:db t) "." (:name idx))) idx)
        wr-pk (and (:without-rowid t) (:pk-auto idx))
        [db idx] (sch/register db (assoc idx :temp (:temp t) :db (:db t) :no-master wr-pk) (not wr-pk))]
    (-> db (assoc :index-seq s) (assoc-in [:indexes (lc (:name idx))] (assoc idx :seq s)))))

(defn- create-table [db sql st]
  (let [name (:name st) lname (lc name)]
    (cond
      (or (get-in db [:tables lname]) (get-in db [:views lname]))
      (if (:if-not-exists st) db (err (str (if (get-in db [:views lname]) "view " "table ") name " already exists")))
      (and (str/starts-with? lname "sqlite_") (not (:internal st))) (err (str "object name reserved for internal use: " name))
      (:as st)
      (let [ctx (new-ctx db sql)
            sub (e/compile-select ctx (:as st))
            rows (e/run-select-rows sub (object-array (inc @(:slots ctx))))
            cols (mapv (fn [n a] {:name n :type (case a :integer "INT" :real "REAL" :text "TEXT" :numeric "NUM" "")
                                  :aff (or a :blob)})
                       (e/dedup-names (:names sub)) (or (:src-affs sub) (:affs sub)))
            tbl {:name name :cols cols :ipk nil :strict false :checks [] :temp (:temp st) :db (:db st)
                 :sql (sch/create-table-stmt name cols)
                 :rows (into (sorted-map) (map-indexed (fn [i r] [(long (inc i)) (conj (vec r) (long (inc i)))]) rows))}
            [db tbl] (sch/register db tbl true)]
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
                            :default (:e (last (filter #(= :default (:c %)) cons)))
                            :default-text (when-let [de (:e (last (filter #(= :default (:c %)) cons)))]
                                            (when (:ps de) (subs sql (:ps de) (:pe de))))
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
            fks (vec (concat (for [d defs, k (:cons d) :when (= :fk (:c k))] (dissoc k :c))
                             (for [k (:tcons st) :when (= :fk (:c k))] (dissoc k :c))))
            tbl {:name name :cols cols :ipk ipk :strict strict :checks [] :rows (sorted-map)
                 :pk-cols (mapv :i pk-cols) :fks fks
                 :without-rowid (:without-rowid st) :temp (:temp st) :db (:db st) :sql (sch/stmt-sql sql st "TABLE")}
            [db tbl] (sch/register db tbl true)
            db (assoc-in db [:tables lname] tbl)
            ;; compile checks against the table
            tbl (assoc tbl :checks (mapv (fn [ck] (assoc ck :f (:rowf (compile-row-expr db sql tbl (:e ck) :check)))) checks))
            db (assoc-in db [:tables lname] tbl)
            ;; autoindexes: PK (non-ipk) and UNIQUE in declaration order
            uniq-defs (concat
                       (for [[i c] (map-indexed vector cols)
                             :let [items (concat (when (and (:pk c) (not= i ipk)) [[:pk]])
                                                 (map (fn [_] [:u]) (:unique c)))]
                             it items]
                         (with-meta [{:e {:op :col :name (:name c)} :coll nil :desc false}] {:pk (= it [:pk])}))
                       (for [k (:tcons st) :when (or (= :unique (:c k)) (and (= :pk (:c k)) (nil? ipk)))]
                         (with-meta (:cols k) {:pk (= :pk (:c k))})))
            db (reduce (fn [db [n icols]]
                         (let [tbl (get-in db [:tables lname])
                               idx (assoc (build-index db sql tbl (str "sqlite_autoindex_" (sch/display-name name {:db (:db st)}) "_" n) true icols nil true)
                                          :pk-auto (:pk (meta icols)))]
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
    (str "UNIQUE constraint failed: " (str/join ", " (map #(str (sch/display-name (:name tbl) tbl) "." (:name %)) (:cols idx))))
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

(defn- try-fire
  "Runs triggers for a row event; ::ignore when a program raised IGNORE."
  [db trigs tbl old new]
  (try (trg/fire db trigs tbl old new)
       (catch clojure.lang.ExceptionInfo ex (if (trg/ignore? ex) ::ignore (throw ex)))))

(defn- dml-target
  "[tkey tbl] of the table a DML statement targets; a view becomes a temporary table of its rows
  (INSTEAD OF triggers then stand in for the writes)."
  [db sql st event]
  (let [tname (:table st) l (lc tname)]
    (if-let [tbl (get-in db [:tables l])]
      (do (when (:virtual tbl) (err (str "table " tname " may not be modified")))
          [l tbl])
      (if-let [view (get-in db [:views l])]
        (do (when-not (trg/any-for? db l event)
              (err (str "cannot modify " tname " because it is a view")))
            (let [ctx (new-ctx db sql)
                  sub (e/compile-select (assoc ctx :sql (or (:src view) sql)) (:select view))
                  rows (e/run-select-rows sub (object-array (+ 2 @(:slots ctx))))
                  cols (or (:cols view) (:names sub))]
              [(str "\u0000v:" l)
               {:name (:name view) :view true :vkey l
                :cols (mapv (fn [n] {:name n :type nil :aff :blob}) cols)
                :ipk nil :strict false :checks []
                :rows (into (sorted-map) (map-indexed (fn [i r] [(long (inc i)) (conj (vec r) (long (inc i)))]) rows))}]))
        (err (str "no such table: " (when (:schema st) (str (:schema st) ".")) tname))))))

(defn- with-target [db tkey tbl] (if (:view tbl) (assoc-in db [:tables tkey] tbl) db))
(defn- end-target [db tkey tbl] (if (:view tbl) (update db :tables dissoc tkey) db))

(defn- after-update
  "AFTER UPDATE triggers for a row an upsert updated; returns [db changes]."
  [db tbl old new tkey changed-cis changes]
  (let [cols (map #(lc (:name (nth (:cols tbl) %))) changed-cis)
        trigs (trg/matching db (:vkey tbl tkey) :after :update cols)
        r (if (seq trigs) (try-fire db trigs tbl old new) db)]
    (if (= r ::ignore) [db changes] [r (inc changes)])))

(defn- exec-insert [db sql st emit prepared!]
  (let [tname (:table st)
        [tkey tbl] (dml-target db sql st :insert)
        db (with-target db tkey tbl)
        vw (:view tbl)
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
    (let [[db changes]
          (reduce
           (fn [[db changes] r]
        (let [tbl (get-in db [:tables tkey])
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
              bts (trg/matching db (:vkey tbl tkey) :before :insert nil)
              bres (if (seq bts) (try-fire db bts tbl nil (assoc row ncols rid)) db)
              ignored (= bres ::ignore)
              db (if ignored db bres)
              tbl (get-in db [:tables tkey])
              row (when-not ignored (check-notnull db sql tbl row on-conflict))
              has-checks (seq (:checks tbl))
              row (when row (if has-checks (check-checks tbl (check-strict tbl row) on-conflict) row))
              m-up (fn [what] (boolean (some #((:match %) what) up-compiled)))
              first-up (first up-compiled)
              delay-ipk (boolean (and first-up (:target first-up) (not ((:match first-up) :rowid))))
              ipk-hit (when (and row (seq up-compiled) (not delay-ipk) (contains? (:rows tbl) rid) (m-up :rowid)) :rowid)]
          (if (nil? row)
            [db changes]
            (let [[tbl2 res] (if ipk-hit
                               [tbl [:upsert ipk-hit]]
                               (resolve-uniques db tbl rid row nil on-conflict m-up
                                                (fn [] (when-not has-checks (check-strict tbl row)))
                                                delay-ipk))]
              (cond
                (= res :ignore) [db changes]
                (vector? res)
                ;; upsert
                (let [conf (second res)
                      up (first (filter #((:match %) conf) up-compiled))
                      crid (if (= conf :rowid) rid (find-conflict tbl conf row nil))
                      existing (get-in tbl [:rows crid])]
                  (if (= :nothing (:do up))
                    [db changes]
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
                        [db changes]
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
                          (let [db3 (assoc-in db [:tables tkey] tblu)]
                            (after-update db3 tbl existing (assoc nrow ncols nrid) tkey (map first (:sets-c up)) changes)))))))
                :else
                (let [tbl3 (put-row tbl2 rid row)
                      db2 (assoc-in db [:tables tkey] tbl3)]
                  (when ret
                    (aset env (long (:slot ret)) (assoc row ncols rid))
                    (.add ret-rows (mapv #(% env) (:fs ret))))
                  (let [r (if (seq (trg/matching db2 (:vkey tbl tkey) (if vw :instead :after) :insert nil))
                            (try-fire db2 (trg/matching db2 (:vkey tbl tkey) (if vw :instead :after) :insert nil) tbl3 nil (assoc row ncols rid))
                            db2)]
                    (if (= r ::ignore) [db2 changes] [r (if vw changes (inc changes))]))))))))
           [db 0] src-rows)]
      (doseq [r ret-rows] (emit r))
      (assoc (end-target db tkey tbl) :changes changes))))



;; ====================================================================== UPDATE / DELETE

(defn- exec-update [db sql st emit prepared!]
  (let [tname (:table st)
        [tkey tbl] (dml-target db sql st :update)
        vw (:view tbl)
        db (with-target db tkey tbl)
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
        where-pre (when (:where st) (e/where-precheck sctx (:where st)))
        ret (compile-returning ctx src (:returning st))
        on-conflict (or (:or st) :abort)
        env (object-array (+ 2 @(:slots ctx)))
        slot (long (:slot src))
        _ (prepared! (:names ret))
        targets (vec (filter (fn [[rid row]]
                               (aset env slot row)
                               (or (nil? where-f) (let [w (where-f env)] (and (not (nil? w)) (v/bool-value w)))))
                             (if (and where-pre (not (where-pre env))) [] (:rows tbl))))
        ret-rows (java.util.ArrayList.)]
    (let [set-names (mapv (fn [[ci _]] (if (neg? ci) "rowid" (lc (:name (nth (:cols tbl) ci))))) sets)
          [db changes]
          (reduce
           (fn [[db changes] [rid _]]
             (let [tbl (get-in db [:tables tkey])
                   old (get-in tbl [:rows rid])]
               (if (nil? old)
                 [db changes]
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
                       moved (not= rid-new rid)
                       ;; with a new rowid the type check follows the rowid check; otherwise it precedes CHECK
                       early-type (and has-checks (not moved))
                       bts (trg/matching db (:vkey tbl tkey) :before :update set-names)
                       bres (if (seq bts) (try-fire db bts tbl old (assoc row ncols rid-new)) db)
                       ignored (= bres ::ignore)
                       db (if ignored db bres)
                       tbl (get-in db [:tables tkey])
                       row (when-not ignored (check-notnull db sql tbl row on-conflict))
                       row (when row (if has-checks (check-checks tbl (if early-type (check-strict tbl row) row) on-conflict) row))]
                   (if (nil? row)
                     [db changes]
                     (let [[tbl2 res] (resolve-uniques db tbl rid-new row rid on-conflict (constantly false)
                                                       (fn [] (when-not early-type (check-strict tbl row))) false)]
                       (if (= res :ignore)
                         [db changes]
                         (let [tbl3 (put-row (update tbl2 :rows dissoc rid) rid-new row)
                               db2 (assoc-in db [:tables tkey] tbl3)
                               ats (trg/matching db2 (:vkey tbl tkey) (if vw :instead :after) :update set-names)]
                           (when ret
                             (aset env slot (assoc row ncols rid-new))
                             (.add ret-rows (mapv #(% env) (:fs ret))))
                           (let [r (if (seq ats) (try-fire db2 ats tbl3 old (assoc row ncols rid-new)) db2)]
                             (if (= r ::ignore) [db2 changes] [r (if vw changes (inc changes))]))))))))))
           [db 0] targets)]
      (doseq [r ret-rows] (emit r))
      (assoc (end-target db tkey tbl) :changes changes))))

(defn- exec-delete [db sql st emit prepared!]
  (let [tname (:table st)
        [tkey tbl] (dml-target db sql st :delete)
        vw (:view tbl)
        db (with-target db tkey tbl)
        ctx (new-ctx db sql)
        src (table-source ctx tbl (:alias st))
        _ (when (and (:indexed-by st) (not= :none (:indexed-by st)))
            (when-not (get-in db [:indexes (lc (:indexed-by st))])
              (err (str "no such index: " (:indexed-by st)))))
        sctx (assoc ctx :scope {:sources [src] :parent nil})
        where-f (when (:where st) (e/condf (e/cx sctx (:where st))))
        where-pre (when (:where st) (e/where-precheck sctx (:where st)))
        ret (compile-returning ctx src (:returning st))
        env (object-array (+ 2 @(:slots ctx)))
        slot (long (:slot src))
        _ (prepared! (:names ret))
        doomed (vec (filter (fn [[rid row]]
                              (aset env slot row)
                              (or (nil? where-f) (let [w (where-f env)] (and (not (nil? w)) (v/bool-value w)))))
                            (if (and where-pre (not (where-pre env))) [] (:rows tbl))))]
    (when ret
      (doseq [[_ row] doomed]
        (aset env slot row)
        (emit (mapv #(% env) (:fs ret)))))
    (if (and (not vw) (not (trg/any-for? db tkey :delete)))
      (-> db
          (assoc-in [:tables tkey :rows] (apply dissoc (:rows tbl) (map first doomed)))
          (assoc :changes (count doomed)))
      (let [[db changes]
            (reduce
             (fn [[db changes] [rid _]]
               (let [tbl (get-in db [:tables tkey])
                     old (get-in tbl [:rows rid])]
                 (if (nil? old)
                   [db changes]
                   (let [bts (trg/matching db (:vkey tbl tkey) :before :delete nil)
                         bres (if (seq bts) (try-fire db bts tbl old nil) db)]
                     (if (= bres ::ignore)
                       [db changes]
                       (let [db (if (contains? (get-in bres [:tables tkey :rows]) rid)
                                  (update-in bres [:tables tkey :rows] dissoc rid)
                                  bres)
                             ats (trg/matching db (:vkey tbl tkey) (if vw :instead :after) :delete nil)
                             r (if (seq ats) (try-fire db ats (get-in db [:tables tkey]) old nil) db)]
                         (if (= r ::ignore) [db changes] [r (if vw changes (inc changes))])))))))
             [db 0] doomed)]
        (assoc (end-target db tkey tbl) :changes changes)))))

;; ====================================================================== other DDL

(defn- create-trigger [db sql st]
  (let [name (:name st) ln (lc name) tl (lc (:table st))
        tbl (get-in db [:tables tl]) view (get-in db [:views tl])]
    (cond
      (get-in db [:triggers ln]) (if (:if-not-exists st) ::empty (err (str "trigger " name " already exists")))
      (and (nil? tbl) (nil? view)) (err (str "no such table: " (or (:table-schema st) "main") "." (:table st)))
      (and view (not= :instead (:timing st)))
      (err (str "cannot create " (if (= :before (:timing st)) "BEFORE" "AFTER") " trigger on view: " (:table st)))
      (and tbl (= :instead (:timing st))) (err (str "cannot create INSTEAD OF trigger on table: " (:table st)))
      (and tbl (or (:virtual tbl) (str/starts-with? tl "sqlite_"))) (err "cannot create trigger on system table")
      :else
      (let [trig (-> (trg/make-trigger st sql)
                     (assoc :table tl :table-name (:name (or tbl view)) :temp (boolean (or (:temp st) (:temp tbl) (:temp view)))
                            :sql (sch/stmt-sql sql st "TRIGGER")))
            [db trig] (sch/register db trig false)]
        (assoc-in db [:triggers ln] trig)))))

(defn- parse-trigger-text [old ns]
  (let [st (p/next-statement (p/parse-statements ns))]
    (-> (trg/make-trigger st ns)
        (assoc :table (:table old) :table-name (:table-name old) :temp (:temp old) :mseq (:mseq old) :sql ns))))

(defn- create-index [db sql st]
  (let [name (:name st) lname (lc name)
        tbl (get-in db [:tables (lc (:table st))])]
    (cond
      (get-in db [:indexes lname]) (if (:if-not-exists st) db (err (str "index " name " already exists")))
      (get-in db [:tables lname]) (err (str "there is already a table named " name))
      (nil? tbl) (err (str "no such table: main." (:table st)))
      (str/starts-with? lname "sqlite_") (err (str "object name reserved for internal use: " name))
      :else
      (let [idx (assoc (build-index db sql tbl name (:unique st) (:cols st) (:where st) false)
                       :sql (sch/stmt-sql sql (assoc st :st-e (:st-next st)) (if (:unique st) "UNIQUE INDEX" "INDEX")))]
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
               (let [idxs (filter (fn [i] (= (:table i) lname)) (vals (:indexes db)))
                     db (reduce (fn [db o] (sch/free-page db (sch/schema-of o) (:rootpage o))) db (cons t idxs))]
                 (-> db
                     (update :tables dissoc lname)
                     (update :indexes (fn [m] (into {} (remove (fn [[_ i]] (= (:table i) lname)) m))))
                     (update :triggers (fn [m] (into {} (remove (fn [[_ g]] (= (:table g) lname)) m))))))
               (if (:if-exists st) db (err (str "no such table: " (:name st)))))
      :index (if-let [i (get-in db [:indexes lname])]
               (if (:auto i)
                 (err "index associated with UNIQUE or PRIMARY KEY constraint cannot be dropped")
                 (-> db (sch/free-page (sch/schema-of i) (:rootpage i)) (update :indexes dissoc lname)))
               (if (:if-exists st) db (err (str "no such index: " (:name st)))))
      :view (if (get-in db [:views lname])
              (-> db (update :views dissoc lname)
                  (update :triggers (fn [m] (into {} (remove (fn [[_ g]] (= (:table g) lname)) m)))))
              (if (:if-exists st) db (err (str "no such view: " (:name st)))))
      :trigger (if (get-in db [:triggers lname])
                 (update db :triggers dissoc lname)
                 (if (:if-exists st) ::empty (err (str "no such trigger: " (:name st))))))))

(defn- index-stat
  "sqlite_stat1 stat text for index idx of tbl, or nil when the index is empty."
  [tbl idx]
  (let [rows (filter #(or (nil? (:wf idx)) ((:wf idx) %)) (e/table-rows tbl))
        n (count rows)
        k (count (:cols idx))
        colls (mapv :coll (:cols idx))]
    (when (pos? n)
      (let [keys (sort (e/row-comparator colls) (map #(e/index-key tbl idx %) rows))
            eqv (fn [a b c] (or (and (nil? a) (nil? b)) (and (some? a) (some? b) (zero? (v/sql-cmp a b c)))))
            dist (long-array k)]
        (reduce (fn [prev cur]
                  (when prev
                    (let [i (loop [i 0] (if (and (< i k) (eqv (nth prev i) (nth cur i) (nth colls i))) (recur (inc i)) i))]
                      (loop [j i] (when (< j k) (aset dist j (inc (aget dist j))) (recur (inc j))))))
                  cur)
                nil keys)
        (str n " " (str/join " " (for [j (range k)] (let [d (inc (aget dist j))] (quot (+ n d -1) d)))))))))

(defn- exec-analyze [db sql st]
  (let [lname (lc (:name st)) lsch (lc (:schema st))
        target (cond
                 (nil? lname) :all
                 (and (nil? lsch) (#{"main" "temp"} lname)) :all
                 (and (nil? lsch) (#{"sqlite_master" "sqlite_schema"} lname)) [:table lname]
                 (get-in db [:indexes lname]) [:index lname]
                 (get-in db [:tables lname]) [:table lname]
                 :else (err (str "no such table: " (:name st))))
        db (if (get-in db [:tables "sqlite_stat1"])
             db
             (create-table db "CREATE TABLE sqlite_stat1(tbl,idx,stat)"
                           (assoc (p/next-statement (p/parse-statements "CREATE TABLE sqlite_stat1(tbl,idx,stat)")) :internal true)))
        tnames (case target
                 :all (->> (vals (:tables db)) (sort-by :mseq) (map (comp lc :name))
                           (remove #(str/starts-with? % "sqlite_")))
                 (if (= :table (first target)) [(second target)]
                     (let [t (:table (get-in db [:indexes (second target)]))] (if (str/starts-with? t "sqlite_") [] [t]))))
        only-idx (when (and (vector? target) (= :index (first target))) (second target))
        stat-rows (vec (for [tn tnames
                             :let [tbl (get-in db [:tables tn])
                                   idxs (table-indexes db tn)
                                   idxs (if only-idx (filter #(= (lc (:name %)) only-idx) idxs) idxs)
                                   n (count (:rows tbl))]
                             r (if (and (empty? idxs) (nil? only-idx))
                                 (when (pos? n) [[(:name tbl) nil (str n)]])
                                 (for [idx idxs :let [stat (index-stat tbl idx)] :when stat]
                                   [(:name tbl) (if (and (:without-rowid tbl) (:pk-auto idx)) (:name tbl) (:name idx)) stat]))]
                         r))
        st1 (get-in db [:tables "sqlite_stat1"])
        drop? (fn [row] (let [t (lc (nth row 0)) i (some-> (nth row 1) lc)]
                          (if only-idx (= i only-idx) (some #{t} tnames))))
        kept (vec (remove drop? (vals (:rows st1))))
        start (if (seq kept) (inc (long (last (nth kept (dec (count kept))))))  1)
        rows (into (sorted-map) (concat (map (fn [r] [(long (last r)) r]) kept)
                                        (map-indexed (fn [i r] (let [rid (+ start i)] [rid (conj r rid)])) stat-rows)))]
    (assoc-in db [:tables "sqlite_stat1" :rows] rows)))

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

;; ---- ALTER TABLE: the schema is its SQL text; an alteration rewrites the text and rebuilds the table

(declare parse-trigger-text)

(defn- quote-id [s] (str "\"" (str/replace s "\"" "\"\"") "\""))
(defn- quote-if-needed [s] (sch/ident-put s))

(defn- parse-one [sql] (p/next-statement (p/parse-statements sql)))

(defn- rewrite-tokens
  "sql with token i replaced by (f toks i) wherever that returns a string."
  [sql f]
  (let [toks (lx/tokenize sql) sb (StringBuilder.)]
    (loop [i 0 pos 0]
      (let [t (nth toks i)]
        (if (= :eof (:t t))
          (do (.append sb (subs sql pos)) (str sb))
          (if-let [r (f toks i)]
            (do (.append sb (subs sql pos (:s t))) (.append sb ^String r) (recur (inc i) (:e t)))
            (recur (inc i) pos)))))))

(defn- tok-name? [t nm] (and (#{:id :qid} (:t t)) (= (lc (:v t)) (lc nm))))
(defn- prev-kw [toks i] (when (pos? i) (:u (nth toks (dec i)))))

(defn- rename-table-refs [sql old new]
  (rewrite-tokens sql (fn [toks i]
                        (when (and (tok-name? (nth toks i) old)
                                   (#{"FROM" "JOIN" "INTO" "UPDATE" "ON" "TABLE" "REFERENCES"} (prev-kw toks i)))
                          (quote-id new)))))

(defn- mentions-table? [sql tname]
  (some #(tok-name? % tname) (lx/tokenize sql)))

(defn- rename-column-refs [sql old new]
  (let [toks (lx/tokenize sql)
        from (or (first (keep-indexed (fn [i t] (when (and (= :op (:t t)) (= "(" (:v t))) i)) toks)) 0)]
    (rewrite-tokens sql (fn [toks i]
                          (when (and (>= i from) (tok-name? (nth toks i) old))
                            (quote-if-needed new))))))

(defn- rebuild-table
  "Replaces table tname by the table the new CREATE TABLE text describes; rows are mapped by row-fn (row incl. rowid).
  Non-automatic indexes are rebuilt from the text idx-sql-fn gives for them."
  [db tname new-sql row-fn idx-sql-fn]
  (let [old (get-in db [:tables tname])
        idxs (sort-by :mseq (filter #(= (:table %) tname) (vals (:indexes db))))
        db0 (-> db (update :tables dissoc tname)
                (update :indexes (fn [m] (into {} (remove (fn [[_ i]] (= (:table i) tname)) m)))))
        st (assoc (parse-one new-sql) :temp (:temp old) :internal true)
        db1 (create-table db0 new-sql st)
        by-name (into {} (map (fn [i] [(lc (:name i)) i]) idxs))
        restore (fn [db objs-key key o]
                  (let [cur (get-in db [objs-key key])
                        db (sch/free-page db (sch/schema-of cur) (:rootpage cur))]
                    (assoc-in db [objs-key key] (assoc cur :mseq (:mseq o) :rootpage (:rootpage o)))))
        db1 (restore db1 :tables tname old)
        db1 (assoc-in db1 [:tables tname :rows] (into (sorted-map) (map (fn [[rid r]] [rid (row-fn r)]) (:rows old))))
        db1 (reduce (fn [db [k i]]
                      (if-let [o (by-name (lc (:name i)))] (restore db :indexes k o) db))
                    db1 (filter (fn [[_ i]] (= (:table i) tname)) (:indexes db1)))]
    (reduce (fn [db i]
              (if (:auto i) db
                  (let [isql (idx-sql-fn i)
                        ist (assoc (parse-one isql) :temp (:temp old) :internal true)
                        db' (create-index db isql ist)]
                    (restore db' :indexes (lc (:name i)) i))))
            db1 idxs)))

(defn- reschema-objects
  "Applies f (sql -> sql) to the text of views and triggers (re-parsing them)."
  [db f]
  (-> db
      (update :views (fn [m] (into {} (map (fn [[k v]]
                                            (let [ns (when (:sql v) (f (:sql v)))]
                                              [k (if (or (nil? ns) (= ns (:sql v))) v
                                                     (let [st (parse-one ns)]
                                                       (assoc v :sql ns :src ns :select (:select st) :cols (:cols st))))]))
                                          m))))
      (update :triggers (fn [m] (into {} (map (fn [[k t]]
                                               (let [ns (when (:sql t) (f (:sql t)))]
                                                 [k (if (or (nil? ns) (= ns (:sql t))) t
                                                        (parse-trigger-text t ns))]))
                                             m))))))

(defn- find-col [tbl n]
  (let [i (.indexOf ^java.util.List (mapv (comp lc :name) (:cols tbl)) (lc n))]
    (when (>= i 0) i)))

(def ^:private master-aliases #{"sqlite_master" "sqlite_schema" "sqlite_temp_master" "sqlite_temp_schema"})

(defn- exec-alter [db sql st ^StringBuilder out]
  (let [tname (lc (:table st))
        tbl (or (get-in db [:tables tname])
                (when (master-aliases tname) (err (str "table " (:table st) " may not be altered")))
                (err (str "no such table: " (when (:schema st) (str (:schema st) ".")) (:table st))))
        tsql (:sql tbl)]
    (when (str/starts-with? tname "sqlite_")
      (err (str "table " (:name tbl) " may not be altered")))
    (case (:action st)
      :rename
      (let [nn (:to st) nk (lc nn) old (:name tbl)]
        (when (or (get-in db [:tables nk]) (get-in db [:indexes nk]) (get-in db [:views nk]))
          (err (str "there is already another table or index with this name: " nn)))
        (when (str/starts-with? nk "sqlite_") (err (str "object name reserved for internal use: " nn)))
        (let [tsql2 (let [s1 (rewrite-tokens tsql (fn [toks i] (when (= i 2) (quote-id nn))))]
                      (rename-table-refs s1 old nn))
              db (-> db
                     (update :tables dissoc tname)
                     (assoc-in [:tables nk] (assoc tbl :name nn :sql tsql2))
                     (update :indexes (fn [m] (into {} (map (fn [[k i]]
                                                              [k (if (= (:table i) tname)
                                                                   (assoc i :table nk
                                                                          :sql (when (:sql i) (rename-table-refs (:sql i) old nn)))
                                                                   i)]) m))))
                     ;; foreign keys of other tables
                     (update :tables (fn [m] (into {} (map (fn [[k t]]
                                                             [k (if (and (not= k nk) (:sql t) (mentions-table? (:sql t) old))
                                                                  (assoc t :sql (rename-table-refs (:sql t) old nn)) t)]) m))))
                     (update :triggers (fn [m] (into {} (map (fn [[k t]]
                                                               [k (if (= (:table t) tname) (assoc t :table nk :table-name nn) t)]) m)))))
              db (reschema-objects db (fn [s] (if (and s (mentions-table? s old)) (rename-table-refs s old nn) s)))
              db (if-let [t (get-in db [:tables "sqlite_sequence"])]
                   (assoc-in db [:tables "sqlite_sequence" :rows]
                             (into (sorted-map) (map (fn [[rid r]] [rid (if (= (lc (first r)) nk) r (if (= (lc (first r)) tname) (assoc r 0 nn) r))]) (:rows t))))
                   db)]
          db))
      :add
      (let [d (:col st)
            nm (:name d)
            _ (when (find-col tbl nm) (err (str "duplicate column name: " nm)))
            cons (:cons d)
            _ (when (some #(= :pk (:c %)) cons) (err "Cannot add a PRIMARY KEY column"))
            _ (when (some #(= :unique (:c %)) cons) (err "Cannot add a UNIQUE column"))
            dflt (:e (first (filter #(= :default (:c %)) cons)))
            notnull (some #(= :notnull (:c %)) cons)
            has-check (some #(= :check (:c %)) cons)
            coldef (subs sql (:s d) (:e d))
            _ (when (and dflt (or (not (e/const-term? dflt)) (some #(and (map? %) (= :fn (:op %))) (tree-seq coll? seq dflt))))
                (.append out "# raise(ABORT,'Cannot add a column with non-constant default')\n")
                (err "Cannot add a column with non-constant default"))
            dv (when dflt ((:f (e/cx (new-ctx db sql) dflt)) (object-array 1)))
            _ (when (and notnull (or (nil? dflt) (nil? dv)))
                (.append out "# raise(ABORT,'Cannot add a NOT NULL column with default value NULL')\n")
                (when (seq (:rows tbl)) (err "Cannot add a NOT NULL column with default value NULL")))
            _ (when (and has-check (not notnull))
                (.append out "# CASE WHEN quick_check GLOB 'CHECK*' THEN raise(ABORT,'CHECK constraint failed') WHEN quick_check GLOB 'non-* value in*' THEN raise(ABORT,'type mismatch on DEFAULT') ELSE raise(ABORT,'NOT NULL constraint failed') END\n"))
            ;; the new column goes before the closing parenthesis of the column list
            close (let [toks (lx/tokenize tsql)
                        depth (volatile! 0) last-close (volatile! nil)]
                    (doseq [t toks]
                      (when (= :op (:t t))
                        (cond (= "(" (:v t)) (vswap! depth inc)
                              (= ")" (:v t)) (do (vswap! depth dec) (when (zero? @depth) (vreset! last-close (:s t)))))))
                    (or @last-close (str/last-index-of tsql ")")))
            new-sql (str (subs tsql 0 close) ", " coldef (subs tsql close))
            aff (v/type-affinity (:type d))
            dv (when-not (nil? dv) (v/apply-affinity dv aff))]
        (rebuild-table db tname new-sql
                       (fn [r] (conj (pop r) dv (peek r)))
                       (fn [i] (:sql i))))
      :rename-col
      (let [i (find-col tbl (:old st))
            _ (when-not i (err (str "no such column: \"" (:old st) "\"")))
            _ (when (and (find-col tbl (:to st)) (not= (lc (:old st)) (lc (:to st)))) (err (str "duplicate column name: " (:to st))))
            old (:old st) nn (:to st)
            new-sql (rename-column-refs tsql old nn)
            db2 (rebuild-table db tname new-sql identity
                               (fn [ix] (rename-column-refs (:sql ix) old nn)))
            db2 (reschema-objects db2 (fn [s] (if (and s (mentions-table? s (:name tbl))) (rename-column-refs s old nn) s)))]
        db2)
      :drop-col
      (let [i (find-col tbl (:col st))
            _ (when-not i (err (str "no such column: \"" (:col st) "\"")))
            c (nth (:cols tbl) i)
            _ (when (= 1 (count (:cols tbl))) (err (str "cannot drop column \"" (:name c) "\": no other columns exist")))
            _ (when (:pk c) (err (str "cannot drop PRIMARY KEY column: \"" (:name c) "\"")))
            _ (when (seq (:unique c)) (err (str "cannot drop UNIQUE column: \"" (:name c) "\"")))
            tst (parse-one tsql)
            cd (nth (:cols tst) i)
            toks (lx/tokenize tsql)
            before (last (filter #(< (:s %) (:s cd)) toks))
            after (first (filter #(>= (:s %) (:e cd)) toks))
            [from to] (if (and after (= "," (:v after)))
                        (let [nxt (first (filter #(> (:s %) (:s after)) toks))] [(:s cd) (:s nxt)])
                        [(:s before) (:e cd)])
            new-sql (str (subs tsql 0 from) (subs tsql to))]
        (doseq [ix (vals (:indexes db))
                :when (and (= (:table ix) tname) (not (:auto ix)))]
          (when (or (some #(= (:col %) i) (:cols ix))
                    (and (:where-ast ix) (some #(and (map? %) (= :col (:op %)) (= (lc (:name %)) (lc (:name c)))) (tree-seq coll? seq (:where-ast ix)))))
            (err (str "error in index " (:name ix) " after drop column: no such column: " (:name c)))))
        (rebuild-table db tname new-sql
                       (fn [r] (let [rid (peek r)] (conj (into (subvec r 0 i) (subvec r (inc i) (dec (count r)))) rid)))
                       (fn [ix] (:sql ix)))))))

;; ====================================================================== driver

(defn- readonly? [st]
  (or (#{:select :commit :rollback :savepoint :release} (:kind st))
      (and (= :begin (:kind st)) (not (#{:immediate :exclusive} (:mode st))))
      (and (= :pragma (:kind st)) (not (:writes st)))))

;; ====================================================================== transactions
;; :tx in the db value is nil (autocommit) or {:snap db-before-BEGIN :sp [[lc-name snapshot] ...] :by-sp bool}.
;; Snapshots are db values without :tx.

(defn- snapshot [db] (dissoc db :tx))

(defn- restore [db snap]
  (assoc snap :tx (:tx db) :changes (:changes db)))

(defn- exec-tx [db st]
  (let [tx (:tx db)]
    (case (:kind st)
      :begin (if tx (err "cannot start a transaction within a transaction")
                 (assoc db :tx {:snap (snapshot db) :sp [] :by-sp false}))
      :commit (if tx (dissoc db :tx) (err "cannot commit - no transaction is active"))
      :rollback
      (cond
        (nil? tx) (err (if (:to st) (str "no such savepoint: " (:to st)) "cannot rollback - no transaction is active"))
        (:to st)
        (let [n (lc (:to st))
              sp (:sp tx)
              i (last (keep-indexed (fn [i [nm]] (when (= nm n) i)) sp))]
          (if (nil? i) (err (str "no such savepoint: " (:to st)))
              (let [[_ snap] (nth sp i)]
                (assoc (restore db snap) :tx (assoc tx :sp (subvec sp 0 (inc i)))))))
        :else (dissoc (restore db (:snap tx)) :tx))
      :savepoint (let [tx (or tx {:snap (snapshot db) :sp [] :by-sp true})]
                   (assoc db :tx (update tx :sp conj [(lc (:name st)) (snapshot db)])))
      :release (let [n (lc (:name st))
                     sp (:sp tx)
                     i (when tx (last (keep-indexed (fn [i [nm]] (when (= nm n) i)) sp)))]
                 (cond (nil? i) (err (str "no such savepoint: " (:name st)))
                       (and (zero? i) (:by-sp tx)) (dissoc db :tx)
                       :else (assoc db :tx (assoc tx :sp (subvec sp 0 i))))))))

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
  (when (#{:create-table :create-index :create-view :create-trigger} (:kind st)) (sch/check-schema db (:schema st)))
  (when (and (#{:insert :update :delete} (:kind st)) (contains? sch/master-names (lc (:table st))))
    (err (str "table " (:table st) " may not be modified")))
  (when (and (= :drop (:kind st)) (= :table (:what st)) (contains? sch/master-names (lc (:name st))))
    (err (str "table " (:name st) " may not be dropped")))
  (when (and (= :create-index (:kind st)) (contains? sch/master-names (lc (:table st))))
    (err (str "table " (:table st) " may not be indexed")))
  (when (= :create-table (:kind st))
    (doseq [d (:cols st)
            k (:cons d) :when (= :default (:c k))]
      (when-not (sch/constant-default? (:e k))
        (err (str "default value of column [" (:name d) "] is not constant")))))
  (let [st (sch/qualify-stmt db st)]
  (case (:kind st)
    :attach
    (let [ctx (new-ctx db sql)
          ev (fn [e] (if (and (= :col (:op e)) (nil? (:table e))) (:name e) ((:f (e/cx ctx e)) (object-array 4))))
          file (ev (:file st))
          nm (some-> (ev (:as st)) str)
          lnm (lc nm)]
      (when (or (#{"main" "temp"} lnm) (some #{lnm} (sch/attached-names db)))
        (err (str "database " nm " is already in use")))
      (when (>= (count (:attached db)) 10) (err "too many attached databases - max 10"))
      (update db :attached (fnil conj []) [lnm {:file (if (= file ":memory:") "" (str file)) :name nm}]))
    :detach
    (let [e (:name st)
          nm (if (and (= :col (:op e)) (nil? (:table e))) (:name e) ((:f (e/cx (new-ctx db sql) e)) (object-array 4)))
          lnm (lc (str nm))]
      (when-not (some #{lnm} (sch/attached-names db))
        (err (if (#{"main" "temp"} lnm) (str "cannot detach database " nm) (str "no such database: " nm))))
      (let [keep? (fn [[_ o]] (not= lnm (:db o)))]
        (-> db
            (update :attached (fn [a] (vec (remove #(= lnm (first %)) a))))
            (update :tables #(into {} (filter keep? %)))
            (update :indexes #(into {} (filter keep? %)))
            (update :views #(into {} (filter keep? %)))
            (update :triggers #(into {} (filter keep? %)))
            (update :pages dissoc lnm))))
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
    :pragma
    (let [r (pragma/run db st)]
      (when (:names r)
        (emit-line out (str "# " (str/join "|" (:names r))))
        (doseq [row (:rows r)] (write-row out row)))
      (when (and (nil? (:names r)) (:writes r))
        (emit-line out (str "changes: " (:changes db))))
      (:db r))
    (:insert :update :delete)
    (let [rows (java.util.ArrayList.)
          f (case (:kind st) :insert exec-insert :update exec-update :delete exec-delete)
          db2 (binding [e/*with* (:with st)]
               (f db sql st (fn [r] (.add rows r))
                 (fn [names]
                   (when names (emit-line out (str "# " (str/join "|" names))))
                   (vreset! *prepared* true))))]
      (if (seq (:returning st))
        (doseq [r rows] (write-row out r))
        (emit-line out (str "changes: " (:changes db2))))
      db2)
    (let [db2 (case (:kind st)
                :create-table (create-table db sql st)
                :create-index (create-index db sql st)
                :create-trigger (create-trigger db sql st)
                :create-view (let [ln (lc (:name st))]
                               (cond (or (get-in db [:views ln]) (get-in db [:tables ln]))
                                     (if (:if-not-exists st) db (err (str (if (get-in db [:views ln]) "view " "table ") (:name st) " already exists")))
                                     :else (do (e/compile-select (new-ctx db sql) (:select st))
                                               (let [[db v] (sch/register db {:name (:name st) :cols (:cols st) :select (:select st) :src sql
                                                                              :temp (:temp st) :db (:db st) :sql (sch/stmt-sql sql st "VIEW")} false)]
                                                 (assoc-in db [:views ln] v)))))
                :drop (exec-drop db st)
                :reindex (exec-reindex db st)
                :alter (exec-alter db sql st out)
                :analyze (exec-analyze db sql st)
                (:begin :commit :rollback :savepoint :release) (exec-tx db st)
                (:noop :pragma) db
                :vacuum (do (when-let [sc (lc (:schema st))]
                              (when-not (or (#{"main" "temp"} sc) (some #{sc} (sch/attached-names db)))
                                (err (str "unknown database " (:schema st)))))
                            db))]
      (cond
        (= db2 ::empty) db
        (and (#{:create-table :create-index :create-view :create-trigger :drop :alter} (:kind st)) (not (identical? db db2)))
        (do (when-not (and (= :alter (:kind st)) (pos? (.length out)))
              (emit-line out (str "changes: " (:changes db2))))
            (update db2 :schema-version (fnil inc 0)))
        :else (do (when-not (or (readonly? st) (and (= :alter (:kind st)) (pos? (.length out))))
                    (emit-line out (str "changes: " (:changes db2))))
                  db2))))))

(reset! trg/exec-fn exec-statement)

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

(ns sqlclj.exec
  "Statement execution: DDL, DML with constraints, and the per-line driver output."
  (:require [sqlclj.jsontvf]
            [clojure.string :as str]
            [sqlclj.values :as v]
            [sqlclj.lexer :as lx]
            [sqlclj.parser :as p]
            [sqlclj.engine :as e :refer [err lc]]
            [sqlclj.schema :as sch]
            [sqlclj.trigger :as trg]
            [sqlclj.fk :as fk]
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
             :affs (mapv :aff (:cols tbl)) :colls (mapv :coll (:cols tbl)) :slot slot :rowid? (not (:without-rowid tbl))
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
                                         (and (not (:without-rowid tbl)) (#{"rowid" "oid" "_rowid_"} (lc (:name ast)))) ncols
                                         :else (err (str "no such column: " (:name ast))))))
                            c (when-not ci (compile-row-expr db sql tbl ast))]
                        {:col ci :ast ast :kf (:rowf c) :name (if ci (if (< ci ncols) (:name (nth (:cols tbl) ci)) "rowid") nil)
                         :coll-text (:coll ic)
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
       (sort-by (fn [i] [(if (and (:pk-auto i) (:without-rowid (get-in db [:tables (lc tname)]))) 0 1) (- (long (:seq i)))]))))

(defn- add-index [db idx]
  (let [s (inc (long (:index-seq db)))
        t (get-in db [:tables (:table idx)])
        idx (if (and (:db t) (:auto idx)) (assoc idx :name (str (:db t) "." (:name idx))) idx)
        wr-pk (and (:without-rowid t) (:pk-auto idx))
        [db idx] (sch/register db (assoc idx :temp (:temp t) :db (:db t) :no-master wr-pk) (not wr-pk))]
    (-> db (assoc :index-seq s) (assoc-in [:indexes (lc (:name idx))] (assoc idx :seq s)))))

(def ^:private nondeterministic-fns #{"random" "randomblob" "changes" "last_insert_rowid" "total_changes"
                                       "current_time" "current_date" "current_timestamp"})

(defn- check-gen-expr
  ([ast] (check-gen-expr ast "generated columns"))
  ([ast what]
   (doseq [x (tree-seq coll? seq ast) :when (map? x)]
     (case (:op x)
       :fn (when (nondeterministic-fns (lc (:name x)))
             (err (str "non-deterministic functions prohibited in " what)))
       (:subq :exists) (err (str "subqueries prohibited in " what))
       nil))))

(defn- check-gen-loops [cols]
  (let [names (mapv (comp lc :name) cols)
        refs (fn [c] (set (for [x (tree-seq coll? seq (:e (:gen c))) :when (and (map? x) (= :col (:op x)) (nil? (:table x)))] (lc (:name x)))))
        state (volatile! {})]
    (letfn [(visit [i]
              (let [st (get @state i)]
                (cond (= st :done) nil
                      (= st :busy) true
                      :else (do (vswap! state assoc i :busy)
                                (let [c (nth cols i)
                                      loop? (when (:gen c)
                                              (some (fn [n] (let [j (.indexOf ^java.util.List names n)]
                                                              (when (and (>= j 0) (visit j))
                                                                (err (str "generated column loop on \"" (:name c) "\"")))))
                                                    (refs c)))]
                                  (vswap! state assoc i :done)
                                  nil)))))]
      (dotimes [i (count cols)] (visit i)))))

(defn- fill-generated
  "Computes the generated columns of row (rowid last) in column order."
  [tbl row]
  (if (:has-gen tbl)
    (let [pass (fn [row]
                 (reduce (fn [row ci]
                           (let [c (nth (:cols tbl) ci)]
                             (if-let [f (:f (:gen c))]
                               (assoc row ci (v/apply-affinity (f row) (:aff c)))
                               row)))
                         row (range (count (:cols tbl)))))
          n (count (filter :gen (:cols tbl)))]
      ;; a generated column may depend on a later one: repeat until every dependency has settled
      (nth (iterate pass row) (if (> n 1) n 1)))
    row))

(defn- ensure-sequence-table [db st]
  (if (get-in db [:tables "sqlite_sequence"])
    db
    (let [tbl {:name "sqlite_sequence" :cols [{:name "name" :type nil :aff :blob} {:name "seq" :type nil :aff :blob}]
               :ipk nil :strict false :checks [] :rows (sorted-map) :pk-cols [] :fks []
               :temp (:temp st) :db (:db st) :sql "CREATE TABLE sqlite_sequence(name,seq)"}
          [db tbl] (sch/register db tbl true)]
      (assoc-in db [:tables "sqlite_sequence"] tbl))))

(defn- sequence-value [db tbl]
  (let [sq (get-in db [:tables "sqlite_sequence"])]
    (or (some (fn [[_ r]] (when (= (first r) (:name tbl)) (let [x (second r)] (if (instance? Long x) x (long (or (v/apply-affinity x :integer) 0)))))) (:rows sq)) 0)))

(defn- bump-sequence [db tbl rid]
  (let [sq (get-in db [:tables "sqlite_sequence"])]
    (if-not sq db
      (let [hit (some (fn [[k r]] (when (= (first r) (:name tbl)) k)) (:rows sq))]
        (cond
          hit (let [cur (second (get (:rows sq) hit))]
                (if (and (instance? Long cur) (>= (long cur) (long rid))) db
                    (assoc-in db [:tables "sqlite_sequence" :rows hit] [(:name tbl) rid hit])))
          :else (let [k (if (empty? (:rows sq)) 1 (inc (long (key (first (rseq (:rows sq)))))))]
                  (assoc-in db [:tables "sqlite_sequence" :rows k] [(:name tbl) rid k])))))))

(defn- create-table [db sql st]
  (let [st (if (= "temp" (lc (:schema st))) (assoc st :temp true) st)
        lname0 (lc (:name st))
        ex (get-in db [:tables lname0])
        [db st] (cond
                  (and (:temp st) ex (not (:temp ex)) (nil? (:db ex)) (not (:db st)))
                  [(sch/move-main-aside db lname0) st]
                  (and (not (:temp st)) (nil? (:db st)) ex (:temp ex))
                  [(assoc db :shadow true) (assoc st :name (str "main." (:name st)) :db "main")]
                  :else [db st])
        name (:name st) lname (lc name)]
    (cond
      (or (get-in db [:tables lname]) (get-in db [:views lname]))
      (if (:if-not-exists st) db (err (str (if (get-in db [:views lname]) "view " "table ") (or (:name-raw st) name) " already exists")))
      (and (str/starts-with? lname "sqlite_") (not (:internal st))) (err (str "object name reserved for internal use: " name))
      (get-in db [:indexes lname]) (err (str "there is already an index named " name))
      (:as st)
      (let [ctx (new-ctx db sql)
            sub (e/compile-select ctx (:as st))
            rows (e/run-select-rows sub (object-array (inc @(:slots ctx))))
            cols (mapv (fn [n a] {:name n :type (case a :integer "INT" :real "REAL" :text "TEXT" :numeric "NUM" "")
                                  :aff (or a :blob)})
                       (e/dedup-names (:names sub)) (or (:src-affs sub) (:affs sub)))
            tbl {:name name :cols cols :ipk nil :strict false :checks [] :temp (:temp st) :db (:db st)
                 :sql (sch/create-table-stmt (sch/display-name name {:db (:db st)}) cols)
                 :rows (into (sorted-map) (map-indexed (fn [i r] [(long (inc i)) (conj (vec (v/strip-row r)) (long (inc i)))]) rows))}
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
                            :notnull-conflict (:conflict (first (filter #(= :notnull (:c %)) cons)))
                            :default (:e (last (filter #(= :default (:c %)) cons)))
                            :default-text (when-let [de (:e (last (filter #(= :default (:c %)) cons)))]
                                            (when (:ps de) (subs sql (:ps de) (:pe de))))
                            :coll (when-let [c (:coll (last (filter #(= :collate (:c %)) cons)))] (e/check-coll c))
                            :pk (first (filter #(= :pk (:c %)) cons))
                            :unique (filter #(= :unique (:c %)) cons)
                            :gen (when-let [g (last (filter #(= :generated (:c %)) cons))]
                                   {:e (:e g) :stored (:stored g)})
                            :hidden-kind (when-let [g (last (filter #(= :generated (:c %)) cons))]
                                           (if (:stored g) 3 2))}))
                       defs)
            _ (doseq [c cols :when (:gen c)]
                (when (:default c) (err "cannot use DEFAULT on a generated column"))
                (when (:pk c) (err "generated columns cannot be part of the PRIMARY KEY")))
            _ (when (and (seq cols) (every? :gen cols)) (err "must have at least one non-generated column"))
            _ (doseq [c cols :when (:gen c)]
                (check-gen-expr (:e (:gen c))))
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
                                             (when (:gen (nth cols (col-idx (:name (:e ic)))))
                                               (err "generated columns cannot be part of the PRIMARY KEY"))
                                             {:i (col-idx (:name (:e ic))) :desc (:desc ic) :coll (:coll ic)})
                                           (:cols (first tpks)))
                          :else nil)
            ipk (when (and (= 1 (count pk-cols)) (not (:without-rowid st))
                           (let [c (nth cols (:i (first pk-cols)))]
                             (and (:type c) (= "INTEGER" (str/upper-case (:type c))))
                             )
                           (not (and (seq col-pks) (:desc (first pk-cols)))))
                  (:i (first pk-cols)))
            cols (if ipk (assoc-in cols [ipk :aff] :integer) cols)
            cols (if (:without-rowid st) (reduce (fn [cs pc] (assoc-in cs [(:i pc) :notnull] true)) cols pk-cols) cols)
            checks (vec (concat
                         (for [[c d] (map vector cols defs), k (:cons d) :when (= :check (:c k))]
                           {:e (:e k) :name (:name k) :text (subs sql (:ps (:e k)) (:pe (:e k)))})
                         (for [k (:tcons st) :when (= :check (:c k))]
                           {:e (:e k) :name (:name k) :text (subs sql (:ps (:e k)) (:pe (:e k)))})))
            fks (vec (concat (for [d defs, k (:cons d) :when (= :fk (:c k))] (dissoc k :c))
                             (for [k (:tcons st) :when (= :fk (:c k))] (dissoc k :c))))
            _ (doseq [k fks]
                (when (and (:to k) (not= (count (:to k)) (count (:from k))))
                  (err "number of columns in foreign key does not match the number of columns in the referenced table"))
                (doseq [f (:from k)]
                  (when (neg? (.indexOf ^java.util.List lnames (lc f)))
                    (err (str "unknown column \"" f "\" in foreign key definition")))))
            autoinc (boolean (some (fn [d] (some #(and (= :pk (:c %)) (:auto %)) (:cons d))) defs))
            _ (when (and autoinc (:without-rowid st)) (err "AUTOINCREMENT not allowed on WITHOUT ROWID tables"))
            _ (when (and autoinc (not ipk)) (err "AUTOINCREMENT is only allowed on an INTEGER PRIMARY KEY"))
            _ (when (and (:without-rowid st) (empty? pk-cols)) (err (str "PRIMARY KEY missing on table " name)))
            tbl {:name name :cols cols :ipk ipk :strict strict :checks [] :rows (sorted-map)
                 :pk-cols (mapv :i pk-cols) :pk-desc (mapv (comp boolean :desc) pk-cols) :fks fks :autoinc autoinc
                 :pk-conflict (or (when ipk (:conflict (:pk (nth cols ipk)))) (:conflict (first tpks)))
                 :without-rowid (:without-rowid st) :temp (:temp st) :db (:db st) :sql (sch/stmt-sql sql st "TABLE")}
            [db tbl] (sch/register db tbl true)
            db (assoc-in db [:tables lname] tbl)
            db (if autoinc (ensure-sequence-table db st) db)
            ;; compile checks against the table
            tbl (assoc tbl :checks (mapv (fn [ck] (assoc ck :f (:rowf (compile-row-expr db sql tbl (:e ck) :check)))) checks))
            tbl (if (some :gen cols)
                  (do (check-gen-loops cols)
                      (assoc tbl :has-gen true
                             :cols (mapv (fn [c] (if (:gen c) (assoc-in c [:gen :f] (:rowf (compile-row-expr db sql tbl (:e (:gen c))))) c)) cols)))
                  tbl)
            db (assoc-in db [:tables lname] tbl)
            ;; autoindexes: PK (non-ipk) and UNIQUE in declaration order
            uniq-defs (concat
                       (for [[i c] (map-indexed vector cols)
                             :let [items (concat (when (and (:pk c) (not= i ipk)) [[:pk (:conflict (:pk c))]])
                                                 (map (fn [u] [:u (:conflict u)]) (:unique c)))]
                             it items]
                         (with-meta [{:e {:op :col :name (:name c)} :coll nil :desc false}] {:pk (= (first it) :pk) :conflict (second it)}))
                       (for [k (:tcons st) :when (or (= :unique (:c k)) (and (= :pk (:c k)) (nil? ipk)))]
                         (with-meta (:cols k) {:pk (= :pk (:c k)) :conflict (:conflict k)})))
            db (reduce (fn [db [n icols]]
                         (let [tbl (get-in db [:tables lname])
                               idx (assoc (build-index db sql tbl (str "sqlite_autoindex_" (sch/display-name name {:db (:db st)}) "_" n) true icols nil true)
                                          :pk-auto (:pk (meta icols)) :conflict (:conflict (meta icols)))]
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

;; The keys of a unique index as a sorted map key -> rowid, built once per table value and index and carried
;; over the plain inserts that follow (carry-unique!), so that a run of inserts does not scan the table each time.
(defonce ^:private unique-cache (atom []))

(defn- remember-unique! [tbl idx m]
  (swap! unique-cache (fn [c] (conj (if (>= (count c) 8) (subvec c 1) c) [tbl idx m]))))

(defn- unique-map [tbl idx]
  (let [hit (some (fn [[t i m]] (when (and (identical? t tbl) (identical? i idx)) m)) @unique-cache)]
    (or hit
        (let [colls (mapv :coll (:cols idx))
              cmp (reify java.util.Comparator
                    (compare [_ a b]
                      (loop [i 0]
                        (if (>= i (count colls)) 0
                            (let [c (v/sql-cmp (nth a i) (nth b i) (nth colls i))]
                              (if (zero? c) (recur (inc i)) c))))))
              m (reduce (fn [m [rid r]]
                          (if (or (nil? (:wf idx)) ((:wf idx) r))
                            (let [k (e/index-key tbl idx r)]
                              (if (key-has-null? k) m (assoc m k rid)))
                            m))
                        (sorted-map-by cmp) (:rows tbl))]
          (remember-unique! tbl idx m)
          m))))

(defn- carry-unique!
  "After a plain insert of the stored row (rowid rid) into old, giving new: the unique maps of old move to new."
  [old new rid row]
  (doseq [[t idx m] (filter #(identical? (first %) old) @unique-cache)]
    (if (or (nil? (:wf idx)) ((:wf idx) row))
      (let [k (e/index-key new idx row)]
        (cond (key-has-null? k) (remember-unique! new idx m)
              (contains? m k) nil
              :else (remember-unique! new idx (assoc m k rid))))
      (remember-unique! new idx m))))

(defn- find-conflict
  "Returns rowid of an existing row conflicting with row r under unique index idx, excluding rowid self."
  [tbl idx row self]
  (when (or (nil? (:wf idx)) ((:wf idx) row))
    (let [k (e/index-key tbl idx row)]
      (when-not (key-has-null? k)
        (let [rid (get (unique-map tbl idx) k)]
          (when (and rid (not= rid self)) rid))))))

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

(defn- fail-with
  "Raises a constraint error under conflict action act (FAIL / ROLLBACK carry their semantics in ex-data)."
  [act msg]
  (if (#{:fail :rollback} act)
    (throw (ex-info msg {:sql-error msg :conflict act}))
    (err msg)))

(defn- check-notnull
  "NOT NULL constraints. Returns the row (possibly with REPLACE defaults) or nil for IGNORE."
  [db sql tbl row on-conflict]
  (reduce (fn [row ci]
            (let [c (nth (:cols tbl) ci)]
              (if (and (:notnull c) (nil? (nth row ci)) (not= ci (:ipk tbl)))
                (let [act (or on-conflict (:notnull-conflict c) :abort)
                      msg (str "NOT NULL constraint failed: " (:name tbl) "." (:name c))]
                  (case act
                    :ignore (reduced nil)
                    :replace (let [d (when (:default c) (v/apply-affinity (default-value db sql tbl ci) (:aff c)))]
                               (if (nil? d)
                                 (err msg)
                                 (assoc row ci d)))
                    (fail-with act msg)))
                row)))
          row (range (count (:cols tbl)))))

(defn- check-strict [tbl row]
  (when (:strict tbl)
    (doseq [ci (range (count (:cols tbl)))]
      (when (not= ci (:ipk tbl)) (strict-check tbl ci (nth row ci)))))
  row)

(def ^:private ignore-checks
  "PRAGMA ignore_check_constraints of the running statement (set in run-line)."
  (volatile! false))

(defn- check-checks
  "CHECK constraints. Returns row, or nil for IGNORE."
  [tbl row on-conflict]
  (let [bad (when-not @ignore-checks
            (some (fn [ck] (let [x ((:f ck) row)] (when (and (not (nil? x)) (not (v/bool-value x))) ck)))
                  (:checks tbl)))]
    (if bad
      (if (= on-conflict :ignore)
        nil
        (fail-with on-conflict (str "CHECK constraint failed: " (or (:name bad) (:text bad)))))
      row)))

(defn- check-row
  "NOT NULL, STRICT types, CHECK. Returns [row action] where action is :ok or :ignore."
  [db sql tbl row on-conflict]
  (let [r (check-notnull db sql tbl row on-conflict)
        r (when r (check-strict tbl r))
        r (when r (check-checks tbl r on-conflict))]
    (if r [r :ok] [nil :ignore])))

(defn- put-row [tbl rid row]
  (assoc-in tbl [:rows rid] (assoc (v/strip-row row) (count (:cols tbl)) rid)))

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
     (letfn [(handle [tbl conf msg what cact]
               (let [act (or on-conflict cact :abort)]
                 (cond
                   (upsert-target? what) [:upsert what]
                   (= act :ignore) :ignore
                   (= act :replace) (update tbl :rows dissoc conf)
                   :else (fail-with act msg))))
             (rowid-step [tbl]
               (if (and (not= rid self) (contains? (:rows tbl) rid))
                 (handle tbl rid (str "UNIQUE constraint failed: " (:name tbl) "."
                                      (if (:ipk tbl) (:name (nth (:cols tbl) (:ipk tbl))) "rowid"))
                         :rowid (when (:ipk tbl) (:pk-conflict tbl)))
                 tbl))
             (index-steps [tbl]
               (loop [tbl tbl idxs (sort-by #(if (upsert-target? %) 0 1) idxs)]
                 (if (empty? idxs)
                   tbl
                   (let [idx (first idxs)
                         conf (find-conflict tbl idx row self)]
                     (if conf
                       (let [r (handle tbl conf (unique-msg* tbl idx) idx (:conflict idx))]
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
     :affs (mapv :aff (:cols tbl)) :colls (mapv :coll (:cols tbl)) :slot slot :rowid? (not (:without-rowid tbl)) :ipk (:ipk tbl)
     :ncols (count cols) :hidden #{}}))

(declare new-ctx)
(defn- compile-returning [ctx src items]
  (when (seq items)
    (when (some #(and (:star %) (:table %)) items)
      (err "RETURNING may not use \"TABLE.*\" wildcards"))
    (let [src (assoc src :alias (lc (:tname src)))
          ctx (assoc ctx :scope {:sources [src] :parent nil})
          cols (mapcat (fn [rc]
                         (if (:star rc)
                           (for [i (range (:ncols src))]
                             {:name (nth (:cols src) i)
                              :f (let [sl (long (:slot src)) i (long i)] (fn [^objects env] (nth (aget env sl) i)))})
                           [{:name (or (:alias rc)
                                       (if (= :col (:op (:e rc)))
                                         (let [n (:name (:e rc))]
                                           (if (and (#{"rowid" "oid" "_rowid_"} (lc n)) (:ipk src) (nil? (:table (:e rc))))
                                             (nth (:cols src) (:ipk src))
                                             n))
                                         (if-let [npe (:name-pe rc)] (subs (:sql ctx) (:ps (:e rc)) npe) (e/text-of-node ctx (:e rc)))))
                             :f (:f (e/cx ctx (:e rc)))}]))
                       items)]
      (cond-> {:names (mapv :name cols) :fs (mapv :f cols) :slot (:slot src)}
        (some (fn [x] (and (map? x) (or (#{:subq :exists} (:op x)) (:select x)))) (tree-seq coll? seq items))
        (assoc :redo (fn [db']
                       (let [ctx2 (new-ctx db' (:sql ctx))
                             src2 (assoc src :slot (e/new-slot! ctx2))
                             r2 (compile-returning ctx2 src2 items)
                             env2 (object-array (+ 2 @(:slots ctx2)))]
                         (fn [row] (aset env2 (long (:slot src2)) row) (mapv #(% env2) (:fs r2))))))))))

(defn- ret-row
  "The RETURNING row for the row held in env; subqueries see database db' (the state right after the change)."
  [ret ^objects env db']
  (if-let [redo (:redo ret)]
    ((redo db') (aget env (long (:slot ret))))
    (mapv #(% env) (:fs ret))))

(defn- next-rowid [tbl]
  (if (empty? (:rows tbl))
    1
    (let [m (long (key (first (rseq (:rows tbl)))))]
      (if (= m Long/MAX_VALUE)
        ;; SQLite then tries random rowids (deterministic here: a fixed-seed generator)
        (let [rnd (java.util.Random. (count (:rows tbl)))]
          (or (some (fn [_] (let [r (bit-and (.nextLong rnd) Long/MAX_VALUE)]
                              (when (and (pos? r) (not (contains? (:rows tbl) r))) r)))
                    (range 100))
              (err "database or disk is full")))
        (inc m)))))

(defn- upsert-matcher
  "Returns (fn [conflict-target]) deciding whether an upsert clause applies to a conflict."
  [tbl clause]
  (let [target (:target clause)]
    (fn [what]
      (cond
        (nil? clause) false
        (nil? target) true
        (= what :rowid) (and (= 1 (count (:cols target)))
                             (nil? (:where target))
                             (let [n (lc (:name (:e (first (:cols target)))))]
                               (or (and (:ipk tbl) (= n (lc (:name (nth (:cols tbl) (:ipk tbl))))))
                                   (and (not (:without-rowid tbl)) (#{"rowid" "oid" "_rowid_"} n)))))
        (map? what)
        (let [sp (var-get #'e/strip-pos)
              tw (:where target) iw (:where-ast what)
              tcols (:cols target) icols (:cols what)]
          (and (= (count tcols) (count icols))
               (if tw (or (nil? iw) (= (sp tw) (sp iw))) (nil? iw))
               (every? (fn [ic]
                         (some (fn [tc]
                                 (and (if (:name ic)
                                        (and (= :col (:op (:e tc))) (= (lc (:name ic)) (lc (:name (:e tc)))))
                                        (and (:ast ic) (= (sp (:e tc)) (sp (:ast ic)))))
                                      (or (nil? (:coll tc)) (= (lc (str (:coll ic))) (lc (str (:coll tc)))))))
                               tcols))
                       icols)))
        :else false))))

(defn- no-journal? [db] (= "off" (get-in db [:pragmas "journal_mode.main"])))

(defn- fail-partial
  "Runs (f) for one row; an error under conflict action FAIL keeps the rows done so far. With journal_mode=OFF there
  is no statement journal either, so every error keeps them."
  [db changes tkey tbl f]
  (try (f)
       (catch clojure.lang.ExceptionInfo ex
         (if (or (= :fail (:conflict (ex-data ex))) (and (no-journal? db) (:sql-error (ex-data ex))))
           (throw (ex-info (ex-message ex) (assoc (ex-data ex) :partial-db (assoc (if (:view tbl) (update db :tables dissoc tkey) db)
                                                                     :changes (if (= :fail (:conflict (ex-data ex))) changes 0)))))
           (throw ex)))))

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
              [(str "$v$" l)
               {:name (str "$v$" l) :view true :vkey l
                :cols (mapv (fn [n] {:name n :type nil :aff :blob}) cols)
                :ipk nil :strict false :checks []
                :rows (into (sorted-map) (map-indexed (fn [i r] [(long (inc i)) (conj (vec (v/strip-row r)) (long (inc i)))]) rows))}]))
        (err (str "no such table: " (cond (:schema st) (str (:schema st) ".") e/*in-trigger* "main.") tname))))))

(defn- with-target [db tkey tbl] (if (:view tbl) (assoc-in db [:tables tkey] tbl) db))
(defn- end-target [db tkey tbl] (if (:view tbl) (update db :tables dissoc tkey) db))

(defn- after-update
  "AFTER UPDATE triggers for a row an upsert updated; returns [db changes]."
  [db tbl old new tkey changed-cis changes]
  (let [cols (map #(lc (:name (nth (:cols tbl) %))) changed-cis)
        trigs (trg/matching db (:vkey tbl tkey) :after :update cols)
        r (if (seq trigs) (try-fire db trigs tbl old new) db)]
    (if (= r ::ignore) [db changes] [r (inc changes)])))

(defn- fk-note!
  "Records a touched row for the end-of-statement foreign key check."
  [db tbl tkey old new]
  (when fk/*child*
    (when (seq (:fks tbl)) (swap! fk/*child* conj [tkey old new]))
    (when (seq (fk/referencing db tbl)) (swap! fk/*parent* conj [tkey old new]))))

(defn- exec-insert [db sql st emit prepared!]
  (let [tname (:table st)
        [tkey tbl] (dml-target db sql st :insert)
        db (with-target db tkey tbl)
        fkt (and (not (:view tbl)) (fk/tracks? db tbl))
        vw (:view tbl)
        ncols (count (:cols tbl))
        lnames (mapv (comp lc :name) (:cols tbl))
        ngen (count (filter :gen (:cols tbl)))
        targets (if (:default st) [] (if (:cols st)
                  (mapv (fn [c] (let [i (.indexOf ^java.util.List lnames (lc c))]
                                  (when (and (>= i 0) (:gen (nth (:cols tbl) i)))
                                    (err (str "cannot INSERT into generated column \"" (:name (nth (:cols tbl) i)) "\"")))
                                  (cond (>= i 0) i
                                        (and (not (:without-rowid tbl)) (#{"rowid" "oid" "_rowid_"} (lc c))) (or (:ipk tbl) -1)
                                        :else (err (str "table " (:name tbl) " has no column named " c)))))
                        (:cols st))
                  (vec (remove #(:gen (nth (:cols tbl) %)) (range ncols)))))
        ctx (new-ctx db sql)
        on-conflict (:or st)
        ;; source rows
        rows (cond
               (:default st) [[]]
               (:rows st) (let [rs (:rows st)]
                            (when (apply not= (map count rs))
                              (err "all VALUES must have the same number of terms"))
                            (doseq [r rs, x r] (e/cx ctx x))
                            (doseq [r rs]
                              (when (not= (count r) (count targets))
                                (if (:cols st)
                                  (err (str (count r) " values for " (count targets) " columns"))
                                  (err (str "table " (if-let [q (:qual-schema st)] (str q "." (sch/display-name (:name tbl) tbl)) (:name tbl)) " has " (- ncols ngen) " columns but " (count r) " values were supplied")))))
                            (mapv (fn [r] (mapv #(:f (e/cx ctx %)) r)) rs))
               (:select st) (let [sub (e/compile-select ctx (:select st))]
                              (when (not= (count (:names sub)) (count targets))
                                (if (:cols st)
                                  (err (str (count (:names sub)) " values for " (count targets) " columns"))
                                  (err (str "table " (if-let [q (:qual-schema st)] (str q "." (sch/display-name (:name tbl) tbl)) (:name tbl)) " has " (- ncols ngen) " columns but " (count (:names sub)) " values were supplied"))))
                              sub))
        upserts (:upsert st)
        ;; upsert compile: scope has the table (existing row) and "excluded"
        up-ctx (when (seq upserts) ctx)
        tsrc (when (seq upserts) (table-source ctx tbl (:alias st)))
        xsrc (when (seq upserts) (assoc (table-source ctx tbl (if (and (nil? (:alias st)) (= "excluded" (lc (:name tbl)))) "\u0000x" "excluded")) :rowid? false :qualified-only true))
        up-compiled (when (seq upserts)
                      (let [uctx (assoc ctx :scope {:sources [tsrc xsrc] :parent nil})]
                        (mapv (fn [u]
                                (when (:target u)
                                  (doseq [tc (:cols (:target u))]
                                    (let [e (:e tc)]
                                      (when (and (= :col (:op e)) (nil? (:table e))
                                                 (not (some #(= (lc (:name %)) (lc (:name e))) (:cols tbl)))
                                                 (not (and (not (:without-rowid tbl)) (#{"rowid" "oid" "_rowid_"} (lc (:name e))))))
                                        (err (str "no such column: " (:name e)))))))
                                (when (and (:target u) (not (some (fn [idx] ((upsert-matcher tbl u) idx))
                                                                  (cons :rowid (filter :unique (table-indexes db tname))))))
                                  (err "ON CONFLICT clause does not match any PRIMARY KEY or UNIQUE constraint"))
                                (assoc u
                                       :sets-c (when (= :update (:do u))
                                                 (let [items (vec (mapcat (fn [[cols ex]]
                                                                            (cond (= 1 (count cols)) [[(first cols) ex nil]]
                                                                                  (= :vector (:op ex))
                                                                                  (do (when (not= (count cols) (count (:items ex)))
                                                                                        (err (str (count cols) " columns assigned " (count (:items ex)) " values")))
                                                                                      (map (fn [c e] [c e nil]) cols (:items ex)))
                                                                                  (= :subq (:op ex)) (map-indexed (fn [k c] [c ex k]) cols)
                                                                                  :else (err (str (count cols) " columns assigned 1 values"))))
                                                                          (:sets u)))]
                                                   (vec (for [[c ex k] items]
                                                          [(let [i (.indexOf ^java.util.List lnames (lc c))
                                                                 i (if (and (neg? i) (#{"rowid" "oid" "_rowid_"} (lc c)) (:ipk tbl)) (:ipk tbl) i)]
                                                             (when (and (>= i 0) (:gen (nth (:cols tbl) i)))
                                                               (err (str "cannot UPDATE generated column \"" (:name (nth (:cols tbl) i)) "\"")))
                                                             (if (neg? i) (err (str "no such column: " c)) i))
                                                           (if (nil? k)
                                                             (:f (e/cx uctx ex))
                                                             (let [sub (e/compile-select uctx (:select ex))]
                                                               (fn [env] (let [r (first (e/run-select-rows sub env))] (when r (nth r k))))))]))))
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
        (fail-partial db changes tkey tbl (fn []
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
                    (if (:autoinc tbl)
                      (let [m (long (if (empty? (:rows tbl)) 0 (key (first (rseq (:rows tbl))))))
                            sv (long (sequence-value db tbl))
                            mx (max m sv)]
                        (if (= mx Long/MAX_VALUE) (err "database or disk is full") (inc mx)))
                      (next-rowid tbl))
                    (let [x (v/apply-affinity rid-val :integer)]
                      (if (instance? Long x) x (err "datatype mismatch"))))
              row (if ipk (assoc row ipk rid) row)
              raw-row row
              row (reduce (fn [row ci] (if (= ci ipk) row (assoc row ci (apply-col-affinity tbl ci (nth row ci)))))
                          row (range ncols))
              row (fill-generated tbl (assoc row ncols rid))
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
                              nrow (fill-generated tbl (assoc nrow ncols nrid))
                              nrow (reduce (fn [r ci] (if (= ci ipk) r (assoc r ci (apply-col-affinity tbl ci (nth r ci)))))
                                           nrow (map first (:sets-c up)))
                              [nrow _] (check-row db sql tbl nrow :abort)
                              [tblu res2] (resolve-uniques db tbl nrid nrow crid :abort (constantly false))
                              tblu (put-row (update tblu :rows dissoc crid) nrid nrow)]
                          (when ret
                            (aset env (long (:slot ret)) (assoc nrow ncols nrid))
                            (.add ret-rows (ret-row ret env (assoc-in db [:tables tkey] tblu))))
                          (let [db3 (assoc-in db [:tables tkey] tblu)
                                db3 (if fkt
                                      (do (fk-note! db3 tbl tkey existing (assoc nrow ncols nrid))
                                          (fk/on-parent-change db3 tbl existing (assoc nrow ncols nrid)))
                                      db3)]
                            (after-update db3 tbl existing (assoc nrow ncols nrid) tkey (map first (:sets-c up)) changes)))))))
                :else
                (let [replaced (when (and (= 1 (get-in db [:pragmas "recursive_triggers"])) (not vw) (trg/any-for? db tkey :delete))
                                 (seq (remove (fn [rid0] (contains? (:rows tbl2) rid0)) (keys (:rows tbl)))))
                      db (if replaced
                           (let [dbd (assoc-in db [:tables tkey] tbl2)]
                             (reduce (fn [d rid0]
                                       (let [old0 (get-in tbl [:rows rid0])
                                             bts (trg/matching d tkey :before :delete nil)
                                             d (if (seq bts) (let [r (try-fire d bts tbl old0 nil)] (if (= r ::ignore) d r)) d)
                                             ats (trg/matching d tkey :after :delete nil)]
                                         (if (seq ats) (let [r (try-fire d ats tbl old0 nil)] (if (= r ::ignore) d r)) d)))
                                     dbd replaced))
                           db)
                      tbl2 (if replaced (get-in db [:tables tkey]) tbl2)
                      tbl3 (put-row tbl2 rid row)
                      _ (carry-unique! tbl2 tbl3 rid (get-in tbl3 [:rows rid]))
                      db2 (assoc-in db [:tables tkey] tbl3)
                      db2 (if (:autoinc tbl) (bump-sequence db2 tbl rid) db2)
                      db2 (if (or (:without-rowid tbl) vw) db2 (assoc db2 :last-rowid rid))
                      _ (when fkt (fk-note! db2 tbl tkey nil (assoc row ncols rid)))
                      db2 (if fkt
                            (reduce (fn [d rid0]
                                      (let [old0 (get-in tbl [:rows rid0])]
                                        (fk-note! d tbl tkey old0 nil)
                                        (fk/on-parent-change d tbl old0 nil)))
                                    db2 (remove (fn [rid0] (contains? (:rows tbl2) rid0)) (keys (:rows tbl))))
                            db2)]
                  (when ret
                    (aset env (long (:slot ret)) (assoc row ncols rid))
                    (.add ret-rows (ret-row ret env db2)))
                  (let [r (if (seq (trg/matching db2 (:vkey tbl tkey) (if vw :instead :after) :insert nil))
                            (try-fire db2 (trg/matching db2 (:vkey tbl tkey) (if vw :instead :after) :insert nil) tbl3 nil (assoc row ncols rid))
                            db2)]
                    (if (= r ::ignore) [db2 changes] [r (if vw changes (inc changes))]))))))))))
           [db 0] src-rows)]
      (doseq [r ret-rows] (emit r))
      (assoc (end-target db tkey tbl) :changes changes))))



;; ====================================================================== UPDATE / DELETE

(defn- exec-update [db sql st emit prepared!]
  (let [tname (:table st)
        [tkey tbl] (dml-target db sql st :update)
        vw (:view tbl)
        fkt (and (not vw) (fk/tracks? db tbl))
        db (with-target db tkey tbl)
        ncols (count (:cols tbl))
        lnames (mapv (comp lc :name) (:cols tbl))
        ctx (new-ctx db sql)
        src (table-source ctx tbl (:alias st))
        _ (when (and (:indexed-by st) (not= :none (:indexed-by st)))
            (when-not (get-in db [:indexes (lc (:indexed-by st))])
              (err (str "no such index: " (:indexed-by st)))))
        sctx (assoc ctx :scope {:sources [src] :parent nil})
        from (:from st)
        _ (when from
            (doseq [it from]
              (when (and (:table it) (= (lc (or (:alias it) (:table it))) (lc (or (:alias st) tname))))
                (err (str "target object/alias may not appear in FROM clause: " (or (:alias st) tname))))))
        set-items (vec (mapcat (fn [[cols ex]]
                                 (cond (= 1 (count cols)) [[(first cols) ex nil]]
                                       (= :vector (:op ex))
                                       (do (when (not= (count cols) (count (:items ex)))
                                             (err (str (count cols) " columns assigned " (count (:items ex)) " values")))
                                           (map (fn [c e] [c e nil]) cols (:items ex)))
                                       (= :subq (:op ex)) (map-indexed (fn [k c] [c ex k]) cols)
                                       :else (err (str (count cols) " columns assigned 1 values"))))
                               (:sets st)))
        sets (vec (for [[c ex k] set-items]
                    [(let [i (.indexOf ^java.util.List lnames (lc c))]
                       (when (and (>= i 0) (:gen (nth (:cols tbl) i)))
                         (err (str "cannot UPDATE generated column \"" (:name (nth (:cols tbl) i)) "\"")))
                       (cond (>= i 0) i
                             (and (not (:without-rowid tbl)) (#{"rowid" "oid" "_rowid_"} (lc c))) (or (:ipk tbl) -1)
                             :else (err (str "no such column: " c))))
                     (when-not from
                       (if (nil? k)
                         (:f (e/cx sctx ex))
                         (let [sub (e/compile-select sctx (:select ex))]
                           (when (not= (count (:names sub)) (count (filter #(= ex (second %)) set-items)))
                             (err (str (count (filter #(= ex (second %)) set-items)) " columns assigned " (count (:names sub)) " values")))
                           (fn [env] (let [r (first (e/run-select-rows sub env))] (when r (nth r k)))))))]))
        where-f (when (and (:where st) (not from)) (e/condf (e/cx sctx (:where st))))
        where-pre (when (and (:where st) (not from)) (e/where-precheck sctx (:where st)))
        ret (compile-returning ctx src (:returning st))
        on-conflict (:or st)
        env (object-array (+ 2 @(:slots ctx)))
        slot (long (:slot src))
        _ (prepared! (:names ret))
        join-count (volatile! nil)
        targets (if from
                  (let [sctx2 (new-ctx db sql)
                        cols (into [{:e {:op :col :table (or (:alias st) tname) :name "\u0000rowid"} :alias "rid"}]
                                   (map-indexed (fn [i [_ ex k]] {:e (if k {:op :lit :v nil} ex) :alias (str "c" i)}) set-items))
                        sel {:body {:type :core :cols cols
                                    :from (into [{:table (if vw tkey tname) :schema (:schema st) :alias (or (:alias st) tname)}]
                                                (map-indexed (fn [i it] (if (zero? i) (assoc it :join :cross-comma) it)) from))
                                    :where (:where st)}}
                        sub (e/compile-select sctx2 sel)
                        rows (e/run-select-rows sub (object-array (+ 2 @(:slots sctx2))))]
                    (let [rows (vec rows)
                          ;; one update per target row (the last join row wins), changes count every join row
                          last-by-rid (reduce (fn [m r] (assoc m (first r) (vec (rest r)))) (sorted-map) rows)]
                      (vreset! join-count (count rows))
                      (mapv (fn [[rid pre]] [rid pre]) last-by-rid)))
                  (vec (filter (fn [[rid row]]
                                 (aset env slot row)
                                 (or (nil? where-f) (let [w (where-f env)] (and (not (nil? w)) (v/bool-value w)))))
                               (if (and where-pre (not (where-pre env))) [] (:rows tbl)))))
        ret-rows (java.util.ArrayList.)]
    (let [set-names (mapv (fn [[ci _]] (if (neg? ci) "rowid" (lc (:name (nth (:cols tbl) ci))))) sets)
          [db changes]
          (reduce
           (fn [[db changes] [rid pre]]
             (fail-partial db changes tkey tbl (fn []
             (let [tbl (get-in db [:tables tkey])
                   old (get-in tbl [:rows rid])]
               (if (nil? old)
                 [db changes]
                 (let [_ (aset env slot old)
                       newvals (if from
                                 (mapv (fn [[ci _] v] [ci v]) sets pre)
                                 (mapv (fn [[ci f]] [ci (f env)]) sets))
                       row (reduce (fn [row [ci val]] (if (= ci -1) row (assoc row ci val))) old newvals)
                       rid-new (let [rv (some (fn [[ci val]] (when (= ci -1) val)) newvals)
                                     rv (if (:ipk tbl) (nth row (:ipk tbl)) (if (some #(= -1 (first %)) newvals) rv rid))]
                                 (if (nil? rv)
                                   (err "datatype mismatch")
                                   (let [x (v/apply-affinity rv :integer)]
                                     (if (instance? Long x) x (err "datatype mismatch")))))
                       row (if (:ipk tbl) (assoc row (:ipk tbl) rid-new) row)
                       row (fill-generated tbl (assoc row ncols rid-new))
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
                               db2 (if fkt
                                     (do (fk-note! db2 tbl tkey old (assoc row ncols rid-new))
                                         (fk/on-parent-change db2 tbl old (assoc row ncols rid-new)))
                                     db2)
                               ats (trg/matching db2 (:vkey tbl tkey) (if vw :instead :after) :update set-names)]
                           (when ret
                             (aset env slot (assoc row ncols rid-new))
                             (.add ret-rows (ret-row ret env db2)))
                           (let [r (if (seq ats) (try-fire db2 ats tbl3 old (assoc row ncols rid-new)) db2)]
                             (if (= r ::ignore) [db2 changes] [r (if vw changes (inc changes))]))))))))))))
           [db 0] targets)]
      (doseq [r ret-rows] (emit r))
      (assoc (end-target db tkey tbl) :changes changes))))

(defn- exec-delete [db sql st emit prepared!]
  (let [tname (:table st)
        [tkey tbl] (dml-target db sql st :delete)
        vw (:view tbl)
        fkt (and (not vw) (fk/tracks? db tbl))
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
        dret-rows (java.util.ArrayList.)
        doomed (vec (filter (fn [[rid row]]
                              (aset env slot row)
                              (or (nil? where-f) (let [w (where-f env)] (and (not (nil? w)) (v/bool-value w)))))
                            (if (and where-pre (not (where-pre env))) [] (:rows tbl))))]
    (when (and ret (not (:redo ret)))
      (doseq [[_ row] doomed]
        (aset env slot row)
        (emit (mapv #(% env) (:fs ret)))))
    (if (and (not vw) (not fkt) (not (:redo ret)) (not (trg/any-for? db tkey :delete)))
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
                             db (if fkt
                                  (do (fk-note! db tbl tkey old nil)
                                      (fk/on-parent-change db tbl old nil))
                                  db)
                             ats (trg/matching db (:vkey tbl tkey) (if vw :instead :after) :delete nil)
                             _ (when (:redo ret) (aset env slot old) (.add dret-rows (ret-row ret env db)))
                             r (if (seq ats) (try-fire db ats (get-in db [:tables tkey]) old nil) db)]
                         (if (= r ::ignore) [db changes] [r (if vw changes (inc changes))])))))))
             [db 0] doomed)]
        (doseq [r dret-rows] (emit r))
        (assoc (end-target db tkey tbl) :changes changes)))))

;; ====================================================================== other DDL

(defn- create-trigger [db sql st]
  (let [name (:name st) ln (lc name) tl (lc (:table st))
        tbl (get-in db [:tables tl]) view (get-in db [:views tl])]
    (cond
      (get-in db [:triggers ln]) (if (:if-not-exists st) ::empty (err (str "trigger " (or (:name-raw st) name) " already exists")))
      (contains? sch/master-names tl) (err "cannot create trigger on system table")
      (str/starts-with? ln "sqlite_") (err (str "object name reserved for internal use: " name))
      (some :returning (:body st)) (err "cannot use RETURNING in a trigger")
      (and (nil? tbl) (nil? view)) (err (str "no such table: " (if (and (:temp st) (nil? (:table-schema st))) "" (str (or (:table-schema st) "main") "."))
                                                      (let [t (str (:table st)) q (some-> (:table-schema st) lc)]
                                                        (if (and q (str/starts-with? (lc t) (str q "."))) (subs t (inc (count q))) t))))
      (and view (not= :instead (:timing st)))
      (err (str "cannot create " (if (= :before (:timing st)) "BEFORE" "AFTER") " trigger on view: " (:table st)))
      (and tbl (= :instead (:timing st))) (err (str "cannot create INSTEAD OF trigger on table: " (:table st)))
      (and tbl (or (:virtual tbl) (str/starts-with? tl "sqlite_"))) (err "cannot create trigger on system table")
      :else
      (let [trig (-> (trg/make-trigger st sql)
                     (assoc :table tl :table-name (sch/display-name (:name (or tbl view)) (or tbl view)) :temp (boolean (or (:temp st) (= "temp" (lc (:schema st))) (:temp tbl) (:temp view)))
                            :db (when-not (or (:temp st) (= "temp" (lc (:schema st)))) (:db st))
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
      (let [_ (doseq [ic (:cols st) :when (not= :col (:op (:e ic)))] (check-gen-expr (:e ic) "index expressions"))
            _ (when (:where st) (check-gen-expr (:where st) "partial index WHERE clauses"))
            idx (assoc (build-index db sql tbl name (:unique st) (:cols st) (:where st) false)
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

(defn- drop-seq-row [db tname]
  (if-let [t (get-in db [:tables "sqlite_sequence"])]
    (assoc-in db [:tables "sqlite_sequence" :rows]
              (into (sorted-map) (remove (fn [[_ r]] (= (first r) tname)) (:rows t))))
    db))

(defn- exec-drop [db st]
  (let [lname (lc (:name st))
        q (some-> (:qual-schema st) lc)
        q (when (#{"main" "temp"} q) q)
        miss? (fn [o] (and o q (if (= q "main") (boolean (:temp o)) (not (:temp o)))))
        look (fn [k] (let [o (get-in db k)] (when-not (miss? o) o)))
        shown (if q (str q "." (:name st)) (:name st))]
    (case (:what st)
      :table (if-let [t (look [:tables lname])]
               (let [idxs (filter (fn [i] (= (:table i) lname)) (vals (:indexes db)))
                     db (reduce (fn [db o] (sch/free-page db (sch/schema-of o) (:rootpage o))) db (cons t idxs))]
                 (-> db
                     (update :tables dissoc lname)
                     (update :indexes (fn [m] (into {} (remove (fn [[_ i]] (= (:table i) lname)) m))))
                     (update :triggers (fn [m] (into {} (remove (fn [[_ g]] (= (:table g) lname)) m))))
                     (drop-seq-row (:name t))))
               (cond (look [:views lname]) (err (str "use DROP VIEW to delete view " (:name st)))
                     (:if-exists st) db
                     :else (err (str "no such table: " shown))))
      :index (if-let [i (look [:indexes lname])]
               (if (:auto i)
                 (err "index associated with UNIQUE or PRIMARY KEY constraint cannot be dropped")
                 (-> db (sch/free-page (sch/schema-of i) (:rootpage i)) (update :indexes dissoc lname)))
               (if (:if-exists st) db (err (str "no such index: " shown))))
      :view (if (look [:views lname])
              (-> db (update :views dissoc lname)
                  (update :triggers (fn [m] (into {} (remove (fn [[_ g]] (= (:table g) lname)) m)))))
              (cond (look [:tables lname]) (err (str "use DROP TABLE to delete table " (:name st)))
                    (:if-exists st) db
                    :else (err (str "no such view: " shown))))
      :trigger (if (look [:triggers lname])
                 (update db :triggers dissoc lname)
                 (if (:if-exists st) ::empty (err (str "no such trigger: " shown)))))))

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

(declare kw-tok? tok-op? matching-paren)

(def ^:private alias-stop
  #{"WHERE" "ON" "USING" "JOIN" "LEFT" "RIGHT" "FULL" "INNER" "CROSS" "NATURAL" "OUTER" "GROUP" "ORDER" "LIMIT" "HAVING"
    "SET" "WINDOW" "UNION" "INTERSECT" "EXCEPT" "INDEXED" "NOT" "RETURNING" "BEGIN" "END" "VALUES" "SELECT" "DEFAULT"
    "WHEN" "FOR" "OFFSET" "FROM" "INTO" "AND" "OR" "THEN" "ELSE" "ASC" "DESC" "COLLATE" "IS" "IN" "LIKE" "GLOB" "BETWEEN"
    "PRIMARY" "UNIQUE" "CHECK" "REFERENCES" "CONSTRAINT" "FOREIGN" "NULL" "AS"})

(defn- aliases-of
  "Lower-case aliases bound to table tn in the token vector: tn AS a / tn a."
  [toks tn]
  (set (for [i (range (dec (count toks)))
             :let [t (nth toks i) n1 (nth toks (inc i))
                   n2 (when (< (+ i 2) (count toks)) (nth toks (+ i 2)))]
             :when (and (tok-name? t tn)
                        (or (not (pos? i)) (not (tok-op? (nth toks (dec i)) ".")))
                        (not (tok-op? n1 ".")))
             a (cond (and (kw-tok? n1 "AS") n2 (#{:id :qid} (:t n2))) [(lc (:v n2))]
                     (= :qid (:t n1)) [(lc (:v n1))]
                     (and (= :id (:t n1)) (not (alias-stop (:u n1)))) [(lc (:v n1))]
                     :else [])]
         a)))

(defn- fk-parent-spans
  "Token index ranges [parent-lc from to) of the parent column lists in REFERENCES clauses."
  [toks]
  (vec (for [i (range (count toks))
             :let [t (nth toks i)]
             :when (and (kw-tok? t "REFERENCES") (< (+ i 2) (count toks)))
             :let [n1 (nth toks (inc i)) n2 (nth toks (+ i 2))]
             :when (and (#{:id :qid :str} (:t n1)) (tok-op? n2 "("))
             :let [close (matching-paren toks (+ i 2))]]
         [(lc (:v n1)) (+ i 3) close])))

(defn- in-from-list?
  "Whether token i sits in the table list of a FROM clause (after a comma)."
  [toks i]
  (loop [j (dec i) depth 0]
    (if (neg? j) false
        (let [t (nth toks j)]
          (cond (tok-op? t ")") (recur (dec j) (inc depth))
                (tok-op? t "(") (if (zero? depth) false (recur (dec j) (dec depth)))
                (pos? depth) (recur (dec j) depth)
                (and (= :id (:t t)) (#{"FROM" "JOIN"} (:u t))) true
                (and (= :id (:t t)) (#{"SELECT" "WHERE" "ON" "SET" "GROUP" "ORDER" "HAVING" "VALUES" "USING" "INTO" "UPDATE" "BEGIN" "WHEN"} (:u t))) false
                (tok-op? t ";") false
                :else (recur (dec j) depth))))))

(defn- rename-table-refs [sql old new]
  (let [toks0 (vec (lx/tokenize sql))
        as-alias (some (fn [[i t]] (and (tok-name? t old) (pos? i) (kw-tok? (nth toks0 (dec i)) "AS"))) (map-indexed vector toks0))]
    (rewrite-tokens sql (fn [toks i]
                          (when (tok-name? (nth toks i) old)
                            (cond
                              (and (or (#{"FROM" "JOIN" "INTO" "UPDATE" "ON" "TABLE" "REFERENCES"} (prev-kw toks i))
                                       (and (pos? i) (tok-op? (nth toks (dec i)) ",") (in-from-list? toks i)))
                                   (not (and (pos? i) (tok-op? (nth toks (dec i)) "."))))
                              (quote-id new)
                              ;; qualifier of a column: old.col, when old is not an alias
                              (and (not as-alias) (< (inc i) (count toks)) (tok-op? (nth toks (inc i)) ".")
                                   (or (zero? i) (not (tok-op? (nth toks (dec i)) "."))))
                              (quote-id new)))))))

(defn- rename-trigger-on
  "legacy_alter_table: only the ON <table> clause of a trigger is rewritten."
  [sql old new]
  (rewrite-tokens sql (fn [toks i]
                        (when (and (tok-name? (nth toks i) old) (= "ON" (prev-kw toks i))
                                   (not-any? #(= "BEGIN" (:u (nth toks %))) (range i)))
                          (quote-id new)))))

(defn- mentions-table? [sql tname]
  (some #(tok-name? % tname) (lx/tokenize sql)))

(defn- rename-column-refs
  "Renames column old to new in the text sql. tn: the table (plain name) the column belongs to;
  trig-table: for a trigger text, the plain name of its table. self?: sql is tn's own CREATE text."
  ([sql old new] (rename-column-refs sql old new nil nil true))
  ([sql old new tn trig-table self?]
   (let [toks (vec (lx/tokenize sql))
         from (if self? (or (first (keep-indexed (fn [i t] (when (tok-op? t "(") i)) toks)) 0) 0)
         quals (when tn (conj (aliases-of toks tn) (lc tn)))
         quals (cond-> (or quals #{}) (and trig-table tn (= (lc trig-table) (lc tn))) (conj "new" "old"))
         spans (fk-parent-spans toks)
         in-foreign-parent? (fn [i] (some (fn [[p a b]] (and (>= i a) (< i b) (not= p (lc tn)))) spans))]
     (rewrite-tokens sql (fn [toks i]
                           (let [t (nth toks i)]
                             (when (and (>= i from) (tok-name? t old))
                               (let [prev (when (pos? i) (nth toks (dec i)))
                                     nxt (when (< (inc i) (count toks)) (nth toks (inc i)))]
                                 (cond
                                   (in-foreign-parent? i) nil
                                   (and prev (tok-op? prev ".") (>= i 2))
                                   (when (or (nil? tn) (contains? quals (lc (:v (nth toks (- i 2)))))) (quote-if-needed new))
                                   (and nxt (tok-op? nxt "(") (not self?)) nil
                                   :else (quote-if-needed new))))))))))

(defn- rename-fk-parent-col
  "In the CREATE text of another table: parent column old of REFERENCES parent(...) becomes new."
  [sql parent old new]
  (let [toks (vec (lx/tokenize sql))
        hit (set (mapcat (fn [[p a b]] (when (= p (lc parent)) (range a b))) (fk-parent-spans toks)))]
    (rewrite-tokens sql (fn [toks i] (when (and (hit i) (tok-name? (nth toks i) old)) (quote-if-needed new))))))

(defn- rebuild-table
  "Replaces table tname by the table the new CREATE TABLE text describes; rows are mapped by row-fn (row incl. rowid).
  Non-automatic indexes are rebuilt from the text idx-sql-fn gives for them."
  [db tname new-sql row-fn idx-sql-fn]
  (let [old (get-in db [:tables tname])
        idxs (sort-by :mseq (filter #(= (:table %) tname) (vals (:indexes db))))
        db0 (-> db (update :tables dissoc tname)
                (update :indexes (fn [m] (into {} (remove (fn [[_ i]] (= (:table i) tname)) m)))))
        pre (when (and (:db old) (not= "main" (:db old))) (str (:db old) "."))
        st (assoc (parse-one new-sql) :temp (:temp old) :internal true)
        st (if pre (assoc st :name (str pre (:name st)) :db (:db old)) st)
        db1 (create-table db0 new-sql st)
        by-name (into {} (map (fn [i] [(lc (:name i)) i]) idxs))
        restore (fn [db objs-key key o]
                  (let [cur (get-in db [objs-key key])
                        db (sch/free-page db (sch/schema-of cur) (:rootpage cur))]
                    (assoc-in db [objs-key key] (assoc cur :mseq (:mseq o) :rootpage (:rootpage o)))))
        db1 (restore db1 :tables tname old)
        db1 (let [nt (get-in db1 [:tables tname])]
              (assoc-in db1 [:tables tname :rows]
                        (into (sorted-map) (map (fn [[rid r]] [rid (fill-generated nt (row-fn r))]) (:rows old)))))
        db1 (reduce (fn [db [k i]]
                      (if-let [o (by-name (lc (:name i)))] (restore db :indexes k o) db))
                    db1 (filter (fn [[_ i]] (= (:table i) tname)) (:indexes db1)))]
    (reduce (fn [db i]
              (if (:auto i) db
                  (let [isql (idx-sql-fn i)
                        ist (assoc (parse-one isql) :temp (:temp old) :internal true)
                        ist (if pre (assoc ist :name (str pre (:name ist)) :table (str pre (:table ist)) :db (:db old)) ist)
                        db' (create-index db isql ist)]
                    (restore db' :indexes (lc (:name i)) i))))
            db1 idxs)))

(defn- reschema-objects
  "Applies f (sql obj -> sql) to the text of views and triggers (re-parsing them)."
  [db f]
  (-> db
      (update :views (fn [m] (into {} (map (fn [[k v]]
                                            (let [ns (when (:sql v) (f (:sql v) v))]
                                              [k (if (or (nil? ns) (= ns (:sql v))) v
                                                     (let [st (parse-one ns)]
                                                       (assoc v :sql ns :src ns :select (:select st) :cols (:cols st))))]))
                                          m))))
      (update :triggers (fn [m] (into {} (map (fn [[k t]]
                                               (let [ns (when (:sql t) (f (:sql t) t))]
                                                 [k (if (or (nil? ns) (= ns (:sql t))) t
                                                        (parse-trigger-text t ns))]))
                                             m))))))

(defn- find-col [tbl n]
  (let [i (.indexOf ^java.util.List (mapv (comp lc :name) (:cols tbl)) (lc n))]
    (when (>= i 0) i)))

(declare kw-tok?)
(defn- find-constraint
  "Index of the CONSTRAINT keyword token of the constraint called nm in the table text, or nil."
  [toks nm]
  (first (keep-indexed (fn [i t]
                         (when (and (= :id (:t t)) (= "CONSTRAINT" (:u t))
                                    (< (inc i) (count toks)) (tok-name? (nth toks (inc i)) nm))
                           i))
                       toks)))

(defn- tok-op? [t o] (and (= :op (:t t)) (= o (:v t))))

(defn- matching-paren
  "Index of the ) closing the ( at token index i."
  [toks i]
  (loop [j (inc i) depth 1]
    (let [t (nth toks j)]
      (cond (= :eof (:t t)) j
            (tok-op? t "(") (recur (inc j) (inc depth))
            (tok-op? t ")") (if (= 1 depth) j (recur (inc j) (dec depth)))
            :else (recur (inc j) depth)))))

(defn- constraint-extent
  "Token index one past the constraint starting at token k (the token after the constraint name), or :undroppable
  / :name-only."
  [toks k]
  (let [t (nth toks k)]
    (case (:u t)
      "CHECK" (inc (matching-paren toks (inc k)))
      "NOT" (let [e (+ k 2)]
              (if (and (kw-tok? (nth toks e) "ON") (kw-tok? (nth toks (inc e)) "CONFLICT")) (+ e 3) e))
      ("PRIMARY" "UNIQUE" "FOREIGN" "REFERENCES") :undroppable
      :name-only)))

(defn- kw-tok? [t k] (and (= :id (:t t)) (= k (:u t))))

(def ^:private master-aliases #{"sqlite_master" "sqlite_schema" "sqlite_temp_master" "sqlite_temp_schema"})

(defn- simple-default?
  "What sqlite3ValueFromExpr can evaluate: literals, unary +/-, CAST of those."
  [e]
  (and (map? e)
       (case (:op e)
         :lit (not (:var e))
         :unary (and (#{:neg :pos} (:o e)) (simple-default? (:e e)))
         :cast (simple-default? (:e e))
         false)))

(defn- sql-err-msg [^Throwable ex]
  (when (instance? clojure.lang.ExceptionInfo ex) (:sql-error (ex-data ex))))

(defn- obj-ref-sql
  "SELECT text that reads view v."
  [v]
  (str "SELECT * FROM " (cond (:db v) (str (quote-id (:db v)) ".") (:temp v) "temp." :else "main.")
       (quote-id (sch/display-name (:name v) v))))

(defn- check-view [db v]
  (let [sql (obj-ref-sql v)]
    (try (let [st (parse-one sql)]
           (e/compile-select (new-ctx db sql) (:select st))
           nil)
         (catch Throwable ex (sql-err-msg ex)))))

(defn- dry-db [db]
  (update db :tables (fn [m] (into {} (map (fn [[k t]] [k (assoc t :rows (sorted-map))]) m)))))

(defn- check-trigger [db t]
  (let [tbl (or (get-in db [:tables (:table t)]) (get-in db [:views (:table t)]))]
    (when tbl
      (let [n (count (:cols tbl))
            row (vec (repeat (inc n) nil))
            t2 (-> t (dissoc :when) (assoc :name (str "\u0001" (:name t))))]
        (try (binding [trg/*active* #{}]
               (trg/fire (dry-db db) [t2] tbl row row))
             nil
             (catch Throwable ex (sql-err-msg ex)))))))

(defn- validate-schema
  "SQLite's sqlite_rename_test: the first view or trigger of the schema that no longer resolves, as
  [kind name message], or nil. Only unresolvable names count."
  [db]
  (let [objs (sort-by (fn [[_ o]] (or (:mseq o) 0))
                      (concat (map (fn [v] [:view v]) (vals (:views db)))
                              (map (fn [t] [:trigger t]) (vals (:triggers db)))))]
    (some (fn [[kind o]]
            (let [m (if (= kind :view) (check-view db o) (check-trigger db o))]
              (when (and m (str/starts-with? m "no such ")) [(if (= kind :view) "view" "trigger") (sch/display-name (:name o) o) m])))
          objs)))

(defn- pre-check [db]
  (when-let [[k n m] (validate-schema db)]
    (err (str "error in " k " " n ": " m))))

(defn- post-check [db what]
  (when-let [[k n m] (validate-schema db)]
    (err (str "error in " k " " n " after " what ": " m))))

(defn- exec-alter [db sql st ^StringBuilder out]
  (let [qs (some-> (:schema st) lc)
        att? (and qs (not (#{"main" "temp"} qs)))
        tname (cond (and att? (some #{qs} (sch/attached-names db))) (str qs "." (lc (:table st)))
                    att? "\u0000"
                    (= qs "main") (sch/resolve-key db (:table st) "main")
                    (nil? qs) (sch/resolve-key db (:table st) nil)
                    :else (lc (:table st)))
        fits? (fn [o] (and o (or (not (#{"main" "temp"} qs)) (if (= qs "temp") (boolean (:temp o)) (not (:temp o))))))
        view (let [v (get-in db [:views tname])] (when (fits? v) v))
        _ (when (and view (nil? (get-in db [:tables tname])))
            (err (case (:action st)
                   :add "Cannot add a column to a view"
                   :rename-col (str "cannot rename columns of view \"" (:name view) "\"")
                   :drop-col (str "cannot drop column from view \"" (:name view) "\"")
                   (str "view " (:name view) " may not be altered"))))
        tbl (or (let [t (get-in db [:tables tname])] (when (fits? t) t))
                (when (master-aliases (lc (:table st))) (err (str "table " (:table st) " may not be altered")))
                (err (str "no such table: " (when (:schema st) (str (:schema st) ".")) (:table st))))
        tsql (:sql tbl)]
    (when (str/starts-with? tname "sqlite_")
      (err (str "table " (:name tbl) " may not be altered")))
    (case (:action st)
      :rename
      (let [nn (:to st) pfx (when (:db tbl) (str (:db tbl) ".")) nk (lc (str pfx nn)) old (sch/display-name (:name tbl) tbl)
            legacy (let [x (get-in db [:pragmas "legacy_alter_table"])] (and x (not= 0 x) (not= "off" (lc (str x)))))
            fk-on (let [x (get-in db [:pragmas "foreign_keys"])] (and x (not= 0 x) (not= "off" (lc (str x)))))]
        (when (or (get-in db [:tables nk]) (get-in db [:indexes nk]) (get-in db [:views nk]))
          (err (str "there is already another table or index with this name: " nn)))
        (when-not legacy (pre-check db))
        (when (str/starts-with? nk "sqlite_") (err (str "object name reserved for internal use: " nn)))
        (let [tsql2 (let [s1 (rewrite-tokens tsql (fn [toks i] (when (= i 2) (quote-id nn))))]
                      (if legacy s1 (rename-table-refs s1 old nn)))
              legacy-bad (fn [what nm sql*]
                           (let [toks (vec (lx/tokenize sql*))]
                             (doseq [i (range (dec (count toks)))]
                               (when (and (>= i 3) (tok-name? (nth toks i) old) (tok-op? (nth toks (inc i)) ".")
                                          (not (tok-op? (nth toks (dec i)) ".")))
                                 (err (str "error in " what " " nm " after rename: no such column: " old "." (:v (nth toks (+ i 2)))))))))
              _ (when legacy
                  (legacy-bad "table" nn tsql2)
                  (doseq [i (vals (:indexes db))
                          :when (and (= (:table i) tname) (:sql i))]
                    (legacy-bad "index" (sch/display-name (:name i) i) (:sql i))))
              db (-> db
                     (update :tables dissoc tname)
                     (assoc-in [:tables nk] (assoc tbl :name (str pfx nn) :sql tsql2))
                     (update :indexes (fn [m] (into {} (map (fn [[k i]]
                                                              (if (= (:table i) tname)
                                                                (let [i (assoc i :table nk
                                                                               :sql (when (:sql i) (if legacy (rename-trigger-on (:sql i) old nn) (rename-table-refs (:sql i) old nn))))
                                                                      i (if (:auto i)
                                                                          (let [nm (sch/display-name (:name i) i) p2 (str "sqlite_autoindex_" old "_")]
                                                                            (if (str/starts-with? (lc nm) (lc p2))
                                                                              (assoc i :name (str pfx "sqlite_autoindex_" nn "_" (subs nm (count p2))))
                                                                              i))
                                                                          i)]
                                                                  [(lc (:name i)) i])
                                                                [k i])) m))))
                     ;; foreign keys of other tables
                     (update :tables (fn [m] (into {} (map (fn [[k t]]
                                                             [k (if (and (not= k nk) (:sql t) (or (not legacy) fk-on) (mentions-table? (:sql t) old))
                                                                  (assoc t :sql (rename-table-refs (:sql t) old nn)
                                                                         :fks (mapv (fn [fk] (if (= (lc (:table fk)) (lc old)) (assoc fk :table nn) fk)) (:fks t))) t)]) m))))
                     (update :triggers (fn [m] (into {} (map (fn [[k t]]
                                                               [k (if (= (:table t) tname) (assoc t :table nk :table-name nn) t)]) m)))))
              db (reschema-objects db (fn [s _] (if (and s (mentions-table? s old))
                                                (if legacy
                                                  (if (str/starts-with? (str/upper-case (str/triml s)) "CREATE TRIGGER")
                                                    (rename-trigger-on s old nn) s)
                                                  (rename-table-refs s old nn))
                                                s)))
              db (if-let [t (get-in db [:tables "sqlite_sequence"])]
                   (assoc-in db [:tables "sqlite_sequence" :rows]
                             (into (sorted-map) (map (fn [[rid r]] [rid (if (= (lc (first r)) nk) r (if (= (lc (first r)) tname) (assoc r 0 nn) r))]) (:rows t))))
                   db)]
          (when-not legacy (post-check db "rename"))
          db))
      :add
      (let [d (:col st)
            nm (:name d)
            _ (when (find-col tbl nm) (err (str "duplicate column name: " nm)))
            cons (:cons d)
            _ (when (some #(= :pk (:c %)) cons) (err "Cannot add a PRIMARY KEY column"))
            _ (when (some #(= :unique (:c %)) cons) (err "Cannot add a UNIQUE column"))
            _ (when (some #(and (= :generated (:c %)) (:stored %)) cons)
                (.append out "# raise(ABORT,'cannot add a STORED column')\n")
                (err "cannot add a STORED column"))
            dflt (:e (first (filter #(= :default (:c %)) cons)))
            notnull (some #(= :notnull (:c %)) cons)
            has-check (some #(= :check (:c %)) cons)
            coldef (subs sql (:s d) (:e d))
            _ (when (and dflt (not (simple-default? dflt)))
                (.append out "# raise(ABORT,'Cannot add a column with non-constant default')\n")
                (err "Cannot add a column with non-constant default"))
            dv (when dflt ((:f (e/cx (new-ctx db sql) dflt)) (object-array 1)))
            _ (when (and notnull (or (nil? dflt) (nil? dv)))
                (.append out "# raise(ABORT,'Cannot add a NOT NULL column with default value NULL')\n")
                (when (seq (:rows tbl)) (err "Cannot add a NOT NULL column with default value NULL")))
            _ (when (and (:strict tbl) (not (and has-check (not notnull))))
                (.append out "# CASE WHEN quick_check GLOB 'CHECK*' THEN raise(ABORT,'CHECK constraint failed') WHEN quick_check GLOB 'non-* value in*' THEN raise(ABORT,'type mismatch on DEFAULT') ELSE raise(ABORT,'NOT NULL constraint failed') END\n"))
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
        (try
          (rebuild-table db tname new-sql
                         (fn [r] (conj (pop r) dv (peek r)))
                         (fn [i] (:sql i)))
          (catch clojure.lang.ExceptionInfo ex
            (if-let [m (:sql-error (ex-data ex))]
              (err (str "error in table " (:name tbl) " after add column: " m))
              (throw ex)))))
      :rename-col
      (let [i (find-col tbl (:old st))
            _ (when-not i (err (str "no such column: \"" (or (:old-raw st) (:old st)) "\"")))
            old (:old st) nn (:to st)
            _ (let [names (mapv :name (:cols tbl))
                    names (assoc names i nn)
                    seen (volatile! #{})]
                (doseq [n names]
                  (when (contains? @seen (lc n))
                    (err (str "error in table " (sch/display-name (:name tbl) tbl) " after rename: duplicate column name: " n)))
                  (vswap! seen conj (lc n))))
            legacy (let [x (get-in db [:pragmas "legacy_alter_table"])] (and x (not= 0 x) (not= "off" (lc (str x)))))
            _ (when-not legacy (pre-check db))
            tn (sch/display-name (:name tbl) tbl)
            new-sql (rename-column-refs tsql old nn tn nil true)
            db2 (rebuild-table db tname new-sql identity
                               (fn [ix] (rename-column-refs (:sql ix) old nn tn nil false)))
            db2 (reschema-objects db2 (fn [s o] (if (and s (or (mentions-table? s tn) (and (:table o) (= (:table o) tname))))
                                                  (rename-column-refs s old nn tn (when (:table o) (sch/display-name (or (:table-name o) (:table o)) o)) false)
                                                  s)))
            ;; foreign keys of other tables that name this column as the parent column
            db2 (update db2 :tables (fn [m] (into {} (map (fn [[k t]]
                                                            [k (if (and (not= k tname) (:sql t) (some #(= (lc (:table %)) (lc tn)) (:fks t)))
                                                                 (assoc t :sql (rename-fk-parent-col (:sql t) tn old nn)
                                                                        :fks (mapv (fn [fk] (if (= (lc (:table fk)) (lc tn))
                                                                                              (update fk :to (fn [c] (when c (mapv #(if (= (lc %) (lc old)) nn %) c))))
                                                                                              fk))
                                                                                   (:fks t)))
                                                                 t)]) m))))]
        (when-not legacy (post-check db2 "rename"))
        db2)
      :set-notnull
      (let [i (or (find-col tbl (:col st)) (err (str "no such column: \"" (:col st) "\"")))
            c (nth (:cols tbl) i)
            tst (parse-one tsql)
            cd (nth (:cols tst) i)]
        (.append out "# sqlite_fail('constraint failed', 19)\n")
        (if (:notnull c)
          db
          (do (when (some (fn [[_ r]] (nil? (nth r i))) (:rows tbl)) (err "constraint failed"))
              (rebuild-table db tname (str (subs tsql 0 (:e cd)) " NOT NULL" (subs tsql (:e cd))) identity (fn [ix] (:sql ix))))))
      :drop-notnull
      (let [i (or (find-col tbl (:col st)) (err (str "no such column: \"" (:col st) "\"")))
            c (nth (:cols tbl) i)]
        (if-not (:notnull c)
          db
          (let [tst (parse-one tsql)
                cd (nth (:cols tst) i)
                toks (vec (filter #(and (>= (:s %) (:s cd)) (< (:s %) (:e cd))) (lx/tokenize tsql)))
                k (first (keep-indexed (fn [j t] (when (and (kw-tok? t "NOT") (< (inc j) (count toks)) (kw-tok? (nth toks (inc j)) "NULL")) j)) toks))
                named (and (>= k 2) (kw-tok? (nth toks (- k 2)) "CONSTRAINT"))
                from-tok (if named (nth toks (- k 2)) (nth toks k))
                prev-end (:e (nth toks (dec (if named (- k 2) k))))
                ext (let [e (+ k 2)]
                      (if (and (< (inc e) (count toks)) (kw-tok? (nth toks e) "ON") (kw-tok? (nth toks (inc e)) "CONFLICT")) (+ e 3) e))
                end (:e (nth toks (dec ext)))]
            (rebuild-table db tname (str (subs tsql 0 prev-end) (subs tsql end)) identity (fn [ix] (:sql ix))))))
      :add-constraint
      (let [toks (lx/tokenize tsql)
            _ (when (and (:cname st) (find-constraint toks (:cname st)))
                (.append out (str "# sqlite_fail('constraint " (:cname st) " already exists', 1)\n"))
                (err (str "constraint " (:cname st) " already exists")))
            _ (.append out (str "# sqlite_fail('constraint " (:cname st) " already exists', 1)\n"))
            close (let [depth (volatile! 0) last-close (volatile! nil)]
                    (doseq [t toks]
                      (when (= :op (:t t))
                        (cond (= "(" (:v t)) (vswap! depth inc)
                              (= ")" (:v t)) (do (vswap! depth dec) (when (zero? @depth) (vreset! last-close (:s t)))))))
                    (or @last-close (str/last-index-of tsql ")")))
            new-sql (str (subs tsql 0 close) ", " (subs sql (:text-s st) (:text-e st)) (subs tsql close))
            db2 (rebuild-table db tname new-sql identity (fn [ix] (:sql ix)))
            nt (get-in db2 [:tables tname])
            ck (last (:checks nt))]
        (doseq [[_ r] (:rows nt)]
          (let [x ((:f ck) r)]
            (when (and (not (nil? x)) (not (v/bool-value x)))
              (err (str "CHECK constraint failed: " (or (:name ck) (:text ck)))))))
        db2)
      :drop-constraint
      (let [toks (vec (lx/tokenize tsql))
            i (or (find-constraint toks (:cname st)) (err (str "no such constraint: " (:cname st))))
            k (+ i 2)
            ext (constraint-extent toks k)
            _ (when (= ext :undroppable) (err (str "constraint may not be dropped: " (:cname st))))
            s0 (:s (nth toks i))
            [from to] (if (= ext :name-only)
                        [s0 (:s (nth toks k))]
                        (let [e (:e (nth toks (dec ext)))
                              nt (nth toks ext)
                              pt (nth toks (dec i))]
                          (cond
                            (and (or (tok-op? nt ")") (tok-op? nt ",")) (tok-op? pt ","))
                            (if (tok-op? nt ")") [(:s pt) (:s nt)]
                                [s0 (:s (nth toks (inc ext)))])
                            (or (tok-op? nt ")") (tok-op? nt ",")) [(:e pt) e]
                            :else [s0 (:s nt)])))]
        (rebuild-table db tname (str (subs tsql 0 from) (subs tsql to)) identity (fn [ix] (:sql ix))))
      :drop-col
      (let [i (find-col tbl (:col st))
            _ (when-not i (err (str "no such column: \"" (:col st) "\"")))
            c (nth (:cols tbl) i)
            _ (when (= 1 (count (:cols tbl))) (err (str "cannot drop column \"" (:name c) "\": no other columns exist")))
            _ (when (or (:pk c) (some #(= % i) (:pk-cols tbl))) (err (str "cannot drop PRIMARY KEY column: \"" (:col st) "\"")))
            _ (when (seq (:unique c))
                (err (str "cannot drop UNIQUE column: \"" (:col st) "\"")))
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
        (let [db2 (try
                    (rebuild-table db tname new-sql
                                   (fn [r] (let [rid (peek r)] (conj (into (subvec r 0 i) (subvec r (inc i) (dec (count r)))) rid)))
                                   (fn [ix] (:sql ix)))
                    (catch clojure.lang.ExceptionInfo ex
                      (if-let [m (:sql-error (ex-data ex))]
                        (err (str "error in table " (:name tbl) " after drop column: " m))
                        (throw ex))))]
          (post-check db2 "drop column")
          db2)))))

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
  (assoc snap :tx (:tx db) :changes (:changes db) :total-changes (:total-changes db) :last-rowid (:last-rowid db)
         :pragmas (merge (:pragmas db) (select-keys (:pragmas snap) ["user_version" "application_id"])) :temp-used (:temp-used db)))

(defn- exec-tx [db st]
  (let [tx (:tx db)]
    (case (:kind st)
      :begin (if tx (err "cannot start a transaction within a transaction")
                 (assoc db :tx {:snap (snapshot db) :sp [] :by-sp false}))
      :commit (if tx (do (fk/commit-check db) (dissoc db :tx)) (err "cannot commit - no transaction is active"))
      :rollback
      (cond
        (nil? tx) (err (if (:to st) (str "no such savepoint: " (:to st)) "cannot rollback - no transaction is active"))
        (:to st)
        (let [n (lc (:to st))
              sp (:sp tx)
              i (last (keep-indexed (fn [i [nm]] (when (= nm n) i)) sp))]
          (if (nil? i) (err (str "no such savepoint: " (:to st)))
              (let [[_ snap] (nth sp i)]
                (assoc (if (no-journal? db) db (restore db snap)) :tx (assoc tx :sp (subvec sp 0 (inc i)))))))
        ;; journal_mode=OFF: there is no rollback journal, so ROLLBACK [TO] undoes nothing
        (no-journal? db) (dissoc db :tx)
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

(declare exec-statement*)

(defn exec-statement
  "Executes one parsed statement. Writes its output into out. Returns the new db. Throws on error
  (after any rows already written)."
  [db sql st ^StringBuilder out]
  (let [n0 (.length out)] (exec-statement* db sql st out n0)))

(defn- exec-statement* [db sql st ^StringBuilder out n0]
  (when (and (= 1 (get-in db [:pragmas "query_only"]))
             (#{:insert :update :delete :create-table :create-index :create-view :create-trigger :drop :alter :vacuum :reindex :analyze} (:kind st)))
    (err "attempt to write a readonly database"))
  (when (#{:create-table :create-index :create-view :create-trigger} (:kind st)) (sch/check-schema db (:schema st)))
  (when (and (#{:insert :update :delete} (:kind st)) (contains? sch/master-names (lc (:table st))))
    (err (str "table " (:table st) " may not be modified")))
  (when (and (= :drop (:kind st)) (= :table (:what st)) (contains? sch/master-names (lc (:name st))))
    (err (str "table " (:name st) " may not be dropped")))
  (when (and (= :create-index (:kind st)) (contains? sch/master-names (lc (:table st))))
    (err (str "table " (:table st) " may not be indexed")))
  (when (and (#{:create-table :create-view :create-trigger} (:kind st)) (:temp st) (:schema st)
             (not= "temp" (lc (:schema st))))
    (err (str "temporary " (case (:kind st) :create-table "table" :create-view "view" "trigger") " name must be unqualified")))
  (when (= :create-table (:kind st))
    (doseq [d (:cols st)
            k (:cons d) :when (= :default (:c k))]
      (when-not (sch/constant-default? (:e k))
        (err (str "default value of column [" (:name d) "] is not constant")))))
  (let [st (sch/qualify-stmt db (if (:schema st) (assoc st :qual-schema (:schema st)) st))]
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
        (err (if (= "main" lnm) (str "cannot detach database " nm) (str "no such database: " nm))))
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
          db2 (binding [e/*with* (:with st) fk/*child* (atom []) fk/*parent* (atom [])]
               (let [r (f db sql st (fn [r] (.add rows r))
                          (fn [names]
                            (when names (emit-line out (str "# " (str/join "|" names))))
                            (vreset! *prepared* true)))]
                 (when (or (seq @fk/*child*) (seq @fk/*parent*))
                   (try (fk/verify r @fk/*child* @fk/*parent*)
                        (catch clojure.lang.ExceptionInfo ex
                          (throw (ex-info (ex-message ex) (assoc (ex-data ex) :changes-after (if (and (= 1 (long (:changes r))) (empty? @fk/*parent*)) 1 0)))))))
                 r))]
      (cond
        (seq (:returning st)) (doseq [r rows] (write-row out r))
        (and (= 1 (get-in db2 [:pragmas "count_changes"])) (zero? (long trg/*depth*)))
        (do (emit-line out (str "# rows " (case (:kind st) :insert "inserted" :update "updated" "deleted")))
            (emit-line out (str (:changes db2))))
        :else (emit-line out (str "changes: " (:changes db2))))
      (update db2 :total-changes (fnil + 0) (long (or (:changes db2) 0))))
    (let [db2 (case (:kind st)
                :create-table (create-table db sql st)
                :create-index (create-index db sql st)
                :create-trigger (create-trigger db sql st)
                :create-view (let [st (if (= "temp" (lc (:schema st))) (assoc st :temp true) st)
                                   ln0 (lc (:name st))
                                   exv (get-in db [:views ln0])
                                   [db st] (cond
                                             (and (:temp st) exv (not (:temp exv)) (nil? (:db exv)) (not (:db st)))
                                             [(sch/move-main-aside db ln0) st]
                                             (and (not (:temp st)) (nil? (:db st)) exv (:temp exv))
                                             [(assoc db :shadow true) (assoc st :name (str "main." (:name st)) :db "main")]
                                             :else [db st])
                                   ln (lc (:name st))]
                               (cond (get-in db [:indexes ln]) (err (str "there is already an index named " (:name st)))
                                     (or (get-in db [:views ln]) (get-in db [:tables ln]))
                                     (if (:if-not-exists st) db (err (str (if (get-in db [:views ln]) "view " "table ") (or (:name-raw st) (:name st)) " already exists")))
                                     :else (do (when (some #(and (map? %) (:var %)) (tree-seq coll? seq (:select st)))
                                                 (err "parameters are not allowed in views"))
                                               (when-not (or (:temp st) (:db st))
                                                 (let [att (set (sch/attached-names db))
                                                       refs (atom nil)]
                                                   (clojure.walk/postwalk (fn [x] (when (and (map? x) (contains? x :indexed-by) (:table x)
                                                                                             (some-> (:schema x) lc (= "temp")))
                                                                                    (reset! refs "temp"))
                                                                            (when (and (map? x) (contains? x :indexed-by) (:table x)
                                                                                             (string? (:table x)))
                                                                                    (let [k (lc (:table x))
                                                                                          dbn (some (fn [a] (when (str/starts-with? k (str a ".")) a)) att)]
                                                                                      (when dbn (reset! refs dbn))))
                                                                            x)
                                                                          (:select st))
                                                   (when @refs (err (str "view " (:name st) " cannot reference objects in database " @refs)))))
                                               (let [[db v] (sch/register db {:name (:name st) :cols (:cols st) :select (:select st) :src sql
                                                                              :temp (:temp st) :db (:db st) :sql (sch/stmt-sql sql st "VIEW")} false)]
                                                 (assoc-in db [:views ln] v)))))
                :drop (let [ln (lc (:name st))
                            t (get-in db [:tables ln])
                            parent? (and t (= :table (:what st)) (= 1 (get-in db [:pragmas "foreign_keys"]))
                                         (seq (:rows t))
                                         (some (fn [[k ct]] (and (not= k ln)
                                                                 (some #(= (lc (:table %)) (lc (sch/display-name (:name t) t))) (:fks ct))))
                                               (:tables db)))
                            db (if parent?
                                 (let [d (exec-statement db sql {:kind :delete :table ln :where nil :returning nil} (StringBuilder.))]
                                   (assoc d :changes (:changes db)))
                                 db)]
                        (exec-drop db st))
                :reindex (exec-reindex db st)
                :alter (exec-alter db sql st out)
                :analyze (exec-analyze db sql st)
                (:begin :commit :rollback :savepoint :release) (exec-tx db st)
                (:noop :pragma) db
                :vacuum (do (when (:tx db) (err "cannot VACUUM from within a transaction"))
                            (when-let [sc (lc (:schema st))]
                              (when-not (or (#{"main" "temp"} sc) (some #{sc} (sch/attached-names db)))
                                (err (str "unknown database " (:schema st)))))
                            db))]
      (cond
        (= db2 ::empty) db
        (and (#{:create-table :create-index :create-view :create-trigger :drop :alter} (:kind st)) (not (identical? db db2)))
        (do (when-not (and (= :alter (:kind st)) (> (.length out) n0))
              (emit-line out (str "changes: " (:changes db2))))
            (update db2 :schema-version (fnil inc 0)))
        :else (do (when-not (or (readonly? st) (and (= :alter (:kind st)) (> (.length out) n0)))
                    (emit-line out (str "changes: " (:changes db2))))
                  db2))))))

(alter-var-root #'pragma/*fk-check* (constantly fk/check-all))

(reset! trg/exec-fn (fn [db sql st out] (binding [*prepared* (volatile! false)] (exec-statement db sql st out))))

(defn run-line
  "Runs one input line; returns [db' output]."
  [db ^String line]
  (let [out (StringBuilder.)
        ps (p/parse-statements line)]
    (loop [db db]
      (let [st (try (p/next-statement ps)
                    (catch clojure.lang.ExceptionInfo ex
                      (if-let [m (:sql-error (ex-data ex))]
                        (do (emit-line out (str "error: "
                                                (if-let [pc (:pending-create (ex-data ex))]
                                                  (let [nm (:name pc) l (lc nm) sc (some-> (:schema pc) lc)]
                                                    (cond
                                                      (and sc (not (#{"main" "temp"} sc)) (not (some #{sc} (sch/attached-names db))))
                                                      (str "unknown database " (:schema pc))
                                                      (and (or (nil? sc) (= sc "main")) (get-in db [:tables l])) (str "table " (or (:raw pc) nm) " already exists")
                                                      (and (or (nil? sc) (= sc "main")) (get-in db [:views l])) (str "view " (or (:raw pc) nm) " already exists")
                                                      (and (or (nil? sc) (= sc "main")) (get-in db [:indexes l])) (str "there is already an index named " nm)
                                                      :else m))
                                                  m)))
                            ::stop)
                        (throw ex))))]
        (cond
          (= st ::stop) [db (.toString out)]
          (nil? st) [db (.toString out)]
          :else
          (let [prep (volatile! false)
                r (try (binding [*prepared* prep]
                         (vreset! v/enc (case (get-in db [:pragmas "encoding"]) "UTF-16le" :utf16le "UTF-16be" :utf16be :utf8))
                         (vreset! ignore-checks (= 1 (get-in db [:pragmas "ignore_check_constraints"])))
                         (exec-statement db line st out))
                       (catch clojure.lang.ExceptionInfo ex
                         (if-let [m (:sql-error (ex-data ex))]
                           (let [d (ex-data ex)]
                             (emit-line out (str "error: " m))
                             (cond (:partial-db d) [::stop (let [pd (:partial-db d)] (update pd :total-changes (fnil + 0) (long (or (:changes pd) 0))))]
                                   (:changes-after d) [::stop (assoc db :changes (:changes-after d))]
                                   (and (= :rollback (:conflict d)) (:tx db)) [::stop (assoc (dissoc (restore db (:snap (:tx db))) :tx) :changes 0)]
                                   @prep [::stop (assoc db :changes 0)]
                                   :else ::stop))
                           (throw ex)))
                       (catch StackOverflowError _
                         (emit-line out "error: parser stack overflow") ::stop))]
            (cond
              (= r ::stop) [db (.toString out)]
              (vector? r) [(second r) (.toString out)]
              :else (recur r))))))))

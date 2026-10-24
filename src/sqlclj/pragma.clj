(ns sqlclj.pragma
  "PRAGMA statements and the pragma_xxx table-valued functions.
  (run db st) -> {:db db' :names [..] :rows [..] :writes bool}; :names nil means no result set."
  (:require [clojure.string :as str]
            [sqlclj.engine :as e :refer [err lc]]
            [sqlclj.schema :as sch]))

(def compile-options
  ["ATOMIC_INTRINSICS=1" "COMPILER=gcc-13.3.0" "DEFAULT_AUTOVACUUM" "DEFAULT_CACHE_SIZE=-2000"
   "DEFAULT_FILE_FORMAT=4" "DEFAULT_JOURNAL_SIZE_LIMIT=-1" "DEFAULT_MEMSTATUS=0" "DEFAULT_MMAP_SIZE=0"
   "DEFAULT_PAGE_SIZE=4096" "DEFAULT_PCACHE_INITSZ=20" "DEFAULT_RECURSIVE_TRIGGERS" "DEFAULT_SECTOR_SIZE=4096"
   "DEFAULT_SYNCHRONOUS=2" "DEFAULT_WAL_AUTOCHECKPOINT=1000" "DEFAULT_WAL_SYNCHRONOUS=2"
   "DEFAULT_WORKER_THREADS=0" "DIRECT_OVERFLOW_READ" "DQS=0" "MALLOC_SOFT_LIMIT=1024" "MAX_ATTACHED=10"
   "MAX_COLUMN=2000" "MAX_COMPOUND_SELECT=500" "MAX_DEFAULT_PAGE_SIZE=8192" "MAX_EXPR_DEPTH=1000"
   "MAX_FUNCTION_ARG=1000" "MAX_LENGTH=1000000000" "MAX_LIKE_PATTERN_LENGTH=50000" "MAX_MMAP_SIZE=0"
   "MAX_PAGE_COUNT=0xfffffffe" "MAX_PAGE_SIZE=65536" "MAX_SQL_LENGTH=1000000000" "MAX_TRIGGER_DEPTH=1000"
   "MAX_VARIABLE_NUMBER=32766" "MAX_VDBE_OP=250000000" "MAX_WORKER_THREADS=0" "MUTEX_OMIT" "OMIT_AUTOINIT"
   "OMIT_DEPRECATED" "OMIT_LOAD_EXTENSION" "OMIT_PROGRESS_CALLBACK" "OMIT_SHARED_CACHE" "OMIT_WAL"
   "SYSTEM_MALLOC" "TEMP_STORE=3" "THREADSAFE=0"])

(defn- int-value
  "PRAGMA argument as an integer (sqlite3Atoi-like), nil if not numeric."
  [^String v]
  (when v
    (let [m (re-find #"^[+-]?\d+" (str/trim v))]
      (when m (try (Long/parseLong (str/replace m "+" "")) (catch Exception _ nil))))))

(defn- bool-value
  "sqlite3GetBoolean: on/yes/true/full -> 1, off/no/false/none -> 0, numbers."
  [^String v dflt]
  (let [u (str/lower-case (or v ""))]
    (cond (#{"on" "yes" "true"} u) 1
          (#{"off" "no" "false"} u) 0
          :else (if-let [n (int-value v)] (if (zero? n) 0 1) dflt))))

;; flag pragmas: name -> default
(def flag-pragmas
  {"foreign_keys" 0 "recursive_triggers" 0 "reverse_unordered_selects" 0 "automatic_index" 1
   "legacy_alter_table" 0 "trusted_schema" 1 "writable_schema" 0 "count_changes" 0
   "defer_foreign_keys" 0 "query_only" 0 "ignore_check_constraints" 0 "cell_size_check" 0
   "full_column_names" 0 "short_column_names" 1 "empty_result_callbacks" 0 "legacy_file_format" 0
   "read_uncommitted" 0 "checkpoint_fullfsync" 0 "fullfsync" 0 "case_sensitive_like" 0})

;; integer pragmas that return their (new) value when set; name -> [default column-name]
(def int-pragmas-echo
  {"busy_timeout" [0 "timeout"] "mmap_size" [0 "mmap_size"] "journal_size_limit" [-1 "journal_size_limit"]
   "soft_heap_limit" [0 "soft_heap_limit"]
   "hard_heap_limit" [0 "hard_heap_limit"] "threads" [0 "threads"] "analysis_limit" [0 "analysis_limit"]
   "max_page_count" [4294967294 "max_page_count"]})

;; integer pragmas silent when set
(def int-pragmas-quiet
  {"cache_size" -2000 "page_size" 4096 "secure_delete" 0 "cache_spill" 100})

(def header-pragmas #{"user_version" "application_id" "schema_version"})

(defn- pget [db k dflt] (get-in db [:pragmas k] dflt))
(defn- pset [db k v] (assoc-in db [:pragmas k] v))

(defn- one [db name v] {:db db :names [name] :rows [[v]]})
(defn- none [db] {:db db :names nil :rows []})

(defn- find-table [db tname]
  (get-in db [:tables (lc tname)]))

(defn table-info-rows
  "Rows of table_info (xinfo? adds the hidden column)."
  [db tname xinfo?]
  (if-let [t (find-table db tname)]
    (let [pks (vec (:pk-cols t))]
      (vec (keep-indexed
            (fn [i c]
              (let [pkpos (let [j (.indexOf ^java.util.List pks i)] (if (neg? j) 0 (inc j)))
                    hidden (long (or (:hidden-kind c) 0))
                    cid (if xinfo? i (count (filter #(zero? (long (or (:hidden-kind %) 0))) (take i (:cols t)))))]
                (when (or xinfo? (zero? hidden))
                  (cond-> [(long cid) (:name c) (or (:type c) "") (if (:notnull c) 1 0)
                           (:default-text c) (long pkpos)]
                    xinfo? (conj hidden)))))
            (:cols t))))
    (if-let [v (get-in db [:views (lc tname)])]
      (let [ctx (assoc (e/->Ctx db "" nil (atom 0) nil nil nil) :refs (atom #{}))
            sub (try (e/compile-select ctx (:select v)) (catch Exception _ nil))
            names (or (:cols v) (:names sub))]
        (vec (map-indexed (fn [i n] (cond-> [(long i) n "" 0 nil 0] xinfo? (conj 0))) names)))
      [])))

(defn- table-indexes [db lname]
  (->> (vals (:indexes db)) (filter #(= (:table %) lname)) (sort-by :seq >)))

(defn index-list-rows [db tname]
  (if-let [t (find-table db tname)]
    (vec (map-indexed (fn [i idx]
                        [(long i) (:name idx) (if (:unique idx) 1 0)
                         (cond (not (:auto idx)) "c" (:pk-auto idx) "pk" :else "u")
                         (if (:where idx) 1 0)])
                      (table-indexes db (lc (:name t)))))
    []))

(defn index-info-rows [db iname xinfo?]
  (if-let [idx (get-in db [:indexes (lc iname)])]
    (let [t (get-in db [:tables (:table idx)])
          ncols (count (:cols t))
          keyrows (map-indexed (fn [i ic]
                                 (let [cid (cond (nil? (:col ic)) -2 (= (:col ic) ncols) -1 :else (:col ic))
                                       nm (when (and (:col ic) (< (:col ic) ncols)) (:name (nth (:cols t) (:col ic))))]
                                   (if xinfo?
                                     [(long i) (long cid) nm (if (:desc ic) 1 0) (or (:coll ic) "BINARY") 1]
                                     [(long i) (long cid) nm])))
                               (:cols idx))]
      (vec (if xinfo?
             (concat keyrows [[(long (count (:cols idx))) -1 nil 0 "BINARY" 0]])
             keyrows)))
    []))

(defn- act-name [a]
  (case a :set-null "SET NULL" :set-default "SET DEFAULT" :cascade "CASCADE" :restrict "RESTRICT" "NO ACTION"))

(defn fk-list-rows [db tname]
  (if-let [t (find-table db tname)]
    (let [fks (vec (reverse (:fks t)))]
      (vec (for [[id fk] (map-indexed vector fks)
                 [seq from] (map-indexed vector (:from fk))]
             [(long id) (long seq) (:table fk) from (get (:to fk) seq)
              (act-name (:on-update fk)) (act-name (:on-delete fk)) (or (some-> (:match fk) str/upper-case) "NONE")])))
    []))

(def ^:dynamic *fk-check* nil)

(defn run
  "Executes a PRAGMA. st: {:schema :name :value}."
  [db st]
  (let [n (lc (:name st))
        v (:value st)
        sc (some-> (:schema st) lc)]
    (when (and sc (not (#{"main" "temp"} sc)) (not (some #{sc} (map first (:attached db)))))
      (err (str "unknown database " (:schema st))))
    (cond
      (contains? flag-pragmas n)
      (if (nil? v)
        (if (= n "case_sensitive_like") (none db) (one db n (long (pget db n (flag-pragmas n)))))
        (let [b (bool-value v (pget db n (flag-pragmas n)))]
          (if (and (= n "foreign_keys") (:tx db))
            (none db)
            (none (pset db n b)))))

      (contains? int-pragmas-echo n)
      (let [[d col] (int-pragmas-echo n)]
        (if (nil? v)
          (one db col (long (pget db n d)))
          (let [x (or (int-value v) (pget db n d))
                db (pset db n x)]
            (one db col (long x)))))

      (contains? int-pragmas-quiet n)
      (if (nil? v)
        (one db n (long (pget db n (int-pragmas-quiet n))))
        (let [x (int-value v)]
          (if (nil? x) (none db)
              (if (= n "page_size")
                (if (and (seq (:tables db)) (not (empty? (get-in db [:pages "main"]))))
                  (none db)
                  (let [ok (and (>= x 512) (<= x 65536) (zero? (bit-and x (dec x))))]
                    (none (if ok (pset db n x) db))))
                (none (pset db n x))))))

      (contains? header-pragmas n)
      (if (nil? v)
        (one db n (long (if (= n "schema_version") (or (:schema-version db) 0) (pget db n 0))))
        (let [x (or (int-value v) 0)]
          {:db (if (= n "schema_version") (assoc db :schema-version x) (pset db n x)) :names nil :rows [] :writes true}))

      :else
      (case n
        "journal_mode"
        (let [k (str "journal_mode." (or sc "main"))
              cur (pget db k "memory")
              u (some-> v lc)
              nv (cond (nil? u) cur (#{"off" "memory"} u) u :else cur)]
          (one (pset db k nv) "journal_mode" nv))
        "locking_mode"
        (let [cur (pget db "locking_mode" "normal")
              u (some-> v lc)
              nv (if (#{"normal" "exclusive"} u) u cur)]
          (one (pset db "locking_mode" nv) "locking_mode" nv))
        "synchronous"
        (if (nil? v) (one db n (long (pget db n 2)))
            (let [u (lc v)
                  x (case u "off" 0 "normal" 1 "full" 2 "extra" 3 (or (int-value v) 0))]
              (none (pset db n x))))
        "temp_store"
        (if (nil? v) (one db n (long (pget db n 0)))
            (let [u (lc v)
                  x (case u "default" 0 "file" 1 "memory" 2 (or (int-value v) 0))]
              (none (pset db n x))))
        "auto_vacuum"
        (if (nil? v) (one db n (long (pget db n 0)))
            (let [u (lc v)
                  x (case u "none" 0 "full" 1 "incremental" 2 (or (int-value v) 0))]
              (none (if (empty? (get-in db [:pages "main"])) (pset db n x) db))))
        "encoding" (if (nil? v) (one db n "UTF-8") (none db))
        "integrity_check" (if (and v (not (int-value v)) (not (find-table db v)))
                            (err (str "no such table: " v))
                            (one db n "ok"))
        "quick_check" (if (and v (not (int-value v)) (not (find-table db v)))
                        (err (str "no such table: " v))
                        (one db n "ok"))
        "page_count" (one db n (long (if (or (seq (get-in db [:pages (or sc "main")])) (:schema-version db))
                                       (inc (count (get-in db [:pages (or sc "main")]))) 0)))
        "freelist_count" (one db n 0)
        "data_version" (one db n 1)
        "incremental_vacuum" {:db db :names nil :rows [] :writes true}
        "compile_options" {:db db :names ["compile_options"] :rows (mapv vector compile-options)}
        "collation_list" {:db db :names ["seq" "name"] :rows [[0 "RTRIM"] [1 "NOCASE"] [2 "BINARY"]]}
        "database_list" {:db db :names ["seq" "name" "file"]
                         :rows (vec (concat [[0 "main" ""] [1 "temp" ""]]
                                            (map-indexed (fn [i [k a]] [(long (+ 2 i)) k (or (:file a) "")]) (:attached db))))}
        "table_info" (if v {:db db :names ["cid" "name" "type" "notnull" "dflt_value" "pk"] :rows (table-info-rows db v false)} (none db))
        "table_xinfo" (if v {:db db :names ["cid" "name" "type" "notnull" "dflt_value" "pk" "hidden"] :rows (table-info-rows db v true)} (none db))
        "index_list" (if v {:db db :names ["seq" "name" "unique" "origin" "partial"] :rows (index-list-rows db v)} (none db))
        "index_info" (if v {:db db :names ["seqno" "cid" "name"] :rows (index-info-rows db v false)} (none db))
        "index_xinfo" (if v {:db db :names ["seqno" "cid" "name" "desc" "coll" "key"] :rows (index-info-rows db v true)} (none db))
        "foreign_key_list" (if v {:db db :names ["id" "seq" "table" "from" "to" "on_update" "on_delete" "match"] :rows (fk-list-rows db v)} (none db))
        "foreign_key_check"
        (do (when (and v (not (find-table db v))) (err (str "no such table: " v)))
            {:db db :names ["table" "rowid" "parent" "fkid"] :rows (if *fk-check* (vec (*fk-check* db v)) [])})
        "table_list"
        {:db db :names ["schema" "name" "type" "ncol" "wr" "strict"]
         :rows (vec (concat
                     (for [t (sort-by :mseq (vals (:tables db))) :when (not (:temp t))]
                       ["main" (:name t) "table" (long (count (:cols t))) (if (:without-rowid t) 1 0) (if (:strict t) 1 0)])
                     (for [vw (sort-by :mseq (vals (:views db))) :when (not (:temp vw))]
                       ["main" (:name vw) "view" (long (count (:cols vw))) 0 0])
                     [["main" "sqlite_schema" "table" 5 0 0]]
                     (for [t (sort-by :mseq (vals (:tables db))) :when (:temp t)]
                       ["temp" (:name t) "table" (long (count (:cols t))) (if (:without-rowid t) 1 0) (if (:strict t) 1 0)])
                     [["temp" "sqlite_temp_schema" "table" 5 0 0]]))}
        (none db)))))

;; pragma_xxx table-valued functions
(def tvf-cols
  {"table_info" ["cid" "name" "type" "notnull" "dflt_value" "pk"]
   "table_xinfo" ["cid" "name" "type" "notnull" "dflt_value" "pk" "hidden"]
   "index_list" ["seq" "name" "unique" "origin" "partial"]
   "index_info" ["seqno" "cid" "name"]
   "index_xinfo" ["seqno" "cid" "name" "desc" "coll" "key"]
   "foreign_key_list" ["id" "seq" "table" "from" "to" "on_update" "on_delete" "match"]
   "foreign_key_check" ["table" "rowid" "parent" "fkid"]
   "database_list" ["seq" "name" "file"]
   "collation_list" ["seq" "name"]
   "compile_options" ["compile_options"]
   "table_list" ["schema" "name" "type" "ncol" "wr" "strict"]})

(defn- tvf-resolver [lname db]
  (when (str/starts-with? lname "pragma_")
    (let [n (subs lname 7)
          cols (or (tvf-cols n)
                   (when (or (flag-pragmas n) (int-pragmas-echo n) (int-pragmas-quiet n) (header-pragmas n)
                             (#{"journal_mode" "encoding" "integrity_check" "quick_check" "page_count"
                                "freelist_count" "synchronous" "temp_store" "auto_vacuum" "locking_mode" "data_version"} n))
                     [(if-let [[_ c] (int-pragmas-echo n)] c n)]))
          takes-arg (contains? #{"table_info" "table_xinfo" "index_list" "index_info" "index_xinfo"
                                 "foreign_key_list" "foreign_key_check" "integrity_check" "quick_check"} n)]
      (when cols
        {:cols cols :hidden (if takes-arg ["arg" "schema"] ["schema"])
         :rows (fn [db args]
                 (let [[a s] (if takes-arg args [nil (first args)])
                       a (when (some? a) (str a))]
                   (:rows (run db {:name n :value a :schema nil :tvf true}))))}))))

(swap! e/tvf-resolvers (fn [rs] (if (some #(= % #'tvf-resolver) rs) rs (conj rs #'tvf-resolver))))

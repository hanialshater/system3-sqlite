(ns sqlclj.pragma
  "PRAGMA statements and the pragma_xxx table-valued functions.
  (run db st) -> {:db db' :names [..] :rows [..] :writes bool}; :names nil means no result set."
  (:require [clojure.string :as str]
            [sqlclj.engine :as e :refer [err lc]]
            [sqlclj.schema :as sch]
            [sqlclj.values :as vv]))

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

(defn- dig? [c] (and c (<= (int \0) (int c) (int \9))))
(defn- hexd? [c] (and c (or (dig? c) (<= (int \a) (int c) (int \f)) (<= (int \A) (int c) (int \F)))))

(defn- get-int32
  "sqlite3GetInt32: a 32-bit integer from the start of z (decimal, or 0x hex), or nil."
  [^String z]
  (let [z (or z "") n (count z)
        at (fn [i] (when (< i n) (.charAt z (int i))))]
    (cond
      (and (= (at 0) \0) (#{\x \X} (at 1)) (hexd? (at 2)))
      (let [i (loop [i 2] (if (= (at i) \0) (recur (inc i)) i))
            k (loop [k i] (if (and (< (- k i) 8) (hexd? (at k))) (recur (inc k)) k))
            u (if (> k i) (Long/parseLong (subs z i k) 16) 0)]
        (when (and (zero? (bit-and u 0x80000000)) (not (hexd? (at k))))
          (long (unchecked-int u))))
      :else
      (let [neg (= (at 0) \-)
            i0 (if (or neg (= (at 0) \+)) 1 0)]
        (when (dig? (at i0))
          (let [i1 (loop [i i0] (if (= (at i) \0) (recur (inc i)) i))
                i2 (loop [i i1] (if (and (< (- i i1) 11) (dig? (at i))) (recur (inc i)) i))
                v (if (> i2 i1) (Long/parseLong (subs z i1 i2)) 0)]
            (when (and (<= (- i2 i1) 10) (<= (- v (if neg 1 0)) 2147483647))
              (if neg (- v) v))))))))

(defn- atoi [z] (or (get-int32 z) 0))

(defn- dec-or-hex-i64
  "sqlite3DecOrHexToI64: [ok? value]."
  [^String z]
  (let [z (or z "") n (count z)
        at (fn [i] (when (< i n) (.charAt z (int i))))]
    (if (and (= (at 0) \0) (#{\x \X} (at 1)))
      (let [i (loop [i 2] (if (= (at i) \0) (recur (inc i)) i))
            k (loop [k i] (if (hexd? (at k)) (recur (inc k)) k))
            u (reduce (fn [^java.math.BigInteger a c] (.add (.multiply a (java.math.BigInteger/valueOf 16)) (java.math.BigInteger/valueOf (Character/digit (char c) 16))))
                      java.math.BigInteger/ZERO (subs z i k))]
        [(and (= k n) (<= (- k i) 16)) (.longValue u)])
      (let [m (count (re-find #"^[+\- \n\t0-9]*" z))
            sub (subs z 0 m)
            t (str/triml sub)
            neg (str/starts-with? t "-")
            t (if (or neg (str/starts-with? t "+")) (subs t 1) t)
            digits (re-find #"^[0-9]*" t)
            rest* (subs t (count digits))
            d (str/replace digits #"^0+" "")
            big (java.math.BigInteger. (if (empty? d) "0" d))
            lim (java.math.BigInteger/valueOf Long/MAX_VALUE)
            over (> (.compareTo big (if neg (.add lim java.math.BigInteger/ONE) lim)) 0)
            val (cond over (if neg Long/MIN_VALUE Long/MAX_VALUE)
                      neg (- (.longValue big))
                      :else (.longValue big))]
        [(and (seq digits) (empty? rest*) (not over)) val]))))

(defn- int-value
  "PRAGMA argument as an integer: sqlite3Atoi; nil when absent."
  [^String v]
  (when v (long (atoi v))))

(defn- safety-level
  "getSafetyLevel for the boolean pragmas."
  [^String z omit-full dflt]
  (if (dig? (first z))
    (bit-and (long (atoi z)) 0xff)
    (case (lc z) "on" 1 "no" 0 "off" 0 "false" 0 "yes" 1 "true" 1
      "extra" (if omit-full dflt 3) "full" (if omit-full dflt 2) dflt)))

(defn- bool-value
  "sqlite3GetBoolean."
  [^String v dflt]
  (if (zero? (long (safety-level (or v "") true dflt))) 0 1))

(defn- skey
  "Key of a per-schema pragma value."
  [n sc]
  (if (and sc (not= sc "main")) (str n "." sc) n))

;; flag pragmas: name -> default
(def flag-pragmas
  {"foreign_keys" 0 "recursive_triggers" 0 "reverse_unordered_selects" 0 "automatic_index" 1
   "legacy_alter_table" 0 "trusted_schema" 1 "writable_schema" 0 "count_changes" 0
   "defer_foreign_keys" 0 "query_only" 0 "ignore_check_constraints" 0 "cell_size_check" 0
   "full_column_names" 0 "short_column_names" 1 "empty_result_callbacks" 0
   "read_uncommitted" 0 "checkpoint_fullfsync" 0 "fullfsync" 0 "case_sensitive_like" 0})

;; integer pragmas that return their (new) value when set; name -> [default column-name]
(def int-pragmas-echo
  {"busy_timeout" [0 "timeout"] "mmap_size" [0 "mmap_size"] "journal_size_limit" [-1 "journal_size_limit"]
   "soft_heap_limit" [0 "soft_heap_limit"]
   "hard_heap_limit" [0 "hard_heap_limit"] "threads" [0 "threads"] "analysis_limit" [0 "analysis_limit"]
   "max_page_count" [4294967294 "max_page_count"] "secure_delete" [0 "secure_delete"]})

(defn- page-count [db sc]
  (long (if (or (seq (get-in db [:pages (or sc "main")])) (and (= (or sc "main") "main") (:schema-version db)))
          (inc (count (get-in db [:pages (or sc "main")]))) 0)))

;; integer pragmas silent when set
(def int-pragmas-quiet
  {"cache_size" -2000 "page_size" 4096 "cache_spill" 100})

(def header-pragmas #{"user_version" "application_id" "schema_version"})

(defn- pget [db k dflt] (get-in db [:pragmas k] dflt))
(defn- pset [db k v] (assoc-in db [:pragmas k] v))

(defn- one [db name v] {:db db :names [name] :rows [[v]]})
(defn- none [db] {:db db :names nil :rows []})

(defn- find-table [db tname]
  (get-in db [:tables (lc tname)]))

(defn- std-type [t]
  (let [t (or t "")
        u (.toUpperCase ^String t)]
    (if (contains? #{"ANY" "BLOB" "INT" "INTEGER" "REAL" "TEXT"} u) u t)))

(defn table-info-rows
  "Rows of table_info (xinfo? adds the hidden column)."
  ([db tname xinfo?] (table-info-rows db tname xinfo? nil))
  ([db tname xinfo? sc]
  (if-let [t (let [t (find-table db tname)]
               (if (and t sc (or (and (= sc "temp") (not (:temp t))) (and (= sc "main") (:temp t)))) nil
                   (or t (sch/master-table db (lc tname) sc))))]
    (let [pks (vec (:pk-cols t))]
      (vec (keep-indexed
            (fn [i c]
              (let [pkpos (let [j (.indexOf ^java.util.List pks i)] (if (neg? j) 0 (inc j)))
                    hidden (long (or (:hidden-kind c) 0))
                    cid (if xinfo? i (count (filter #(zero? (long (or (:hidden-kind %) 0))) (take i (:cols t)))))]
                (when (or xinfo? (zero? hidden))
                  (cond-> [(long cid) (:name c) (std-type (:type c)) (if (or (:notnull c) (and (:without-rowid t) (pos? pkpos))) 1 0)
                           (:default-text c) (long pkpos)]
                    xinfo? (conj hidden)))))
            (:cols t))))
    (if-let [v (get-in db [:views (lc tname)])]
      (let [ctx (assoc (e/->Ctx db (or (:src v) "") nil (atom 0) nil nil nil) :refs (atom #{}) :in-view (or (:db v) (if (:temp v) "temp" "main")))
            sub (try (e/compile-select (assoc ctx :decl-names true) (:select v)) (catch Exception _ nil))
            names (e/dedup-names (or (:cols v) (:names sub)))
            affs (or (:src-affs sub) (:affs sub))
            tname (fn [i] (case (nth affs i nil) :integer "INT" :real "REAL" :text "TEXT" :numeric "NUM" :blob "BLOB" ""))]
        (vec (map-indexed (fn [i n] (cond-> [(long i) n (tname i) 0 nil 0] xinfo? (conj 0))) names)))
      []))))

(defn- table-indexes [db lname]
  (->> (vals (:indexes db)) (filter #(= (:table %) lname)) (sort-by :seq >)))

(defn index-list-rows [db tname]
  (if-let [t (find-table db tname)]
    (vec (map-indexed (fn [i idx]
                        [(long i) (sch/display-name (:name idx) idx) (if (:unique idx) 1 0)
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
                                     [(long i) (long cid) nm (if (:desc ic) 1 0) (or (:coll-text ic) (:coll ic) "BINARY") 1]
                                     [(long i) (long cid) nm])))
                               (:cols idx))]
      (vec (if xinfo?
             (if (and (:without-rowid t) (:pk-auto idx)) keyrows (concat keyrows [[(long (count (:cols idx))) -1 nil 0 "BINARY" 0]]))
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
        sc (some-> (:schema st) lc)
        db (if (= sc "temp") (assoc db :temp-used true) db)]
    (when (and sc (not (#{"main" "temp"} sc)) (not (some #{sc} (map first (:attached db)))))
      (err (str "unknown database " (:schema st))))
    (let [v (if (and sc v (string? v))
              (cond
                (#{"table_info" "table_xinfo" "index_list" "foreign_key_list"} n)
                (case sc
                  "main" (sch/resolve-key db v "main")
                  "temp" (if (:temp (get-in db [:tables (lc v)])) v "\u0000none")
                  (sch/obj-key db v sc))
                (#{"index_info" "index_xinfo"} n)
                (if (#{"main" "temp"} sc) v (sch/obj-key db v sc))
                :else v)
              v)]
    (cond
      (contains? flag-pragmas n)
      (if (nil? v)
        (if (= n "case_sensitive_like") (none db) (one db n (long (pget db n (flag-pragmas n)))))
        (let [b (bool-value v 0)]
          (if (and (= n "foreign_keys") (:tx db))
            (none db)
            (none (pset db n b)))))

      (= n "cache_spill")
      ;; pcache: pages = cache_size, or -1024*cache_size/(page_size+136) when negative; the spill size is
      ;; max(pages, szSpill); 0 while the CacheSpill flag is off
      (let [pages (fn [db x] (let [x (long x)] (if (>= x 0) x (quot (* -1024 x) (+ 136 (long (pget db "page_size" 4096)))))))]
        (if (nil? v)
          (one db n (if (= 0 (pget db "cache_spill.on" 1)) 0
                        (max (long (pages db (pget db "cache_size" -2000))) (long (pget db "cache_spill.sz" 1)))))
          (let [x (int-value v)
                db (if (and x (not (zero? (long x)))) (pset db "cache_spill.sz" (pages db x)) db)]
            (none (pset db "cache_spill.on" (bool-value v (if (and x (zero? (long x))) 0 1)))))))

      (contains? int-pragmas-echo n)
      (let [[d col] (int-pragmas-echo n)
            k (if (= n "max_page_count") (skey n sc) n)
            cur (long (pget db k d))]
        (if (nil? v)
          (one db col cur)
          (let [[ok x] (dec-or-hex-i64 v)
                x (case n
                    "busy_timeout" (let [ms (atoi v)] (if (> ms 0) ms 0))
                    ("mmap_size" "threads") 0
                    "soft_heap_limit" (if (and ok (>= x 0)) x cur)
                    "hard_heap_limit" (if (and ok (> x 0) (or (zero? cur) (> cur x))) x cur)
                    "analysis_limit" (if (and ok (>= x 0)) (bit-and x 0x7fffffff) cur)
                    "journal_size_limit" (let [x (if (< x -1) -1 x)] x)
                    "max_page_count" (let [x (if ok (max 0 (min x 0xfffffffe)) 0)]
                                       (if (pos? x) (max x (page-count db sc)) cur))
                    "secure_delete" (if (= "fast" (lc v)) 2 (bool-value v 0))
                    cur)
                db (pset db k x)]
            (one db col (long x)))))

      (= n "cache_size")
      (let [k (skey n sc)]
        (if (nil? v) (one db n (long (pget db k -2000)))
            (none (pset db k (long (atoi v))))))

      (contains? int-pragmas-quiet n)
      (if (nil? v)
        (one db n (long (pget db n (int-pragmas-quiet n))))
        (let [x (int-value v)
              ;; cache_size reads its value with sqlite3Atoi: no leading integer means 0
              x (or x (when (= n "cache_size") (or (some-> (re-find #"^\s*[-+]?\d+" (str v)) str/trim parse-long) 0)))]
          (if (nil? x) (none db)
              (if (= n "page_size")
                (if (and (seq (:tables db)) (not (empty? (get-in db [:pages "main"]))))
                  (none db)
                  (let [ok (and (>= x 512) (<= x 65536) (zero? (bit-and x (dec x))))]
                    (none (if ok (pset db n x) db))))
                (none (pset db n x))))))

      (contains? header-pragmas n)
      (let [k (skey n sc)
            tmp? (= sc "temp")]
        (if (nil? v)
          (one db n (long (if (= n "schema_version") (if (or tmp? (and sc (not= sc "main"))) (pget db k 0) (or (:schema-version db) 0)) (pget db k 0))))
          (let [x (long (atoi v))]
            {:db (if (and (= n "schema_version") (not tmp?) (or (nil? sc) (= sc "main"))) (assoc db :schema-version x) (pset db k x)) :names nil :rows [] :writes true})))

      :else
      (case n
        "journal_mode"
        (let [k (str "journal_mode." (or sc "main"))
              cur (pget db k "memory")
              u (some-> v lc)
              nv (cond (nil? u) cur (#{"off" "memory"} u) u :else cur)]
          (one (if (and v (nil? sc) (not= nv cur))
                 (reduce (fn [d nm] (pset d (str "journal_mode." nm) nv)) db (concat ["main" "temp"] (map first (:attached db))))
                 (pset db k nv))
               "journal_mode" nv))
        "locking_mode"
        (let [k (str "locking_mode." (or sc "main"))
              cur (pget db k (if (= sc "temp") "exclusive" "normal"))
              u (some-> v lc)
              nv (if (#{"normal" "exclusive"} u) u cur)]
          (cond
            sc (one db "locking_mode" "exclusive")
            (nil? v) (one db "locking_mode" cur)
            (#{"normal" "exclusive"} u) (one (pset db "locking_mode" nv) "locking_mode" "exclusive")
            :else (one db "locking_mode" cur)))
        "module_list" {:db db :names ["name"] :rows []}
        "temp_store_directory" {:db db :names ["temp_store_directory"] :rows []}
        "synchronous"
        (let [k (skey n sc)]
          (if (nil? v) (one db n (long (pget db k (if (= sc "temp") 0 2))))
              (if (:tx db)
                (err "Safety level may not be changed inside a transaction")
                (let [x (if (dig? (first v))
                          (let [l (bit-and (inc (long (atoi v))) 7)] (dec (if (zero? l) 1 l)))
                          (case (lc v) ("off" "no" "false") 0 ("on" "yes" "true") 1 "extra" 3 "full" 2 1))]
                  (if (= sc "temp") (none db) (none (pset db k x)))))))
        "temp_store"
        (if (nil? v) (one db n (long (pget db n 0)))
            (let [c (first v)
                  x (cond (and c (<= (int \0) (int c) (int \2))) (- (int c) (int \0))
                          (= "file" (lc v)) 1 (= "memory" (lc v)) 2 :else 0)]
              (if (and (:tx db) (not= x (long (pget db n 0))))
                (err "temporary storage cannot be changed from within a transaction")
                (none (pset db n x)))))
        "auto_vacuum"
        (if (nil? v) (one db n (long (pget db n 0)))
            (let [u (lc v)
                  x (case u "none" 0 "full" 1 "incremental" 2 (or (int-value v) 0))]
              (let [cur (long (pget db n 0))
                    blocked (and (seq (get-in db [:pages "main"])) (not= (zero? cur) (zero? (long x))))]
                (if blocked
                  (none (pset db "auto_vacuum.next" x))
                  {:db (pset db n x) :names nil :rows [] :writes (not (zero? (long x)))}))))
        "encoding" (let [cur (pget db "encoding" "UTF-8")]
                     (if (nil? v)
                       (one db n cur)
                       (let [fixed (not (and (empty? (:tables db)) (empty? (:views db)) (= cur "UTF-8")))]
                         (if fixed
                           (none db)
                           (let [nm (case (lc (str/replace (str v) #"-" ""))
                                      "utf8" "UTF-8" ("utf16" "utf16le") "UTF-16le" "utf16be" "UTF-16be"
                                      (err (str "unsupported encoding: " v)))]
                             (none (pset db "encoding" nm)))))))
        ("integrity_check" "quick_check")
        (if (and v (not (dig? (first v))) (not (find-table db v)))
          (err (str "no such table: " v))
          (let [limit (if (and v (dig? (first v))) (let [x (atoi v)] (if (pos? x) x 100)) 100)
                only (when (and v (not (dig? (first v)))) (lc v))
                ign (= 1 (pget db "ignore_check_constraints" 0))
                msgs (when-not ign
                       (for [t (sort-by (fn [t] (- (long (or (:mseq t) 0)))) (vals (:tables db)))
                             :when (and (seq (:checks t)) (or (nil? only) (= only (lc (:name t))))
                                        (or (nil? sc) (if (= sc "temp") (:temp t) (and (not (:temp t)) (= (or (:db t) "main") sc)))))
                             [_ r] (:rows t)
                             :when (some (fn [ck] (let [x ((:f ck) r)] (and (not (nil? x)) (not (vv/bool-value x))))) (:checks t))]
                         (str "CHECK constraint failed in " (sch/display-name (:name t) t))))
                msgs (vec (take limit msgs))]
            {:db db :names [n] :rows (if (seq msgs) (mapv vector msgs) [["ok"]])}))
        "page_count" (one db n (long (if (or (seq (get-in db [:pages (or sc "main")])) (:schema-version db))
                                       (inc (count (get-in db [:pages (or sc "main")]))) 0)))
        "freelist_count" (one db n 0)
        "data_version" (one db n 1)
        "incremental_vacuum" {:db db :names nil :rows [] :writes true}
        "optimize" {:db db :names ["optimize"] :rows []}
        "compile_options" {:db db :names ["compile_options"] :rows (mapv vector compile-options)}
        "pragma_list" {:db db :names ["name"] :rows (mapv vector ["analysis_limit" "application_id" "auto_vacuum" "automatic_index" "busy_timeout" "cache_size" "cache_spill" "case_sensitive_like" "cell_size_check" "checkpoint_fullfsync" "collation_list" "compile_options" "count_changes" "data_version" "database_list" "defer_foreign_keys" "empty_result_callbacks" "encoding" "foreign_key_check" "foreign_key_list" "foreign_keys" "freelist_count" "full_column_names" "fullfsync" "function_list" "hard_heap_limit" "ignore_check_constraints" "incremental_vacuum" "index_info" "index_list" "index_xinfo" "integrity_check" "journal_mode" "journal_size_limit" "legacy_alter_table" "locking_mode" "max_page_count" "mmap_size" "module_list" "optimize" "page_count" "page_size" "pragma_list" "query_only" "quick_check" "read_uncommitted" "recursive_triggers" "reverse_unordered_selects" "schema_version" "secure_delete" "short_column_names" "shrink_memory" "soft_heap_limit" "synchronous" "table_info" "table_list" "table_xinfo" "temp_store" "temp_store_directory" "threads" "trusted_schema" "user_version" "writable_schema"])}
        "collation_list" {:db db :names ["seq" "name"] :rows [[0 "RTRIM"] [1 "NOCASE"] [2 "BINARY"]]}
        "database_list" {:db db :names ["seq" "name" "file"]
                         :rows (vec (concat [[0 "main" ""]]
                                            (when (or (:temp-used db) (some :temp (concat (vals (:tables db)) (vals (:views db)) (vals (:triggers db)))))
                                              [[1 "temp" ""]])
                                            (map-indexed (fn [i [k a]] [(long (+ 2 i)) (or (:name a) k) (let [f (or (:file a) "")]
                                                                                                                                                (if (or (= f "") (str/starts-with? f "/"))
                                                                                                                                                  f
                                                                                                                                                  (str "/home/user/system-3-company/company/sibling/system3-meta/prototype/bench12/" f)))]) (:attached db))))}
        "table_info" {:db db :names ["cid" "name" "type" "notnull" "dflt_value" "pk"] :rows (if v (table-info-rows db v false sc) [])}
        "table_xinfo" {:db db :names ["cid" "name" "type" "notnull" "dflt_value" "pk" "hidden"] :rows (if v (table-info-rows db v true sc) [])}
        "index_list" {:db db :names ["seq" "name" "unique" "origin" "partial"] :rows (if v (index-list-rows db v) [])}
        "index_info" {:db db :names ["seqno" "cid" "name"] :rows (if v (index-info-rows db v false) [])}
        "index_xinfo" {:db db :names ["seqno" "cid" "name" "desc" "coll" "key"] :rows (if v (index-info-rows db v true) [])}
        "foreign_key_list" {:db db :names ["id" "seq" "table" "from" "to" "on_update" "on_delete" "match"] :rows (if v (fk-list-rows db v) [])}
        "foreign_key_check"
        (do (when (and v (not (find-table db v))) (err (str "no such table: " v)))
            {:db db :names ["table" "rowid" "parent" "fkid"] :rows (if *fk-check* (vec (*fk-check* db v)) [])})
        "table_list"
        (let [obj-db (fn [o] (cond (:temp o) "temp" (:db o) (lc (:db o)) :else "main"))
              row (fn [d kind t]
                    (if (= kind :table)
                      [d (sch/display-name (:name t) t) "table" (long (count (:cols t))) (if (:without-rowid t) 1 0) (if (:strict t) 1 0)]
                      [d (sch/display-name (:name t) t) "view" (long (count (:cols t))) 0 0]))
              objs (concat (map #(vector :table %) (vals (:tables db))) (map #(vector :view %) (vals (:views db))))
              part (fn [d]
                     (if (= d "temp")
                       (concat (for [t (sort-by :mseq (vals (:tables db))) :when (:temp t)] (row "temp" :table t))
                               [["temp" "sqlite_temp_schema" "table" 5 0 0]])
                       (concat (for [[kind t] (sort-by (fn [[_ x]] (- (long (or (:mseq x) 0)))) objs)
                                     :when (= d (obj-db t))]
                                 (row d kind t))
                               [[d "sqlite_schema" "table" 5 0 0]])))]
          {:db db :names ["schema" "name" "type" "ncol" "wr" "strict"]
           :rows (vec (filter (fn [r] (or (nil? v) (= (str/lower-case (str v)) (str/lower-case (nth r 1)))))
                              (mapcat part (if sc [sc] (concat ["main" "temp"] (map first (:attached db)))))))})
        (none db))))))

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
                                 "foreign_key_list" "foreign_key_check" "integrity_check" "quick_check" "table_list"} n)]
      (when cols
        ;; table_list has a real schema column; its hidden one must not hide it from *
        {:cols cols :hidden (if takes-arg ["arg" (if (= n "table_list") "schema " "schema")] ["schema"])
         :rows (fn [db args]
                 (let [[a s] (if takes-arg args [nil (first args)])
                       a (when (some? a) (str a))]
                   (:rows (run db {:name n :value a :schema (when (some? s) (str s)) :tvf true}))))}))))

(swap! e/tvf-resolvers (fn [rs] (if (some #(= % #'tvf-resolver) rs) rs (conj rs #'tvf-resolver))))

(ns sqlclj.schema
  "The schema layer: the stored CREATE text of schema objects, root pages, sqlite_master /
  sqlite_temp_master as readable tables, schema-qualified name lookup.

  DB VALUE CONTRACT (shared by every module):
    :tables   {lc-name tbl}     tbl has :name :cols :rows ... plus :sql :mseq :rootpage :temp
    :indexes  {lc-name idx}     idx has :table (lc) ... plus :sql (nil for autoindexes) :mseq :rootpage :temp
    :views    {lc-name view}    view has :name :cols :select plus :sql :mseq :temp
    :triggers {lc-name trig}    trig has :name :table (lc) plus :sql :mseq :temp  (filled by the trigger code)
    :mseq     counter giving each new schema object its sqlite_master rowid (creation order)
    :pages    {schema #{root pages in use}}  (\"main\" / \"temp\")
    :tx       transaction / savepoint stack (see exec)
    :pragmas  {lc-name value} settable pragma values"
  (:require [clojure.string :as str]))

(def sqlite-keywords
  #{"ABORT" "ACTION" "ADD" "AFTER" "ALL" "ALTER" "ALWAYS" "ANALYZE" "AND" "AS" "ASC" "ATTACH"
    "AUTOINCREMENT" "BEFORE" "BEGIN" "BETWEEN" "BY" "CASCADE" "CASE" "CAST" "CHECK" "COLLATE"
    "COLUMN" "COMMIT" "CONFLICT" "CONSTRAINT" "CREATE" "CROSS" "CURRENT" "CURRENT_DATE"
    "CURRENT_TIME" "CURRENT_TIMESTAMP" "DATABASE" "DEFAULT" "DEFERRABLE" "DEFERRED" "DELETE" "DESC"
    "DETACH" "DISTINCT" "DO" "DROP" "EACH" "ELSE" "END" "ESCAPE" "EXCEPT" "EXCLUDE" "EXCLUSIVE"
    "EXISTS" "EXPLAIN" "FAIL" "FILTER" "FIRST" "FOLLOWING" "FOR" "FOREIGN" "FROM" "FULL"
    "GENERATED" "GLOB" "GROUP" "GROUPS" "HAVING" "IF" "IGNORE" "IMMEDIATE" "IN" "INDEX" "INDEXED"
    "INITIALLY" "INNER" "INSERT" "INSTEAD" "INTERSECT" "INTO" "IS" "ISNULL" "JOIN" "KEY" "LAST"
    "LEFT" "LIKE" "LIMIT" "MATCH" "MATERIALIZED" "NATURAL" "NO" "NOT" "NOTHING" "NOTNULL" "NULL"
    "NULLS" "OF" "OFFSET" "ON" "OR" "ORDER" "OTHERS" "OUTER" "OVER" "PARTITION" "PLAN" "PRAGMA"
    "PRECEDING" "PRIMARY" "QUERY" "RAISE" "RANGE" "RECURSIVE" "REFERENCES" "REGEXP" "REINDEX"
    "RELEASE" "RENAME" "REPLACE" "RESTRICT" "RETURNING" "RIGHT" "ROLLBACK" "ROW" "ROWS"
    "SAVEPOINT" "SELECT" "SET" "TABLE" "TEMP" "TEMPORARY" "THEN" "TIES" "TO" "TRANSACTION"
    "TRIGGER" "UNBOUNDED" "UNION" "UNIQUE" "UPDATE" "USING" "VACUUM" "VALUES" "VIEW" "VIRTUAL"
    "WHEN" "WHERE" "WINDOW" "WITH" "WITHOUT"})

(defn- id-char? [c] (let [c (int c)] (or (>= c 128) (Character/isLetterOrDigit (char c)) (= c (int \_)) (= c (int \$)))))

(defn ident-put
  "SQLite's identPut: the identifier, double-quoted when needed."
  [^String z]
  (let [need (or (zero? (count z))
                 (Character/isDigit (.charAt z 0))
                 (not (every? id-char? z))
                 (contains? sqlite-keywords (str/upper-case z)))]
    (if need (str "\"" (str/replace z "\"" "\"\"") "\"") z)))

(defn- ident-length [^String z] (+ 2 (count z) (count (filter #(= % \") z))))

(defn create-table-stmt
  "The CREATE TABLE text SQLite builds for CREATE TABLE AS SELECT. cols: [{:name :aff}]"
  [tname cols]
  (let [n (+ (reduce + (map #(+ 5 (ident-length (:name %))) cols)) (ident-length tname))
        [sep sep2 end] (if (< n 50) ["" "," ")"] ["\n  " ",\n  " "\n)"])]
    (str "CREATE TABLE " (ident-put tname) "(" sep
         (str/join sep2 (map (fn [c] (str (ident-put (:name c))
                                          (case (:aff c) :text " TEXT" :numeric " NUM" :integer " INT" :real " REAL" "")))
                             cols))
         end)))

(defn stmt-sql
  "The text SQLite stores in sqlite_master for a CREATE statement st parsed from line sql.
  kind: \"TABLE\" \"INDEX\" \"UNIQUE INDEX\" \"VIEW\" \"TRIGGER\"."
  [^String sql st kind]
  (when (and (:name-s st) (:st-e st))
    (str "CREATE " kind " " (subs sql (:name-s st) (:st-e st)))))

(defn schema-of
  "The schema name an object lives in."
  [obj] (or (:db obj) (if (:temp obj) "temp" "main")))

(defn alloc-page
  "Allocates a root page in schema sch; returns [db page]."
  [db sch]
  (let [used (get-in db [:pages sch] #{})
        pg (loop [i 2] (if (contains? used i) (recur (inc i)) i))]
    [(update-in db [:pages sch] (fnil conj #{}) pg) pg]))

(defn free-page [db sch pg]
  (if pg (update-in db [:pages sch] disj pg) db))

(defn register
  "Gives a new schema object its sqlite_master rowid and (for tables and indexes) a root page."
  [db obj needs-page?]
  (let [s (inc (long (or (:mseq db) 0)))
        db (assoc db :mseq s)
        [db pg] (if needs-page? (alloc-page db (schema-of obj)) [db 0])]
    [db (assoc obj :mseq s :rootpage pg)]))

(defn display-name
  "An object's name without the attached-database prefix."
  [^String n obj]
  (if (and n (:db obj) (str/starts-with? (str/lower-case n) (str (:db obj) ".")))
    (subs n (inc (count (:db obj))))
    n))

(defn master-rows
  "Rows of sqlite_master for schema sch (\"main\", \"temp\" or an attached name):
  [type name tbl_name rootpage sql] ordered by creation."
  [db sch]
  (let [dn (fn [n o] (display-name n o))
        objs (concat
              (for [[_ t] (:tables db)] [(:mseq t) ["table" (dn (:name t) t) (dn (:name t) t) (long (or (:rootpage t) 0)) (:sql t)] t])
              (for [[_ i] (:indexes db)]
                [(:mseq i) ["index" (dn (:name i) i) (dn (or (:name (get-in db [:tables (:table i)])) (:table i)) i)
                            (long (or (:rootpage i) 0)) (:sql i)] i])
              (for [[_ v] (:views db)] [(:mseq v) ["view" (dn (:name v) v) (dn (:name v) v) 0 (:sql v)] v])
              (for [[_ g] (:triggers db)] [(:mseq g) ["trigger" (dn (:name g) g) (dn (or (:table-name g) (:name (get-in db [:tables (:table g)])) (:table g)) g) 0 (:sql g)] g]))]
    (->> objs
         (filter (fn [[_ _ o]] (and (= sch (schema-of o)) (not (:no-master o)))))
         (sort-by (fn [[s]] (or s 0)))
         (mapv second))))

(def master-names
  {"sqlite_master" false "sqlite_schema" false "sqlite_temp_master" true "sqlite_temp_schema" true})

(defn master-table
  "A read-only table value for sqlite_master and its aliases, or nil. schema is the qualifier (or nil)."
  [db lname schema]
  (let [lsch (when schema (str/lower-case schema))
        temp? (cond (= lsch "temp") true
                    (contains? master-names lname) (get master-names lname)
                    :else nil)
        sch (cond temp? "temp" (and lsch (not= lsch "main")) lsch :else "main")]
    (when (and (contains? master-names lname) (or (nil? lsch) (#{"main" "temp"} lsch) (some #{lsch} (map first (:attached db)))))
      (let [rows (master-rows db sch)
            cols (mapv (fn [n a] {:name n :type (if (= a :integer) "INT" "TEXT") :aff a})
                       ["type" "name" "tbl_name" "rootpage" "sql"] [:text :text :text :integer :text])]
        {:name (if temp? (if (= lname "sqlite_temp_schema") "sqlite_temp_schema" "sqlite_temp_master")
                   (if (= lname "sqlite_schema") "sqlite_schema" "sqlite_master"))
         :cols cols :ipk nil :strict false :checks [] :virtual true
         :rows (into (sorted-map) (map-indexed (fn [i r] [(long (inc i)) (conj r (long (inc i)))]) rows))}))))

;; ====================================================================== attached databases
;; Objects of an attached database live in the same maps as main's, keyed (and named internally)
;; "<db>.<name>", with :db "<db>". Statements are rewritten (qualify-stmt) before execution so that
;; their table references use those keys. main and temp objects keep plain lower-case keys.

(defn attached-names [db] (map first (:attached db)))

(defn- lc* [s] (when s (str/lower-case s)))

(defn obj-key
  "Storage key of object name in schema sch (an attached schema gives a prefixed key)."
  [db name sch]
  (let [l (lc* name) s (lc* sch)]
    (if (and s (not (#{"main" "temp"} s)))
      (str s "." l)
      l)))

(defn check-schema [db sch]
  (let [s (lc* sch)]
    (when (and s (not (#{"main" "temp"} s)) (not (some #{s} (attached-names db))))
      (throw (ex-info "sql" {:sql-error (str "unknown database " sch)})))))

(defn resolve-key
  "Key of an existing table or view called name, as seen from schema qualifier sch (nil = search order)."
  [db name sch]
  (let [l (lc* name) s (lc* sch)
        exists? (fn [k] (or (get-in db [:tables k]) (get-in db [:views k])))]
    (cond
      (nil? s) (cond (exists? l) l
                     (exists? (str "main." l)) (str "main." l)
                     :else (or (some (fn [a] (let [k (str a "." l)] (when (exists? k) k))) (attached-names db)) l))
      (= "main" s) (if (exists? (str "main." l)) (str "main." l) l)
      (= "temp" s) l
      :else (str s "." l))))

(defn- qualify-from-item [db item]
  (if (and (:table item) (not (:op item)) (some #{(lc* (:schema item))} (attached-names db))
           (not (contains? master-names (lc* (:table item)))))
    (assoc item :table (resolve-key db (:table item) (:schema item)) :schema nil
           :alias (or (:alias item) (:table item)))
    (if (and (:table item) (not (:op item)) (or (nil? (:schema item)) (= "main" (lc* (:schema item))))
             (or (seq (:attached db)) (:shadow db)))
      (let [k (resolve-key db (:table item) nil)]
        (if (= k (lc* (:table item))) item (assoc item :table k :schema nil :alias (or (:alias item) (:table item)))))
      item)))

(defn qualify-stmt
  "Rewrites table references of a statement for attached databases (identity when none attached)."
  [db st]
  (if (and (empty? (:attached db)) (not (:shadow db)) (let [s (lc* (:schema st))] (or (nil? s) (#{"main" "temp"} s))))
    st
    (let [att (set (attached-names db))
          sch (lc* (:schema st))
          walk (fn walk [x]
                 (cond
                   (map? x) (let [x (if (and (= :col (:op x)) (:schema x)) (dissoc x :schema) x)
                                  x (if (and (contains? x :indexed-by) (:table x)) (qualify-from-item db x) x)]
                              (into {} (map (fn [[k v]] [k (walk v)]) x)))
                   (vector? x) (mapv walk x)
                   (seq? x) (doall (map walk x))
                   :else x))]
      (case (:kind st)
        (:create-table :create-view)
        (let [st (walk st)]
          (if (att sch) (assoc st :name (obj-key db (:name st) sch) :db sch :schema nil) st))
        :create-index
        (if (att sch)
          (assoc (walk st) :name (obj-key db (:name st) sch) :table (obj-key db (:table st) sch) :db sch :schema nil)
          (let [st (walk st)] (assoc st :table (resolve-key db (:table st) nil))))
        :drop
        (assoc st :name (cond (and sch (not (#{"main" "temp"} sch))) (obj-key db (:name st) sch)
                              (and (#{:table :view} (:what st)) (not= sch "temp")) (resolve-key db (:name st) sch)
                              :else (:name st)))
        :create-trigger
        (let [tsch (lc* (:table-schema st))
              tk (cond tsch (resolve-key db (:table st) tsch)
                       (att sch) (obj-key db (:table st) sch)
                       :else (resolve-key db (:table st) nil))
              tdb (when (string? tk) (some (fn [a] (when (str/starts-with? tk (str a ".")) a)) att))]
          (cond-> (assoc st :table tk)
            (att sch) (assoc :name (obj-key db (:name st) sch) :db sch :schema nil)
            (and (not (att sch)) (not= sch "temp") (not (:temp st)) tdb) (assoc :db tdb)))
        (:insert :update :delete)
        (let [t (:table st) s (:schema st)
              st (walk st)]
          (assoc st :table (resolve-key db t s) :schema nil))
        (walk st)))))

(defn constant-default?
  "Whether a DEFAULT expression is constant (no column references, bound variables or subqueries)."
  [e]
  (letfn [(walk [x]
            (cond
              (map? x) (cond
                         (and (= :col (:op x)) (not (and (nil? (:table x)) (#{"true" "false"} (lc* (:name x)))))) false
                         (:var x) false
                         (#{:subq :exists :raise} (:op x)) false
                         (and (= :in (:op x)) (or (:select x) (:table x))) false
                         :else (every? walk (vals x)))
              (sequential? x) (every? walk x)
              :else true))]
    (walk e)))

(defn move-main-aside
  "A temp table is being created with the name of main table l: the main table moves to key main.l."
  [db l]
  (let [k (str "main." l)
        t (get-in db [:tables l])]
    (-> db
        (update :tables dissoc l)
        (assoc-in [:tables k] (assoc t :name (str "main." (:name t)) :db "main"))
        (update :indexes (fn [m] (into {} (map (fn [[ik i]] [ik (if (= (:table i) l) (assoc i :table k) i)]) m))))
        (update :triggers (fn [m] (into {} (map (fn [[gk g]] [gk (if (= (:table g) l) (assoc g :table k) g)]) m))))
        (assoc :shadow true))))

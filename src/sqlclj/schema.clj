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
  [obj] (if (:temp obj) "temp" "main"))

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

(defn master-rows
  "Rows of sqlite_master (temp? false) or sqlite_temp_master: [type name tbl_name rootpage sql]
  ordered by creation."
  [db temp?]
  (let [objs (concat
              (for [[_ t] (:tables db)] [(:mseq t) ["table" (:name t) (:name t) (long (or (:rootpage t) 0)) (:sql t)] t])
              (for [[_ i] (:indexes db)]
                [(:mseq i) ["index" (:name i) (:name (get-in db [:tables (:table i)]) (:table i))
                            (long (or (:rootpage i) 0)) (:sql i)] i])
              (for [[_ v] (:views db)] [(:mseq v) ["view" (:name v) (:name v) 0 (:sql v)] v])
              (for [[_ g] (:triggers db)] [(:mseq g) ["trigger" (:name g) (or (:table-name g) (:name (get-in db [:tables (:table g)])) (:table g)) 0 (:sql g)] g]))]
    (->> objs
         (filter (fn [[_ _ o]] (= (boolean temp?) (boolean (:temp o)))))
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
                    :else nil)]
    (when (and (contains? master-names lname) (or (nil? lsch) (#{"main" "temp"} lsch)))
      (let [rows (master-rows db temp?)
            cols (mapv (fn [n a] {:name n :type (if (= a :integer) "INT" "TEXT") :aff a})
                       ["type" "name" "tbl_name" "rootpage" "sql"] [:text :text :text :integer :text])]
        {:name (if temp? (if (= lname "sqlite_temp_schema") "sqlite_temp_schema" "sqlite_temp_master")
                   (if (= lname "sqlite_schema") "sqlite_schema" "sqlite_master"))
         :cols cols :ipk nil :strict false :checks [] :virtual true
         :rows (into (sorted-map) (map-indexed (fn [i r] [(long (inc i)) (conj r (long (inc i)))]) rows))}))))

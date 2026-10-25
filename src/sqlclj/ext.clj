(ns sqlclj.ext
  "Scalar functions beyond the core set: printf/format, date and time, json, misc.
  (compile-fn name nargs coll) returns (fn [vals]) or nil when the function is unknown."
  (:require [clojure.string :as str]
            [sqlclj.values :as v]
            [sqlclj.printf :as pf]
            [sqlclj.datetime :as dt]
            [sqlclj.json :as js])
  (:import [sqlclj.values Blob]))

(def compile-options
  ["ATOMIC_INTRINSICS=1" "COMPILER=gcc-13.3.0" "DEFAULT_AUTOVACUUM" "DEFAULT_CACHE_SIZE=-2000"
   "DEFAULT_FILE_FORMAT=4" "DEFAULT_JOURNAL_SIZE_LIMIT=-1" "DEFAULT_MEMSTATUS=0" "DEFAULT_MMAP_SIZE=0"
   "DEFAULT_PAGE_SIZE=4096" "DEFAULT_PCACHE_INITSZ=20" "DEFAULT_RECURSIVE_TRIGGERS" "DEFAULT_SECTOR_SIZE=4096"
   "DEFAULT_SYNCHRONOUS=2" "DEFAULT_WAL_AUTOCHECKPOINT=1000" "DEFAULT_WAL_SYNCHRONOUS=2" "DEFAULT_WORKER_THREADS=0"
   "DIRECT_OVERFLOW_READ" "DQS=0" "MALLOC_SOFT_LIMIT=1024" "MAX_ATTACHED=10" "MAX_COLUMN=2000"
   "MAX_COMPOUND_SELECT=500" "MAX_DEFAULT_PAGE_SIZE=8192" "MAX_EXPR_DEPTH=1000" "MAX_FUNCTION_ARG=1000"
   "MAX_LENGTH=1000000000" "MAX_LIKE_PATTERN_LENGTH=50000" "MAX_MMAP_SIZE=0" "MAX_PAGE_COUNT=0xfffffffe"
   "MAX_PAGE_SIZE=65536" "MAX_SQL_LENGTH=1000000000" "MAX_TRIGGER_DEPTH=1000" "MAX_VARIABLE_NUMBER=32766"
   "MAX_VDBE_OP=250000000" "MAX_WORKER_THREADS=0" "MUTEX_OMIT" "OMIT_AUTOINIT" "OMIT_DEPRECATED"
   "OMIT_LOAD_EXTENSION" "OMIT_PROGRESS_CALLBACK" "OMIT_SHARED_CACHE" "OMIT_WAL" "SYSTEM_MALLOC" "TEMP_STORE=3"
   "THREADSAFE=0"])

(defn- id-char? [c] (let [c (int c)] (or (and (>= c 48) (<= c 57)) (and (>= c 65) (<= c 90)) (and (>= c 97) (<= c 122))
                                       (= c 95) (= c 36) (>= c 128))))

(defn compile-option-used [^String name]
  (let [name (if (.regionMatches name true 0 "SQLITE_" 0 7) (subs name 7) name)
        n (count name)]
    (if (some (fn [^String o] (and (.regionMatches o true 0 name 0 n) (>= (count o) n)
                                   (or (= (count o) n) (not (id-char? (.charAt o n))))))
              compile-options)
      1 0)))

(def ^:private rng (java.security.SecureRandom.))

(defn- hexval [c] (Character/digit (char c) 16))

(defn- put-utf8 [^java.io.ByteArrayOutputStream bo ^long cp]
  (cond (< cp 0x80) (.write bo (int cp))
        (< cp 0x800) (do (.write bo (int (bit-or 0xC0 (bit-shift-right cp 6)))) (.write bo (int (bit-or 0x80 (bit-and cp 0x3F)))))
        (< cp 0x10000) (do (.write bo (int (bit-or 0xE0 (bit-shift-right cp 12))))
                           (.write bo (int (bit-or 0x80 (bit-and (bit-shift-right cp 6) 0x3F))))
                           (.write bo (int (bit-or 0x80 (bit-and cp 0x3F)))))
        :else (do (.write bo (int (bit-or 0xF0 (bit-and (bit-shift-right cp 18) 0x07))))
                  (.write bo (int (bit-or 0x80 (bit-and (bit-shift-right cp 12) 0x3F))))
                  (.write bo (int (bit-or 0x80 (bit-and (bit-shift-right cp 6) 0x3F))))
                  (.write bo (int (bit-or 0x80 (bit-and cp 0x3F)))))))

(defn unistr [^String s]
  (let [b (v/str->utf8 s) n (alength b) bo (java.io.ByteArrayOutputStream.)
        bad #(v/err "invalid Unicode escape")]
    (loop [i 0]
      (when (< i n)
        (let [c (bit-and (aget b i) 0xff)]
          (if (not= c 92)
            (do (.write bo c) (recur (inc i)))
            (let [hexn (fn [from k]
                         (when (> (+ from k) n) (bad))
                         (loop [j 0 acc 0]
                           (if (= j k) acc
                               (let [h (hexval (bit-and (aget b (+ from j)) 0xff))]
                                 (when (neg? h) (bad))
                                 (recur (inc j) (+ (* acc 16) h))))))]
              (when (>= (inc i) n) (bad))
              (let [d (bit-and (aget b (inc i)) 0xff)]
                (cond
                  (= d 92) (do (.write bo 92) (recur (+ i 2)))
                  (= d (int \u)) (do (put-utf8 bo (hexn (+ i 2) 4)) (recur (+ i 6)))
                  (= d (int \U)) (do (put-utf8 bo (hexn (+ i 2) 8)) (recur (+ i 10)))
                  (= d (int \+)) (do (put-utf8 bo (hexn (+ i 2) 6)) (recur (+ i 8)))
                  :else (do (put-utf8 bo (hexn (inc i) 4)) (recur (+ i 5))))))))))
    (v/utf8->str (.toByteArray bo))))

(defn- cstr
  "text up to the first NUL"
  [^String s] (let [k (.indexOf s (int 0))] (if (neg? k) s (subs s 0 k))))

(defn unistr-quote [x]
  (cond (nil? x) nil
        (v/blob? x) (str "X'" (v/hex-upper (v/blob-bytes x)) "'")
        (or (instance? Long x) (instance? Double x)) (v/text-of x)
        :else
        (let [s (cstr (v/text-of x))
              ctl? (some #(< (int %) 32) s)
              sb (StringBuilder.)]
          (if-not ctl?
            (str "'" (.replace s "'" "''") "'")
            (do (.append sb "unistr('")
                (doseq [c s]
                  (let [ci (int c)]
                    (cond (< ci 32) (.append sb (format "\\u%04x" ci))
                          (= c \\) (.append sb "\\\\")
                          (= c \') (.append sb "''")
                          :else (.append sb c))))
                (.append sb "')")
                (str sb))))))

(defn compile-fn [name n coll]
  (let [ar (fn [ok] (when-not ok (v/err (str "wrong number of arguments to function " name "()"))))]
    (case name
      ("printf" "format")
      (fn [args]
        (when (and (seq args) (not (nil? (first args))))
          (pf/sql-printf (let [t (v/text-of (first args)) k (.indexOf ^String t (int 0))] (if (neg? k) t (subs t 0 k))) (rest args))))
      ("changes" "total_changes" "last_insert_rowid") (do (ar (= n 0)) (fn [_] 0))
      "random" (do (ar (= n 0)) (fn [_] (.nextLong ^java.security.SecureRandom rng)))
      "randomblob" (do (ar (= n 1))
                       (fn [[x]]
                         (let [k (max 1 (v/int-value x))]
                           (when (> k 1000000000) (v/err "string or blob too big"))
                           (let [b (byte-array k)] (.nextBytes ^java.security.SecureRandom rng b) (Blob. b)))))
      "match" (do (ar (= n 2)) (fn [_] (v/err "unable to use function MATCH in the requested context")))
      "sqlite_source_id" (do (ar (= n 0)) (fn [_] "2026-07-24 19:02:57 bf7c7f30031888f4e796e429ab3978879485813aaca6f641c7b33e4e09459bcc"))
      "sqlite_compileoption_used" (do (ar (= n 1)) (fn [[x]] (when-not (nil? x) (compile-option-used (v/text-of x)))))
      "sqlite_compileoption_get" (do (ar (= n 1)) (fn [[x]] (let [i (v/int-value x)] (when (and (>= i 0) (< i (count compile-options))) (nth compile-options i)))))
      "date" (fn [args] (dt/date-fn args))
      "time" (fn [args] (dt/time-fn args))
      "datetime" (fn [args] (dt/datetime-fn args))
      "julianday" (fn [args] (dt/julianday-fn args))
      "unixepoch" (fn [args] (dt/unixepoch-fn args))
      "strftime" (fn [args] (dt/strftime-fn args))
      "timediff" (do (ar (= n 2)) (fn [args] (dt/timediff-fn args)))
      "current_timestamp" (fn [_] (dt/datetime-fn ["now"]))
      "current_date" (fn [_] (dt/date-fn ["now"]))
      "current_time" (fn [_] (dt/time-fn ["now"]))
      "\u0000truth" (fn [[x mode]]
                      (let [m (long mode) false? (odd? m) not? (>= m 2)
                            r (if (nil? x) false (let [t (v/bool-value x)] (if false? (not t) t)))]
                        (if (if not? (not r) r) 1 0)))
      "json" (do (ar (= n 1)) (js/fn-json false))
      "jsonb" (do (ar (= n 1)) (js/fn-json true))
      "json_array" (js/fn-array false)
      "jsonb_array" (js/fn-array true)
      "json_object" (js/fn-object "json_object" false)
      "jsonb_object" (js/fn-object "jsonb_object" true)
      "json_extract" (js/fn-extract "json_extract" false false)
      "jsonb_extract" (js/fn-extract "jsonb_extract" true false)
      "json_set" (do (ar (>= n 1)) (js/fn-edit "json_set" :set false))
      "jsonb_set" (do (ar (>= n 1)) (js/fn-edit "jsonb_set" :set true))
      "json_insert" (do (ar (>= n 1)) (js/fn-edit "json_insert" :insert false))
      "jsonb_insert" (do (ar (>= n 1)) (js/fn-edit "jsonb_insert" :insert true))
      "json_replace" (do (ar (>= n 1)) (js/fn-edit "json_replace" :replace false))
      "jsonb_replace" (do (ar (>= n 1)) (js/fn-edit "jsonb_replace" :replace true))
      "json_array_insert" (do (ar (>= n 1)) (js/fn-edit "json_array_insert" :ainsert false))
      "jsonb_array_insert" (do (ar (>= n 1)) (js/fn-edit "jsonb_array_insert" :ainsert true))
      "json_remove" (do (ar (>= n 1)) (js/fn-remove "json_remove" false))
      "jsonb_remove" (do (ar (>= n 1)) (js/fn-remove "jsonb_remove" true))
      "json_patch" (do (ar (= n 2)) (js/fn-patch false))
      "jsonb_patch" (do (ar (= n 2)) (js/fn-patch true))
      "json_type" (do (ar (or (= n 1) (= n 2))) js/fn-type)
      "json_array_length" (do (ar (or (= n 1) (= n 2))) js/fn-array-length)
      "json_valid" (do (ar (or (= n 1) (= n 2))) js/fn-valid)
      "json_error_position" (do (ar (= n 1)) js/fn-error-position)
      "json_quote" (do (ar (= n 1)) js/fn-quote)
      "json_pretty" (do (ar (or (= n 1) (= n 2))) js/fn-pretty)
      "subtype" (do (ar (= n 1)) js/fn-subtype)
      "unistr" (do (ar (= n 1)) (fn [[x]] (when-not (nil? x) (unistr (v/text-of x)))))
      "unistr_quote" (do (ar (= n 1)) (fn [[x]] (unistr-quote x)))
      nil)))

(defn arrow-fn
  "the -> (as-sql? false) and ->> (true) operators"
  [as-sql?]
  (fn [l r]
    (let [root (js/arg->node l)]
      (when (and root (not (nil? r)))
        (let [path (cond
                     (instance? Long r) (let [i (long r)] (if (>= i 0) (str "$[" i "]") (str "$[#" i "]")))
                     :else
                     (let [t (v/text-of r)]
                       (cond (= t "") (js/jerr "bad JSON path: ''")
                             (= (.charAt ^String t 0) \$) t
                             (= (.charAt ^String t 0) \[) (str "$" t)
                             :else (str "$.\"" t "\""))))
              node (js/lookup root path)]
          (when node
            (if as-sql?
              (let [r (js/node->sql node)] (if (js/json-subtype? r) (String. ^String r) r))
              (js/mark-json (js/node->text node)))))))))

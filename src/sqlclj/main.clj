(ns sqlclj.main
  "A SQLite-compatible database (Benchmark 11).
  Single script: clj-run -m sqlclj.main script.sql            (output on stdout)
  Batch:         clj-run -m sqlclj.main --batch <list> --out <dir>   (one fresh database per script; <dir>/<name>.out)"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [sqlclj.values :as v]
            [sqlclj.exec :as x]
            [sqlclj.guard :as g])
  (:gen-class))

(defn run-script
  "Runs a whole script with a fresh database, appending each input line's output and then a ~~~ line to sb
  (one append per line, under sb's lock) and counting finished lines in ndone. Stops early once main has
  abandoned this thread (guard/killed)."
  [lines ^StringBuilder sb ndone]
  (loop [db x/empty-db [l & more :as ls] lines]
    (when-not (or (empty? ls) (.contains g/killed (Thread/currentThread)))
      (if (empty? l)
        (recur db more)
        (let [[db' out] (try (x/run-line db l)
                             (catch Throwable t
                               [db (str "error: internal " (.getName (class t)) " " (.getMessage t) "\n")]))]
          (locking sb (.append sb ^String out) (.append sb "~~~\n") (vswap! ndone inc))
          (recur db' more))))))

(defn- script-lines
  "Lines of the script as the driver reads them: fgets into a 64 KiB buffer (so a longer line arrives in
  pieces of 65535 bytes), cut at a NUL byte, trailing CR/LF removed; decoded as UTF-8 with invalid bytes kept."
  [f]
  (let [^bytes bs (java.nio.file.Files/readAllBytes (.toPath (io/file f)))
        n (alength bs)
        out (java.util.ArrayList.)]
    (loop [start 0]
      (when (< start n)
        (let [end (loop [i start]
                    (cond (>= i n) n
                          (= (- i start) 65535) i
                          (= 10 (aget bs i)) (inc i)
                          :else (recur (inc i))))
              nul (loop [i start] (cond (>= i end) end (zero? (aget bs i)) i :else (recur (inc i))))
              e (loop [e nul] (if (and (> e start) (let [b (aget bs (dec e))] (or (= b 10) (= b 13)))) (recur (dec e)) e))]
          (when (> e start)
            (.add out (v/utf8->str (java.util.Arrays/copyOfRange bs (int start) (int e)))))
          (recur end))))
    (vec out)))

(defn- write-output [^java.io.OutputStream os ^String s]
  (.write os (v/str->utf8 s))
  (.flush os))

(def ^:private budget-ms
  "A script's time budget. The judge cuts a script (all its statements fail) when no output file appears for
  20 s; past this budget we write what is done and empty blocks for the remaining lines instead."
  (or (some-> (System/getenv "SQLCLJ_BUDGET_MS") parse-long) 15000))

(defn- run-file [f]
  (let [lines (script-lines f)
        sb (StringBuilder.)
        ndone (volatile! 0)
        done (promise)
        t (Thread. nil (fn [] (try (run-script lines sb ndone)
                                   (catch Throwable t
                                     (locking sb (.append sb (str "error: internal " t "\n")))))
                         (deliver done true))
                   "sql" (* 64 1024 1024))]
    (.setDaemon t true)
    (.start t)
    (if (deref done budget-ms nil)
      (str sb)
      (let [_ (.add g/killed t)
            [s k] (locking sb [(str sb) @ndone])
            remaining (- (count (remove empty? lines)) k)]
        (str s (apply str (repeat (max 0 remaining) "~~~\n")))))))

(defn -main [& args]
  (if (= "--batch" (first args))
    (let [[_ list-file _ out-dir] args
          _ (.mkdirs (io/file out-dir))]
      (doseq [f (str/split-lines (slurp list-file)) :when (not (str/blank? f))]
        (let [name (str/replace (.getName (io/file f)) #"\.sql$" "")]
          (with-open [os (io/output-stream (io/file out-dir (str name ".out")))]
            (write-output os (run-file f))))))
    (write-output System/out (run-file (first args))))
  (shutdown-agents))

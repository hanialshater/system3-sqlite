(ns sqlclj.main
  "A SQLite-compatible database (Benchmark 11).
  Single script: clj-run -m sqlclj.main script.sql            (output on stdout)
  Batch:         clj-run -m sqlclj.main --batch <list> --out <dir>   (one fresh database per script; <dir>/<name>.out)"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [sqlclj.values :as v]
            [sqlclj.exec :as x])
  (:gen-class))

(defn run-script
  "Runs a whole script with a fresh database; returns its output (each input line's output, then a ~~~ line)."
  [lines]
  (let [sb (StringBuilder.)]
    (loop [db x/empty-db [l & more :as ls] lines]
      (if (empty? ls)
        (str sb)
        (if (empty? l)
          (recur db more)
          (let [[db' out] (try (x/run-line db l)
                               (catch Throwable t
                                 [db (str "error: internal " (.getName (class t)) " " (.getMessage t) "\n")]))]
            (.append sb ^String out) (.append sb "~~~\n")
            (recur db' more)))))))

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

(defn- run-file [f]
  (let [result (promise)
        t (Thread. nil (fn [] (deliver result (try (run-script (script-lines f))
                                                    (catch Throwable t (str "error: internal " t "\n")))))
                   "sql" (* 64 1024 1024))]
    (.start t)
    @result))

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

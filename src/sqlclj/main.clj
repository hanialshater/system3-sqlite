(ns sqlclj.main
  "A SQLite-compatible database (Benchmark 11). This stub answers every statement `error: not implemented`.
  Single script: clj-run -m sqlclj.main script.sql            (output on stdout)
  Batch:         clj-run -m sqlclj.main --batch <list> --out <dir>   (one fresh database per script; <dir>/<name>.out)"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn run-statement
  "Runs one input line (one or more SQL statements) against db; returns [db' output-text]."
  [db line]
  [db "error: not implemented\n"])

(defn run-script
  "Runs a whole script with a fresh database; returns its output (each input line's output, then a ~~~ line)."
  [lines]
  (let [sb (StringBuilder.)]
    (loop [db {} [l & more :as ls] lines]
      (if (empty? ls)
        (str sb)
        (if (str/blank? l)
          (recur db more)
          (let [[db' out] (run-statement db l)]
            (.append sb out) (.append sb "~~~\n")
            (recur db' more)))))))

(defn- script-lines [f] (str/split-lines (slurp f :encoding "UTF-8")))

(defn -main [& args]
  (if (= "--batch" (first args))
    (let [[_ list-file _ out-dir] args]
      (doseq [f (str/split-lines (slurp list-file)) :when (not (str/blank? f))]
        (let [name (str/replace (.getName (io/file f)) #"\.sql$" "")]
          (spit (io/file out-dir (str name ".out")) (run-script (script-lines f)) :encoding "UTF-8"))))
    (print (run-script (script-lines (first args)))))
  (flush))

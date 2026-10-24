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
        (if (str/blank? l)
          (recur db more)
          (let [[db' out] (try (x/run-line db l)
                               (catch Throwable t
                                 [db (str "error: internal " (.getName (class t)) " " (.getMessage t) "\n")]))]
            (.append sb ^String out) (.append sb "~~~\n")
            (recur db' more)))))))

(defn- script-lines
  "Lines of the script, decoded as UTF-8 with invalid bytes kept as escapes; trailing CR/LF removed."
  [f]
  (let [bytes (java.nio.file.Files/readAllBytes (.toPath (io/file f)))
        text (v/utf8->str bytes)]
    (map #(str/replace % #"\r+$" "") (str/split text #"\n" -1))))

(defn- write-output [^java.io.OutputStream os ^String s]
  (.write os (v/str->utf8 s))
  (.flush os))

(defn- run-file [f]
  (let [result (promise)
        t (Thread. nil (fn [] (deliver result (try (run-script (script-lines f))
                                                    (catch Throwable t (str "error: internal " t "\n")))))
                   "sql" (* 512 1024 1024))]
    (.start t)
    @result))

(defn -main [& args]
  (if (= "--batch" (first args))
    (let [[_ list-file _ out-dir] args]
      (doseq [f (str/split-lines (slurp list-file)) :when (not (str/blank? f))]
        (let [name (str/replace (.getName (io/file f)) #"\.sql$" "")]
          (with-open [os (io/output-stream (io/file out-dir (str name ".out")))]
            (write-output os (run-file f))))))
    (write-output System/out (run-file (first args))))
  (shutdown-agents))

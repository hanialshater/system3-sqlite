(ns sqlclj.ext
  "Scalar functions beyond the core set: printf/format, date and time, json, misc.
  (compile-fn name nargs coll) returns (fn [vals]) or nil when the function is unknown."
  (:require [sqlclj.values :as v]
            [sqlclj.printf :as pf]))

(defn compile-fn [name n coll]
  (case name
    ("printf" "format")
    (fn [args]
      (when (and (seq args) (not (nil? (first args))))
        (pf/sql-printf (v/text-of (first args)) (rest args))))
    nil))

(ns sqlclj.plan-ids
  "Best-effort model of the bytecode addresses that EXPLAIN QUERY PLAN prints as the id column: the address of
  each OP_Explain. Only the shapes SQLite's test suite uses most (single table, simple terms) are modelled."
  (:require [clojure.string :as str]
            [sqlclj.engine :as e]
            [sqlclj.plan-glue :as g]))

(defn ops
  "Number of VDBE instructions that evaluating expression x into a register takes inside a loop body."
  [G x]
  (cond
    (not (map? x)) 0
    :else
    (case (:op x)
      (:col :colref :fixed) 1
      :lit 0
      :collate (ops G (:e x))
      :unary (case (:o x) (:plus :pos) (ops G (:e x)) (inc (ops G (:e x))))
      :bin (+ (ops G (:l x)) (ops G (:r x)) 1)
      :fn (inc (reduce + 0 (map #(ops G %) (:args x))))
      :cast (inc (ops G (:e x)))
      (:isnull :notnull) (inc (ops G (:e x)))
      :is (+ (ops G (:l x)) (ops G (:r x)) 1)
      :in (+ (ops G (:e x)) 2)
      :between (+ (ops G (:e x)) 2)
      :like (+ (ops G (:e x)) (if (= :glob (:o x)) 1 2))
      :case (inc (reduce + 0 (map #(ops G %) (tree-seq coll? seq (dissoc x :op)))))
      1)))

(defn result-ops [G x]
  (if (and (map? x) (= :lit (:op x))) 1 (max 1 (ops G x))))

(defn pred-ops
  "Instructions of a WHERE term evaluated per row (compare + jump)."
  [G x]
  (case (:op x)
    :bin (if (#{:and} (:o x)) (+ (pred-ops G (:l x)) (pred-ops G (:r x))) (ops G x))
    :in (+ (ops G (:e x)) 2)
    (ops G x)))

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
      :in (+ (ops G (:e x)) 2 (reduce + 0 (map #(ops G %) (:list x))))
      :between (+ (ops G (:e x)) (ops G (:lo x)) (ops G (:hi x)) 2)
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

(defn const-ops
  "Instructions before the loop for a WHERE term that became constant (all columns replaced by constants)."
  [x]
  (letfn [(num-lit? [n] (and (map? n) (= :lit (:op n)) (#{:int :float} (:kind n))))
          (unfix [n] (if (and (map? n) (= :fixed (:op n))) (:value n) n))
          (c [n]
            (let [n (unfix n)]
              (cond
                (not (map? n)) 0
                (= :lit (:op n)) 1
                (= :fixed (:op n)) 1
                (= :bin (:op n))
                (let [l (unfix (:l n)) r (unfix (:r n))]
                  (cond (#{:and :or} (:o n)) (+ (c l) (c r))
                        (and (num-lit? l) (num-lit? r)) 2
                        :else (+ (c l) (c r) 1)))
                (= :fn (:op n)) (inc (reduce + 0 (map c (:args n))))
                (= :like (:op n)) (+ (c (:e n)) (c (:pat n)) 2)
                (= :unary (:op n)) (c (:e n))
                :else (+ 1 (reduce + 0 (map c (filter map? (vals n))))))))]
    (c x)))

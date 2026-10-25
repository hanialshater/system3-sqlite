(require '[sqlclj.exec :as x] '[sqlclj.engine :as e] '[sqlclj.plan-eqp :as pe] '[sqlclj.planner :as pl] '[clojure.string :as str])
(let [db (first (x/run-line x/empty-db "CREATE TABLE t1(a INTEGER PRIMARY KEY, b, c); CREATE TABLE t2(x INTEGER PRIMARY KEY, y, z);"))]
  (reset! e/plan-hook (fn [info] (prn :from (map #(select-keys % [:join :on]) (:from info))) (prn :where (:where info)))))
(println (first (x/run-line (first (x/run-line x/empty-db "CREATE TABLE t1(a INTEGER PRIMARY KEY, b, c); CREATE TABLE t2(x INTEGER PRIMARY KEY, y, z);")) "EXPLAIN QUERY PLAN SELECT * FROM t1 LEFT JOIN t2 ON t1.b=t2.y")))

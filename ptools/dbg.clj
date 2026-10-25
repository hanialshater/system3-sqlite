(require '[sqlclj.exec :as x] '[sqlclj.engine :as e] '[sqlclj.plan-eqp :as pe] '[sqlclj.planner :as pl] '[clojure.string :as str])
;; usage: clj.sh ptools/dbg.clj file.sql  -- runs all lines but the last; prints plan details for the last
(let [lines (remove str/blank? (str/split-lines (slurp (first *command-line-args*))))
      setup (butlast lines)
      q (last lines)
      db (reduce (fn [db l] (first (x/run-line db l))) x/empty-db setup)]
  (reset! e/plan-hook (fn [info]
                        (let [r (pe/plan-core info)]
                          (println "TERMS")
                          (doseq [t (:terms (:P r))]
                            (println " " (:id t) (pr-str (e/strip-ast (:expr t))) (select-keys t [:op :left :right-col :prereq-right :prereq-all :virtual :parent :equiv :on-of :coll :cmp-aff :truth])))
                          (println "LOOPS")
                          (doseq [l (:loops (:plan r))]
                            (println " " (select-keys l [:tab :prereq :setup :run :nout :flags :neq :nskip :sortidx]) (some-> (:idx l) (#(if (map? %) (:name %) %))) (mapv :id (:lterms l))))
                          (println "PATH" (mapv (fn [l] [(:tab l) (:run l) (:nout l)]) (:path (:plan r))) "ordered" (:ordered (:plan r)) "rcost" (:rcost (:plan r))))))
  (let [[db2 out] (x/run-line db q)]
    (println out)))

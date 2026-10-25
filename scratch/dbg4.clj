(require '[sqlclj.exec :as x] '[sqlclj.engine :as e] '[sqlclj.plan-eqp :as pe] '[sqlclj.planner :as pl] '[clojure.string :as str])
(let [lines (remove str/blank? (str/split-lines (slurp (first *command-line-args*))))
      setup (butlast lines)
      q (last lines)
      db (reduce (fn [db l] (first (x/run-line db l))) x/empty-db setup)
      orig @#'pl/loop-insert]
  (with-redefs [pl/loop-insert (fn [loops t]
                                 (let [r (orig loops t)]
                                   (println "INSERT" (select-keys t [:tab :prereq :setup :run :nout :flags :neq :sortidx]) (some-> (:idx t) (#(if (map? %) (:name %) %)))
                                            "->" (count r))
                                   r))]
    (reset! e/plan-hook (fn [info] (pe/plan-core info)))
    (binding [e/*eqp* (volatile! [])]
      (let [[db2 out] (x/run-line db q)]
        (println out)))))

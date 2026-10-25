(ns sqlclj.plan-eqp
  "Planner driver for SELECT cores: builds the problem, runs sqlclj.planner, renders EXPLAIN QUERY PLAN."
  (:require [clojure.string :as str]
            [sqlclj.engine :as e]
            [sqlclj.planner :as pl]
            [sqlclj.plan-glue :as g]
            [sqlclj.values :as v]))

(def ^:private lc e/lc)

(defn- aff-class [a] (case a (:integer :real :numeric) :numeric :text :text :none))

(defn- default-logest [nrow nkey unique partial]
  (let [x (max 99 (long nrow))
        a0 (if partial (- x 10) x)]
    (vec (concat [a0]
                 (let [base (take (min 5 nkey) [33 32 30 28 26])
                       more (repeat (max 0 (- nkey 5)) 23)
                       l (vec (concat base more))]
                   (if (and unique (pos? nkey)) (assoc l (dec nkey) 0) l))))))

(defn- src-used-cols [info src]
  (set (for [[sl i] @(:refs info)
             :when (and (= sl (:slot src)) (< (long i) (long (:ncols src))))
             :when (not= i (:ipk (:table src)))]
         i)))

(defn- build-index [info src tab-nrow used idx]
  (let [tbl (:table src)
        ncols (count (:cols tbl))
        ipk (:ipk tbl)
        cols (mapv (fn [ic]
                     (let [c (:col ic)
                           c (cond (nil? c) nil (= c ncols) -1 (= c ipk) -1 :else c)]
                       {:c c :coll (or (:coll ic) "BINARY") :desc (boolean (:desc ic))
                        :aff (if (and c (>= (long c) 0)) (aff-class (:aff (nth (:cols tbl) c))) :none)
                        :notnull (boolean (or (= c -1) (and c (>= (long c) 0) (:notnull (nth (:cols tbl) c)))))
                        :expr (when (nil? c) (pr-str (e/strip-ast (:ast ic))))}))
                   (:cols idx))
        nkey (count cols)
        w (+ 1 (reduce + (map (fn [ic] (let [c (:col ic)]
                                         (if (or (nil? c) (= c ncols) (= c ipk)) 1 (e/col-size-est (nth (:cols tbl) c)))))
                              (:cols idx))))
        colset (set (keep :c cols))
        stat (:stat idx)
        logest (if stat (:logest stat) (default-logest tab-nrow nkey (:unique idx) (:where idx)))]
    {:name (:name idx) :cols cols :nkey nkey :ncol (inc nkey)
     :unique (boolean (:unique idx))
     :uniq-not-null (and (:unique idx) (every? :notnull cols))
     :onerror (boolean (:unique idx))
     :logest logest :has-stat1 (boolean stat)
     :szidx (pl/le (* 4 w))
     :unordered (boolean (:unordered stat))
     :noskip (boolean (:noskip stat))
     :partial (some? (:where idx))
     :colset colset
     :covering (and (every? #(contains? colset %) used) true)
     :idx idx}))

(defn- table-indexes [db tbl]
  (->> (vals (:indexes db))
       (filter #(= (:table %) (lc (:name tbl))))
       (sort-by (fn [i] (long (:seq i))))))

(defn- build-tab [info G i src item]
  (let [db (:db info)
        tbl (:table src)
        jt (:join item)
        jointype (cond-> #{}
                   (#{:left :natural-left} jt) (conj :left)
                   (= :cross jt) (conj :cross))]
    (if tbl
      (let [sz (+ (reduce + (map e/col-size-est (:cols tbl))) (if (:ipk tbl) 0 1))
            nrow (or (:stat-nrow tbl) 200)
            used (src-used-cols info src)]
        {:i i :nrow nrow :szrow (pl/le (* 4 sz)) :rowid? (not (:without-rowid tbl))
         :jointype jointype
         :ipk? (some? (:ipk tbl))
         :indexed-by (when (and (:indexed-by src) (not= :none (:indexed-by src))) (:indexed-by src))
         :not-indexed (= :none (:indexed-by src))
         :auto-ok (:auto-ok info)
         :src src
         :indexes (mapv #(build-index info src nrow used %)
                        (remove #(some (fn [ic] (nil? (:col ic))) (:cols %)) (table-indexes db tbl)))})
      {:i i :nrow 200 :szrow 60 :rowid? true :jointype jointype :ephemeral true :auto-ok (:auto-ok info) :src src :indexes []})))

(defn- order-spec
  "ORDER BY / GROUP BY term specs for the planner from AST expressions."
  [G exprs descs colls]
  (vec (map (fn [x desc coll]
              (let [x* (if (= :collate (:op x)) (:e x) x)
                    cr (#'g/col-ref G x*)]
                {:t (first cr) :c (second cr) :coll (or coll "BINARY") :desc (boolean desc)
                 :other-mask (#'g/expr-tables G x)
                 :const (e/const-term? x)}))
            exprs descs colls)))

(defn- loop-detail [P ^sqlclj.planner.Loop lp]
  (let [tab (nth (:tables P) (:tab lp))
        src (:src tab)
        tname (or (:alias-display src) (:alias src))
        flags (:flags lp)
        idx (:idx lp)
        neq (long (:neq lp))
        search? (or (contains? flags :btm-limit) (contains? flags :top-limit) (pos? neq))
        nm (fn [pidx i] (let [c (:c (nth (:cols pidx) i))]
                          (cond (nil? c) "<expr>" (= -1 c) "rowid" :else (nth (:cols src) c))))
        range-str
        (when (and idx (map? idx))
          (when (or (pos? neq) (contains? flags :btm-limit) (contains? flags :top-limit))
            (let [nskip (long (:nskip lp))
                  eqs (for [i (range neq)] (if (>= i nskip) (str (nm idx i) "=?") (str "ANY(" (nm idx i) ")")))
                  j neq
                  term (fn [nterm iterm band op]
                         (str (when band " AND ")
                              (if (> nterm 1) (str "(" (str/join "," (for [k (range nterm)] (nm idx (+ iterm k)))) ")") (nm idx iterm))
                              op
                              (if (> nterm 1) (str "(" (str/join "," (repeat nterm "?")) ")") "?")))
                  btm (when (contains? flags :btm-limit) (term (:nbtm lp) j (pos? neq) ">"))
                  top (when (contains? flags :top-limit) (term (:ntop lp) j (or (pos? neq) (contains? flags :btm-limit)) "<"))]
              (str " (" (str/join " AND " eqs) btm top ")"))))
        ]
    (str (if search? "SEARCH " "SCAN ") tname
         (cond
           (contains? flags :ipk)
           (when (or (contains? flags :column-eq) (contains? flags :column-in) (contains? flags :column-range))
             (str " USING INTEGER PRIMARY KEY (rowid"
                  (cond (or (contains? flags :column-eq) (contains? flags :column-in)) "="
                        (and (contains? flags :btm-limit) (contains? flags :top-limit)) ">? AND rowid<"
                        (contains? flags :btm-limit) ">"
                        :else "<")
                  "?)"))
           (= idx :auto) (str " USING AUTOMATIC COVERING INDEX"
                              (str " (" (str/join " AND " (for [t (:lterms lp)] (let [c (second (:left t))] (str (nth (:cols src) c) "=?")))) ")"))
           (map? idx) (str " USING " (if (contains? flags :idx-only) "COVERING " "") "INDEX " (:name idx) range-str)
           :else ""))))

(defn plan-core
  "Plans a SELECT core. info: {:db :ctx :sources :from :where :on-terms :order-exprs ...}. Returns {:P :plan} or nil."
  [info]
  (let [srcs (:sources info)
        from (:from info)]
    (when (and (seq srcs)
               (< (count srcs) 63)
               (every? (fn [s] (and (not (:tvf s)) (or (:sub s) (:table s)))) srcs)
               (every? (fn [it] (not (#{:right :full :natural-right :natural-full} (:join it)))) from))
      (let [G {:ctx (assoc (:ctx info) :refs nil) :sources srcs
               :slot->i (into {} (map-indexed (fn [i s] [(:slot s) i]) srcs))
               :case-sensitive-like (:case-sensitive-like info)}
            ;; WHERE and ON terms
            conjuncts (concat
                       (for [x (e/split-and* (:where info))] {:expr x})
                       (for [[i it] (map-indexed vector from)
                                    :let [left? (#{:left :natural-left} (:join it))]
                                    x (when (:on it) (e/split-and* (:on it)))]
                                {:expr x :on-of (when left? i)}))
            terms (mapv #(assoc % :ucols (g/ucols G (:expr %))) (g/analyze-where G conjuncts))
            ;; per-term used columns and prerequisites
            tabs (vec (map-indexed (fn [i [s it]] (build-tab (assoc info :auto-ok true) G i s it)) (map vector srcs from)))
            order (:order-spec info)
            P {:tables tabs :terms terms :order (or order []) :order-kind (:order-kind info) :ncols (:ncols info)
               :limit (:limit-est info) :want-distinct (:want-distinct info) :nquery-loop 0}
            P (update P :tables (fn [ts] (mapv (fn [t] (update t :indexes
                                                               (fn [ixs] (mapv #(assoc % :might-help-order
                                                                                       (boolean (some (fn [o] (and (= (:t o) (:i t)) (:c o)
                                                                                                                   (or (= -1 (:c o)) (some (fn [ic] (= (:c ic) (:c o))) (:cols %)))))
                                                                                                      (:order P))))
                                                                       ixs)))) ts)))
            plan (pl/plan P)]
        {:P P :plan plan}))))

(defn eqp-rows
  "Plan rows [id parent notused detail] for a planned core."
  [{:keys [P plan]}]
  (vec (for [lp (:path plan)]
         [0 0 (:run lp) (loop-detail P lp)])))

(defn- eqp-hook [info]
  (let [info (assoc info
                    :order-spec (when (seq (:order-exprs info))
                                  (let [G {:ctx (assoc (:ctx info) :refs nil) :sources (:sources info)
                                           :slot->i (into {} (map-indexed (fn [i s] [(:slot s) i]) (:sources info)))}]
                                    (order-spec G (:order-exprs info) (:order-descs info) (:order-colls info))))
                    :order-kind :orderby)
        r (plan-core info)]
    (when r (vswap! e/*eqp* into (eqp-rows r)))))

(reset! e/plan-hook eqp-hook)

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

(defn- implies1? [ctx w t src]
  (or (e/check-indexed-by ctx {:where-ast w} [t] src)
      (and (map? w) (= :bin (:op w)) (= :or (:o w))
           (or (implies1? ctx (:l w) t src) (implies1? ctx (:r w) t src)))))

(defn- partial-usable?
  "whereUsablePartialIndex: every conjunct of the index WHERE is implied by some WHERE term."
  [ctx idx terms src]
  (every? (fn [w] (some (fn [t] (implies1? ctx w t src)) terms))
          (e/split-and* (:where-ast idx))))

(defn- build-index [info src tab-nrow used idx partial-ok]
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
        dflt (default-logest tab-nrow nkey (:unique idx) (:where idx))
        logest (if stat
                 (let [g (:logest stat)] (vec (map-indexed (fn [i d] (if (< i (count g)) (nth g i) d)) dflt)))
                 dflt)]
    {:name (:name idx) :cols cols :nkey nkey :ncol (inc nkey)
     :unique (boolean (:unique idx))
     :uniq-not-null (and (:unique idx) (every? :notnull cols))
     :onerror (boolean (:unique idx))
     :logest logest :has-stat1 (boolean stat)
     :szidx (pl/le (* 4 w))
     :unordered (boolean (:unordered stat))
     :noskip (boolean (:noskip stat))
     :partial (some? (:where idx))
     :partial-ok (when (:where idx) (partial-ok idx))
     :colset colset
     :covering (and (every? #(contains? colset %) used) true)
     :idx idx}))

(defn- table-indexes [db tbl]
  (->> (vals (:indexes db))
       (filter #(= (:table %) (lc (:name tbl))))
       (sort-by (fn [i] (- (long (:seq i)))))))

(defn- build-tab [info G i src item]
  (let [db (:db info)
        tbl (:table src)
        jt (:join item)
        partial-terms (vec (for [c (:conjuncts info)
                                 :when (if (#{:left :natural-left} jt) (= (:on-of c) i) (or (nil? (:on-of c)) (= (:on-of c) i)))]
                             (:expr c)))
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
         :indexes (mapv #(build-index info src nrow used % (fn [idx] (try (partial-usable? (:ctx info) idx partial-terms src) (catch Throwable _ false))))
                        (remove #(some (fn [ic] (nil? (:col ic))) (:cols %)) (table-indexes db tbl)))})
      {:i i :nrow (or (some-> src :sub :nrow-est) 200) :szrow (pl/le (* 4 (inc (count (:cols src))))) :rowid? true :jointype jointype :ephemeral true :auto-ok (:auto-ok info) :src src :indexes []})))

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
  "Plans a SELECT core. info: see the hook call in engine/compile-core. Returns {:P :plan :G} or nil."
  [info]
  (let [srcs (:sources info)
        from (:from info)]
    (when (and (seq srcs)
               (< (count srcs) 63)
               (every? (fn [s] (and (not (:tvf s)) (or (:sub s) (:table s)))) srcs)
               (every? (fn [s] (or (not (:table s)) (not (:without-rowid (:table s))))) srcs)
               (every? (fn [s] (or (not (:table s)) (and (not (:virtual (:table s))) (some? (get-in (:db info) [:tables (lc (:tname s))]))))) srcs)
               (every? (fn [it] (not (#{:right :full :natural-right :natural-full} (:join it)))) from))
      (let [G {:ctx (assoc (:ctx info) :refs nil) :sources srcs
               :slot->i (into {} (map-indexed (fn [i s] [(:slot s) i]) srcs))
               :case-sensitive-like (let [x (get-in (:db info) [:pragmas "case_sensitive_like"])] (boolean (and x (not= x 0) (not= x false))))}
            using-conj (fn [i it]
                         (let [using (nth (:join-conds info) i nil)
                               left? (#{:left :natural-left} (:join it))
                               s (nth srcs i)]
                           (for [u using
                                 :let [lu (lc u)
                                       lsrc (first (filter #(some #{lu} (:lcols %)) (take i srcs)))]
                                 :when lsrc]
                             {:expr {:op :bin :o :eq
                                     :l {:op :colref :slot (:slot lsrc) :idx (.indexOf ^java.util.List (:lcols lsrc) lu)}
                                     :r {:op :colref :slot (:slot s) :idx (.indexOf ^java.util.List (:lcols s) lu)}}
                              :on-of (when left? i)})))
            conjuncts (vec (concat
                            (for [x (e/split-and* (:where info))] {:expr x})
                            (apply concat
                                   (for [[i it] (map-indexed vector from)
                                         :let [left? (#{:left :natural-left} (:join it))]]
                                     (concat (for [x (when (:on it) (e/split-and* (:on it)))]
                                               {:expr x :on-of (when left? i)})
                                             (using-conj i it))))))
            orig-conjuncts conjuncts
            conjuncts (try (g/propagate-constants G conjuncts) (catch clojure.lang.ExceptionInfo _ conjuncts))
            terms (mapv #(assoc % :ucols (g/ucols G (:expr %))) (g/analyze-where G conjuncts))
            tabs (vec (map-indexed (fn [i [s it]] (build-tab (assoc info :auto-ok true :conjuncts orig-conjuncts) G i s it)) (map vector srcs from)))
            ;; the ORDER BY the planner sees: GROUP BY, else ORDER BY, else DISTINCT
            [okind oexprs odescs ocolls]
            (cond (seq (:group-exprs info)) [:groupby (:group-exprs info) (repeat false) (:group-colls info)]
                  (:is-agg info) [nil nil nil nil]
                  (:win? info) [nil nil nil nil]
                  (seq (:order-exprs info)) [:orderby (:order-exprs info) (:order-descs info) (:order-colls info)]
                  (:distinct info) [:distinctby (:result-exprs info) (repeat false) (:result-colls info)]
                  :else [nil nil nil nil])
            ;; isDistinctRedundant: a unique key is among the DISTINCT columns
            [okind oexprs odescs ocolls]
            (if (and (= okind :distinctby) (= 1 (count srcs)) (:table (first srcs))
                     (let [crs (set (keep #(g/col-ref G (if (= :collate (:op %)) (:e %) %)) oexprs))
                           eqc (fn [c] (some (fn [t] (and (= (:left t) [0 c]) (= :eq (:op t)) (zero? (long (:prereq-right t)))))
                                             terms))]
                       (or (contains? crs [0 -1])
                           (some (fn [ix] (and (:unique ix) (not (:partial ix))
                                               (every? (fn [ic] (or (eqc (:c ic))
                                                                    (and (contains? crs [0 (:c ic)]) (:notnull ic))))
                                                       (:cols ix))))
                                 (:indexes (first tabs))))))
              [nil nil nil nil]
              [okind oexprs odescs ocolls])
            order (when okind (order-spec G oexprs odescs ocolls))
            P {:tables tabs :terms terms :order (or order []) :order-kind okind :ncols (:ncols info)
               :limit (:limit-est info) :want-distinct (boolean (:distinct info)) :nquery-loop 0}
            P (update P :tables (fn [ts] (mapv (fn [t] (update t :indexes
                                                               (fn [ixs] (mapv #(assoc % :might-help-order
                                                                                       (boolean (some (fn [o] (and (= (:t o) (:i t)) (:c o)
                                                                                                                   (or (= -1 (:c o)) (some (fn [ic] (= (:c ic) (:c o))) (:cols %)))))
                                                                                                      (:order P))))
                                                                       ixs)))) ts)))
            plan (pl/plan P)]
        {:P P :plan plan :G G :okind okind}))))

(defn- no-early? [x]
  (let [bad (volatile! false)]
    (letfn [(walk [x]
              (cond (map? x)
                    (do (when (or (#{:subq :exists :window} (:op x)) (:select x) (:over x) (and (= :fn (:op x)) (g/nondeterministic? x))) (vreset! bad true))
                        (doseq [[_ v] x] (walk v)))
                    (sequential? x) (doseq [y x] (walk y))))]
      (walk x))
    @bad))

(defn- early-level
  "Plan level at which an expression can be evaluated (all its tables bound), or nil to evaluate at the leaf."
  [G pos x]
  (when (and x (not (no-early? x)))
    (let [m (g/expr-tables G x)]
      (when-not (zero? (long m))
        (reduce (fn [mx i] (if (zero? (bit-and m (bit-shift-left 1 i))) mx (max mx (long (get pos i 0)))))
                0 (range (count (:sources G))))))))

(defn exec-spec
  "What the executor needs from a plan."
  [{:keys [P plan okind G]} info]
  (let [path (:path plan)
        rev (long (:rev-mask plan))
        pos (zipmap (map :tab path) (range))
        from (:from info)]
    {:order (mapv :tab path) :nrow-out (:nrow-out plan)
     :wconj (vec (for [x (e/split-and* (:where info))] {:expr x :level (early-level G pos x)}))
     :on-levels (vec (for [[i it] (map-indexed vector from)]
                       (let [using (nth (:join-conds info) i nil)
                             lv (concat (for [x (when (:on it) (e/split-and* (:on it)))] (early-level G pos x))
                                        (for [u using] (let [lu (lc u)
                                                             lsrc (first (keep-indexed (fn [j s] (when (some #{lu} (:lcols s)) j)) (take i (:sources G))))]
                                                         (when lsrc (max (long (get pos lsrc 0)) (long (get pos i 0)))))))]
                         (cond (some nil? lv) nil
                               (empty? lv) nil
                               :else (apply max lv)))))
     :ordered (:ordered plan) :nob (count (:order P)) :okind okind
     :loops
     (vec (map-indexed
           (fn [k lp]
             (let [flags (:flags lp)
                   idx (:idx lp)
                   neq (long (:neq lp)) nskip (long (:nskip lp))
                   lterms (:lterms lp)
                   unfix (fn unfix [x] (cond (and (map? x) (= :fixed (:op x))) (:value x)
                                              (and (map? x) (not (contains? x :select))) (into {} (map (fn [[k v]] [k (if (or (map? v) (sequential? v)) (unfix v) v)]) x))
                                              (vector? x) (mapv unfix x)
                                              :else x))
                   tm (fn [t] (when t {:op (:op t) :rhs (unfix (:rhs t)) :in-list (unfix (:in-list t)) :in-select (:in-select t)
                                       :col (second (:left t)) :likeopt (:likeopt t)}))
                   auto? (= idx :auto)
                   autoterms (when auto?
                               (let [ready (reduce bit-or 0 (map #(bit-shift-left 1 (long (:tab %))) (take k path)))
                                     i (:tab lp) tab (nth (:tables P) i)]
                                 (loop [ts (:terms P) seen #{} out []]
                                   (if-let [t (first ts)]
                                     (let [c (second (:left t))]
                                       (if (and (= (first (:left t)) i) (contains? #{:eq :is} (:op t)) (some? c) (>= (long c) 0)
                                                (:auto-ok t) (not (contains? seen c))
                                                (zero? (bit-and (long (:prereq-right t)) (bit-not ready)))
                                                (or (not (contains? (:jointype tab) :left)) (= (:on-of t) i)))
                                         (recur (rest ts) (conj seen c) (conj out t))
                                         (recur (rest ts) seen out)))
                                     out))))
                   eq (if auto?
                        (vec (map-indexed (fn [j t] (assoc (tm t) :pos j)) autoterms))
                        (vec (for [j (range nskip neq)] (assoc (tm (nth lterms j)) :pos j))))
                   auto-extra (when auto?
                                (let [used (src-used-cols info (:src (nth (:tables P) (:tab lp))))
                                      keyc (set (map :col eq))]
                                  (vec (sort (remove keyc used)))))
                   btm (when (contains? flags :btm-limit) (tm (nth lterms neq nil)))
                   top (when (contains? flags :top-limit)
                         (tm (if (contains? flags :btm-limit) (nth lterms (inc neq) nil) (nth lterms neq nil))))]
               {:src (:tab lp)
                :kind (cond (contains? flags :ipk) :ipk (= idx :auto) :auto (map? idx) :index :else :scan)
                :index (when (map? idx) (:idx idx))
                :nskip nskip :eq eq :btm btm :top top :auto-extra auto-extra
                :rev (not (zero? (bit-and rev (bit-shift-left 1 k))))
                :onerow (contains? flags :onerow)}))
           path))}))

(defn eqp-rows
  "Plan rows [id parent notused detail] for a planned core."
  [{:keys [P plan okind G] :as r} spec info]
  (let [path (:path plan)
        nob (count (:order P))
        ordered (long (:ordered plan))
        sat (if (neg? ordered) 0 ordered)
        loop-rows
        (vec (mapcat
              (fn [k lp]
                (let [tab (nth (:tables P) (:tab lp))
                      src (:src tab)
                      left? (contains? (:jointype tab) :left)
                      detail (loop-detail P lp)]
                  (if (= (:idx lp) :auto)
                    (let [eq (:eq (nth (:loops spec) k))
                          cols (str/join " AND " (for [t eq] (str (nth (:cols src) (:col t)) "=?")))
                          partial? (some (fn [t] (and (not (:virtual t)) (= (long (:prereq-all t)) (bit-shift-left 1 (long (:tab lp))))
                                                      (or (nil? (:on-of t)) (= (:on-of t) (:tab lp)))))
                                         (:terms P))
                          tname (or (:alias-display src) (:alias src))]
                      [[0 0 0 (str "BLOOM FILTER ON " tname " (" cols ")")]
                       [0 0 (:run lp)
                        (str "SEARCH " tname " USING AUTOMATIC " (when partial? "PARTIAL ") "COVERING INDEX (" cols ")" (when left? " LEFT-JOIN"))]])
                    [[0 0 (:run lp) (str detail (when left? " LEFT-JOIN"))]])))
              (range) path))
        order-line (fn [sat nob]
                     (cond (zero? sat) "USE TEMP B-TREE FOR ORDER BY"
                           (= 1 (- nob sat)) "USE TEMP B-TREE FOR LAST TERM OF ORDER BY"
                           :else (str "USE TEMP B-TREE FOR LAST " (- nob sat) " TERMS OF ORDER BY")))
        extra
        (case okind
          :orderby (concat (when (:distinct info) ["USE TEMP B-TREE FOR DISTINCT"])
                           (when (< sat nob) [(order-line sat nob)]))
          :groupby (concat (when (< sat nob) ["USE TEMP B-TREE FOR GROUP BY"])
                           (when (and (seq (:order-exprs info)) (not= (mapv e/strip-ast (:order-exprs info)) (mapv e/strip-ast (:group-exprs info))))
                             ["USE TEMP B-TREE FOR ORDER BY"]))
          :distinctby (when (< sat nob) ["USE TEMP B-TREE FOR DISTINCT"])
          nil)]
    (into loop-rows (map (fn [t] [0 0 0 t]) extra))))

(defn- plan-hook-fn [info]
  (let [r (plan-core info)]
    (when r
      (let [spec (exec-spec r info)]
        (when e/*eqp* (vswap! e/*eqp* into (eqp-rows r spec info)))
        spec))))

(reset! e/plan-hook plan-hook-fn)

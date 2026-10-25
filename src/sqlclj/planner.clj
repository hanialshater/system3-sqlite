(ns sqlclj.planner
  "A port of SQLite's next-generation query planner (where.c): WhereLoop generation for each table
  (full scans, index and rowid lookups with equality, IN and range constraints, automatic indexes,
  skip-scans), the N-best-paths solver, and the ORDER BY / GROUP BY / DISTINCT satisfaction analysis.
  Pure functions over a 'problem' map; the glue that builds problems from the SQL AST lives in
  sqlclj.plan-glue.

  Problem:
   {:tables [{:i n :nrow LogEst :szrow LogEst :ipk? bool :jointype #{:left :cross :right}
              :indexed-by name :not-indexed bool :ephemeral bool :auto-ok bool
              :indexes [index ...]   ; newest first, as SQLite's pTab->pIndex
              :colused-all bool}]
    :terms [term ...]   ; in WhereClause order (base terms, then virtual terms)
    :order [{:t tbl :c col :expr key :coll str :desc bool :bignull bool :other-mask m :const bool} ...]
    :order-kind :orderby|:groupby|:distinctby
    :limit LogEst-or-nil :ncols n}
  Index: {:name :cols [{:c col :coll :desc :aff :expr key}] :nkey :ncol :unique :uniq-not-null
          :logest [..] :has-stat1 :szidx :unordered :noskip :partial-ok :covering :ipk}"
  (:require [clojure.string :as str]))

;; ====================================================================== LogEst arithmetic

(def ^:private a-tab [0 2 3 5 6 7 8 9])

(defn le
  "sqlite3LogEst of a non-negative integer."
  ^long [^long x]
  (if (< x 8)
    (if (< x 2) 0
        (loop [y 40 x x] (if (< x 8) (recur (- y 10) (bit-shift-left x 1)) (+ (long (nth a-tab (bit-and x 7))) y -10))))
    (let [i (- 60 (Long/numberOfLeadingZeros x))]
      (+ (long (nth a-tab (bit-and (unsigned-bit-shift-right x i) 7))) 40 (* i 10) -10))))

(def ^:private x-add [10 10 9 9 8 8 7 7 7 6 6 6 5 5 5 4 4 4 4 3 3 3 3 3 3 2 2 2 2 2 2 2])

(defn le-add
  "sqlite3LogEstAdd."
  ^long [^long a ^long b]
  (cond (>= a b) (cond (> a (+ b 49)) a
                       (> a (+ b 31)) (inc a)
                       :else (+ a (long (nth x-add (- a b)))))
        :else (cond (> b (+ a 49)) b
                    (> b (+ a 31)) (inc b)
                    :else (+ b (long (nth x-add (- b a)))))))

(defn est-log
  "estLog: log of a LogEst-scaled row count."
  ^long [^long n]
  (if (<= n 10) 0 (- (le n) 33)))

(defn le-from-double
  "sqlite3LogEstFromDouble."
  ^long [^double x]
  (if (<= x 1.0) 0
      (let [bits (Double/doubleToLongBits x)
            e (- (bit-and (unsigned-bit-shift-right bits 52) 0x7ff) 1023)]
        (- (+ (* e 10) 0) 0))))

;; ====================================================================== term helpers

(defn- bit ^long [^long i] (bit-shift-left 1 i))

(def eq-ops #{:eq :is})
(def range-ops #{:lt :le :gt :ge})

(defn aff-ok?
  "sqlite3IndexAffinityOk: term comparison affinity class vs the index column's affinity class."
  [cmp-aff idx-aff]
  (case cmp-aff
    (:none nil) true
    :text (= idx-aff :text)
    (= idx-aff :numeric)))

(defn- scan-terms
  "whereScanInit/whereScanNext: terms usable for column col of table tbl whose operator is in ops.
  coll/idx-aff (nil for rowid) are checked like SQLite does. Follows column equivalences."
  [terms tbl col ops coll idx-aff]
  (let [terms (vec terms)
        n (count terms)]
    (loop [cls [[tbl col]] ei 0 out []]
      (if (>= ei (count cls))
        out
        (let [[ct cc] (nth cls ei)
              [cls out]
              (loop [k 0 cls cls out out]
                (if (>= k n)
                  [cls out]
                  (let [t (nth terms k)
                        l (:left t)]
                    (if (and l (= (first l) ct) (= (second l) cc)
                             (or (<= (inc ei) 1) (nil? (:on-of t))))
                      (let [cls (if (and (:equiv t) (< (count cls) 11) (:right-col t)
                                         (not (some #(= % (:right-col t)) cls)))
                                  (conj cls (:right-col t))
                                  cls)]
                        (if (and (contains? ops (:op t))
                                 (or (nil? coll) (= (:op t) :isnull)
                                     (and (aff-ok? (:cmp-aff t) idx-aff)
                                          (= (str/upper-case (or (:coll t) "BINARY")) (str/upper-case coll))))
                                 (not (and (eq-ops (:op t)) (:right-col t) (= (:right-col t) (first cls)))))
                          (recur (inc k) cls (conj out t))
                          (recur (inc k) cls out)))
                      (recur (inc k) cls out)))))]
          (recur cls (inc ei) out))))))

;; ====================================================================== loop generation

(defrecord Loop [tab self prereq setup run nout flags idx neq nbtm ntop nskip lterms sortidx])

(defn- loop-lesser
  "whereLoopFindLesser over the loop vector starting at position start: the index of the loop to replace, ::discard,
  or nil (append)."
  [loops start t]
  (loop [i start]
    (if (>= i (count loops))
      nil
      (let [p (nth loops i)]
        (cond
          (or (not= (:tab p) (:tab t)) (not= (:sortidx p) (:sortidx t))) (recur (inc i))
          (and (contains? (:flags p) :auto-index) (contains? (:flags t) :indexed) (contains? (:flags t) :column-eq)
               (= (bit-and (:prereq p) (:prereq t)) (:prereq t)))
          i
          ;; an exact tie keeps the loop found first (the newest index), except that a loop with more
          ;; equality columns replaces one with fewer
          (and (= (bit-and (:prereq t) (:prereq p)) (:prereq p))
               (<= (:run p) (:run t)) (<= (:nout p) (:nout t)) (<= (:setup p) (:setup t))
               (not (and (= (:run p) (:run t)) (= (:nout p) (:nout t)) (= (:setup p) (:setup t))
                         (> (count (:lterms t)) (count (:lterms p)))
                         (= (bit-and (:prereq p) (:prereq t)) (:prereq t)))))
          ::discard
          (and (= (bit-and (:prereq p) (:prereq t)) (:prereq t))   ; template has no more dependencies
               (>= (:run p) (:run t)) (>= (:nout p) (:nout t)))
          i
          :else (recur (inc i)))))))

(defn- loop-insert
  "whereLoopInsert: returns the new loop vector."
  [loops t]
  (let [i (loop-lesser loops 0 t)]
    (cond
      (= i ::discard) loops
      (nil? i) (conj loops t)
      :else
      ;; overwrite loops[i]; delete other entries supplanted by t
      (let [loops (assoc loops i t)]
        (loop [loops loops pos (inc i)]
          (let [j (loop-lesser loops pos t)]
            (cond (or (nil? j) (= j ::discard)) loops
                  :else (recur (vec (concat (subvec loops 0 j) (subvec loops (inc j)))) j))))))))

(defn- range-adjust [term n]
  (if term
    (cond (and (:truth term) (<= (long (:truth term)) 0)) (+ n (long (:truth term)))
          :else (- n 20))
    n))

(defn- range-scan-est
  "whereRangeScanEst without STAT4: returns the new nOut."
  [btm top nout]
  (let [nnew (range-adjust btm nout)
        nnew (range-adjust top nnew)
        nnew (if (and btm (or (nil? (:truth btm)) (pos? (long (:truth btm))))
                      top (or (nil? (:truth top)) (pos? (long (:truth top)))))
               (- nnew 20) nnew)
        nout (- nout (+ (if btm 1 0) (if top 1 0)))
        nnew (if (< nnew 10) 10 nnew)]
    (if (< nnew nout) nnew nout)))

(defn- term-mask-self [t] (bit (long (first (:left t)))))

(defn- output-adjust
  "whereLoopOutputAdjust: reduces nOut for WHERE terms the loop does not use."
  [P ^Loop lp nrow]
  (let [not-allowed (bit-not (bit-or (long (:prereq lp)) (long (:self lp))))
        ireduce (volatile! 0)
        nout (volatile! (long (:nout lp)))
        lterm-ids (set (keep :id (:lterms lp)))]
    (doseq [t (:terms P)
            :while (not (:virtual t))]
      (when (and (not (zero? (bit-and (long (:prereq-all t)) (long (:self lp)))))
                 (zero? (bit-and (long (:prereq-all t)) not-allowed))
                 (not (some #(and % (or (= (:id %) (:id t)) (and (:parent %) (= (:parent %) (:id t))))) (:lterms lp))))
        (if (and (:truth t) (<= (long (:truth t)) 0))
          (vswap! nout + (long (:truth t)))
          (do (vswap! nout dec)
              (when (and (eq-ops (:op t)) (not (:hightruth t)))
                (let [k (if (:int01 t) 10 20)]
                  (when (< @ireduce k) (vreset! ireduce k))))))))
    (let [lim (- (long nrow) (long @ireduce))]
      (assoc lp :nout (if (> @nout lim) lim @nout)))))

(defn- idx-aiRowLogEst [idx i]
  (let [l (:logest idx)] (if (< i (count l)) (long (nth l i)) 0)))

(defn- constraint-compatible-with-outer-join? [t tab]
  (= (:on-of t) (:i tab)))

(declare add-btree-index plan*)

(defn- add-btree-index
  "whereLoopAddBtreeIndex. B is a volatile builder {:loops [..]}; lp is the template loop; returns nil."
  [P B tab probe lp n-in-mul]
  (let [terms (:terms P)
        nkey (long (:nkey probe))
        ncol (long (:ncol probe))
        saved-neq (long (:neq lp))
        saved-nskip (long (:nskip lp))
        saved-nout (long (:nout lp))
        saved-flags (:flags lp)
        saved-nlterm (count (:lterms lp))
        saved-prereq (long (:prereq lp))
        rsize (idx-aiRowLogEst probe 0)
        rlogsize (est-log rsize)
        has-btm (contains? saved-flags :btm-limit)
        ops (cond-> (if has-btm #{:lt :le} #{:eq :in :gt :ge :lt :le :isnull :is})
              (:unordered probe) (disj :gt :ge :lt :le))
        pcol (when (< saved-neq (count (:cols probe))) (nth (:cols probe) saved-neq))
        ;; the column being constrained: ipk probe -> rowid
        col (cond (:ipk probe) -1
                  pcol (:c pcol)
                  :else -1)
        scan-coll (when (and pcol (not (:ipk probe)) (not= -1 col)) (:coll pcol))
        scan-aff (when (and pcol (not (:ipk probe)) (not= -1 col)) (:aff pcol))
        col-key (if (and pcol (:expr pcol)) {:expr (:expr pcol)} col)]
    ;; skip-scan
    (when (and (= saved-neq saved-nskip) (< (inc saved-neq) nkey) (= saved-neq saved-nlterm)
               (not (:noskip probe)) (:has-stat1 probe)
               (>= (idx-aiRowLogEst probe (inc saved-neq)) 42)
               (not (contains? saved-flags :top-limit)))
      (let [niter (- (idx-aiRowLogEst probe saved-neq) (idx-aiRowLogEst probe (inc saved-neq)))
            lp2 (assoc lp :neq (inc saved-neq) :nskip (inc saved-nskip)
                       :lterms (conj (:lterms lp) nil)
                       :flags (conj saved-flags :skipscan)
                       :nout (- saved-nout niter))]
        (add-btree-index P B tab probe lp2 (+ (+ niter 5) (long n-in-mul)))))
    (doseq [term (if (and (not= col-key -1) (map? col-key) (not pcol)) [] (scan-terms terms (:i tab) col-key (if (= col-key -1) (disj ops :isnull) ops) scan-coll scan-aff))]
      (let [eop (:op term)]
        (when-not (or (not (zero? (bit-and (long (:prereq-right term)) (long (:self lp)))))
                      (and (:likeopt term) (= eop :lt))
                      (and (seq (:jointype tab)) (some #{:left :right} (:jointype tab))
                           (not (constraint-compatible-with-outer-join? term tab))))
          (let [lp1 (assoc lp :neq saved-neq :nbtm (:nbtm lp) :ntop (:ntop lp)
                           :flags saved-flags :lterms (subvec (:lterms lp) 0 saved-nlterm))
                lp1 (assoc lp1 :lterms (conj (:lterms lp1) term)
                           :prereq (bit-and (bit-or saved-prereq (long (:prereq-right term))) (bit-not (long (:self lp)))))
                nin (volatile! 0)
                skip? (volatile! false)
                lp1
                (if (= eop :in)
                  (let [n-in (if (:select-in term) 46 (le (long (:nin term))))
                        n-in (if (and (:select-in term)
                                      (some #(and % (= (:expr-id %) (:expr-id term))) (butlast (:lterms lp1))))
                               0 n-in)]
                    (vreset! nin n-in)
                    (if (and (:has-stat1 probe) (>= rlogsize 10)
                             (let [m (idx-aiRowLogEst probe saved-neq)
                                   logk (est-log n-in)
                                   x (- (+ m logk 10) (+ n-in rlogsize))]
                               (< x 0)))
                      (do (vreset! skip? true) lp1)
                      (assoc lp1 :flags (conj (:flags lp1) :column-in))))
                  (cond
                    (contains? eq-ops eop)
                    (let [icol col
                          f (conj (:flags lp1) :column-eq)
                          f (if (or (= icol -1)
                                    (and (>= (long icol) 0) (zero? (long n-in-mul)) (= saved-neq (dec nkey))))
                              (if (or (= icol -1) (:uniq-not-null probe)
                                      (and (= nkey 1) (:onerror probe) (= eop :eq)))
                                (conj f :onerow)
                                (conj f :unq-wanted))
                              f)]
                      (assoc lp1 :flags f))
                    (= eop :isnull) (assoc lp1 :flags (conj (:flags lp1) :column-null))
                    (contains? #{:gt :ge} eop)
                    (let [l (assoc lp1 :flags (conj (:flags lp1) :column-range :btm-limit) :nbtm 1)]
                      (if (:likeopt term)
                        ;; LIKE range terms come in pairs: the next term is the upper bound
                        (let [top (nth terms (inc (long (:id term))))]
                          (-> l (update :lterms conj top) (update :flags conj :top-limit) (assoc :ntop 1)))
                        l))
                    :else
                    (assoc lp1 :flags (conj (:flags lp1) :column-range :top-limit) :ntop 1)))]
            (when-not @skip?
              (let [flags (:flags lp1)
                    range? (contains? flags :column-range)
                    ;; btm/top terms for the range estimate
                    lt (:lterms lp1)
                    [pbtm ptop]
                    (cond (not range?) [nil nil]
                          (contains? #{:gt :ge} eop)
                          [term (when (:likeopt term) (peek lt))]
                          :else [(when (contains? flags :btm-limit) (nth lt (- (count lt) 2))) term])
                    nin-v (if (pos? saved-neq) 0 (long @nin))
                    lp2
                    (if range?
                      (assoc lp1 :nout (range-scan-est pbtm ptop saved-nout))
                      (let [neq (inc saved-neq)
                            lp1 (assoc lp1 :neq neq)
                            nout (long (:nout lp1))
                            pc (when (< saved-neq (count (:cols probe))) (nth (:cols probe) saved-neq))
                            iscol (and pc (not (:ipk probe)) (>= (long (:c pc)) 0))
                            iscol (or iscol (and (:ipk probe) false))]
                        (if (and (:truth term) (<= (long (:truth term)) 0) (or (and pc (not (:ipk probe)) (>= (long (:c pc)) 0))
                                                                              (and pc (:expr pc) false)))
                          (assoc lp1 :nout (- (+ nout (long (:truth term))) nin-v))
                          (let [d (- (idx-aiRowLogEst probe neq) (idx-aiRowLogEst probe (dec neq)))
                                nout (+ nout d)
                                nout (if (= eop :isnull) (+ nout 10) nout)]
                            (assoc lp1 :nout nout)))))
                    nout-after (long (:nout lp2))
                    rcostidx (if (:ipk probe)
                               (+ nout-after 16)
                               (+ nout-after 1 (quot (* 15 (long (:szidx probe))) (long (:szrow tab)))))
                    run (le-add rlogsize rcostidx)
                    run (if (zero? (count (filter #{:idx-only :ipk :expridx} flags)))
                          (le-add run (+ nout-after 16))
                          run)
                    nout-unadj nout-after
                    run (+ run (long n-in-mul) nin-v)
                    lp3 (assoc lp2 :run run :nout (+ nout-after (long n-in-mul) nin-v) :setup 0)
                    lp3 (output-adjust P lp3 rsize)]
                (vswap! B update :loops loop-insert lp3)
                (let [lp-rec (assoc lp3 :nout (if range? saved-nout nout-unadj))]
                  (when (and (not (contains? (:flags lp-rec) :top-limit))
                             (< (long (:neq lp-rec)) ncol)
                             (or (< (long (:neq lp-rec)) nkey) (not (:pk-index probe))))
                    (add-btree-index P B tab probe lp-rec (+ (long n-in-mul) nin-v))))))))))))

(defn- term-covered? [t tab-i probe]
  (every? #(contains? (:colset probe) %) (get (:ucols t) tab-i)))

(defn- cover-lookup
  "Cost of table lookups for a non-covering index scan (the nLookup of whereLoopAddBtree)."
  [P tab probe rsize]
  (loop [ts (:terms P) n (+ (long rsize) 16)]
    (let [t (first ts)]
      (if (or (nil? t) (not (term-covered? t (:i tab) probe)))
        n
        (recur (rest ts)
               (if (and (:truth t) (<= (long (:truth t)) 0))
                 (+ n (long (:truth t)))
                 (+ (dec n) (if (eq-ops (:op t)) -19 0))))))))

(defn- add-btree
  "whereLoopAddBtree for one table. mprereq: prerequisite mask. Appends to B's loops."
  [P B tab mprereq]
  (let [i (:i tab)
        self (bit (long i))
        nrow-tab (long (:nrow tab))
        szrow (long (:szrow tab))
        ipk-probe {:ipk true :name nil :cols [{:c -1}] :nkey 1 :ncol 1 :unique true :uniq-not-null true
                   :logest [nrow-tab 0] :szidx 3 :onerror true}
        probes (cond
                 (:indexed-by tab) (let [ix (first (filter #(= (str/lower-case (:name %)) (str/lower-case (:indexed-by tab))) (:indexes tab)))]
                                     (when ix [ix]))
                 (:not-indexed tab) [ipk-probe]
                 :else (into [ipk-probe] (:indexes tab)))
        base (map->Loop {:tab i :self self :prereq mprereq :setup 0 :run 0 :nout nrow-tab :flags #{}
                         :idx nil :neq 0 :nbtm 0 :ntop 0 :nskip 0 :lterms [] :sortidx 0})
        rsize0 nrow-tab
        rlogsize0 (est-log rsize0)]
    ;; automatic indexes
    (when (and (:auto-ok tab) (not (:indexed-by tab)) (not (:not-indexed tab)) (:rowid? tab))
      (doseq [t (:terms P)]
        (when (and (= (first (:left t)) i) (eq-ops (:op t)) (>= (long (second (:left t))) 0) (:auto-ok t)
                   ;; an existing index led by this column (even one the term cannot use) suppresses the automatic index
                   (not (some (fn [ix] (and (not (:partial ix)) (= (:c (first (:cols ix))) (second (:left t))))) (:indexes tab)))
                   (or (empty? (filter #{:left :right} (:jointype tab))) (= (:on-of t) i)))
          (let [setup (+ rlogsize0 rsize0)
                setup (if (:ephemeral tab) (- setup 25) (+ setup 28))
                setup (max 0 setup)
                nout 43
                run (le-add rlogsize0 nout)
                lp (assoc base :neq 1 :nskip 0 :idx :auto :lterms [t] :setup setup :nout nout :run run
                          :flags #{:auto-index} :prereq (bit-or mprereq (long (:prereq-right t))))]
            (vswap! B update :loops loop-insert lp)))))
    (loop [ps probes sortidx 1]
      (when-let [probe (first ps)]
        (when-not (or (and (:partial probe) (not (:partial-ok probe))) (:bnoquery probe))
          (let [rsize (idx-aiRowLogEst probe 0)
                b? (:might-help-order probe)
                lp0 (assoc base :nout rsize :idx probe :sortidx 0)]
            (if (:ipk probe)
              ;; full table scan
              (let [lp (assoc lp0 :flags #{:ipk} :sortidx (if b? sortidx 0) :run (+ rsize 16))
                    lp (output-adjust P lp rsize)]
                (vswap! B update :loops loop-insert lp)
                (add-btree-index P B tab probe (assoc lp0 :flags #{:ipk}) 0))
              (let [covering (:covering probe)
                    flags (if covering #{:idx-only :indexed} #{:indexed})]
                ;; full scan via index
                (when (or b? (not (:rowid? tab)) (:partial probe) (:indexed-by tab)
                          (and covering (not (:unordered probe)) (< (long (:szidx probe)) szrow) (not (:no-cis P))))
                  (let [run (+ rsize 1 (quot (* 15 (long (:szidx probe))) szrow))
                        run (if-not covering
                              (le-add run (cover-lookup P tab probe rsize))
                              run)
                        lp (assoc lp0 :flags flags :sortidx (if b? sortidx 0) :run run)
                        lp (output-adjust P lp rsize)]
                    (vswap! B update :loops loop-insert lp)))
                (add-btree-index P B tab probe (assoc lp0 :flags flags) 0))))
          nil)
        (recur (rest ps) (inc sortidx))))))

(defn add-all-loops
  "whereLoopAddAll: returns the vector of WhereLoops."
  [P]
  (let [B (volatile! {:loops []})]
    (loop [ts (:tables P) mprior 0]
      (when-let [tab (first ts)]
        (let [mprereq (if (some #{:left :cross} (:jointype tab)) mprior 0)]
          (add-btree P B tab mprereq))
        (recur (rest ts) (bit-or mprior (bit (long (:i tab)))))))
    (:loops @B)))

;; ====================================================================== ORDER BY satisfaction

(defn- find-term
  "sqlite3WhereFindTerm: a term on (tbl col) with an operator in ops whose right side is available given 'ready'."
  [P tbl col ops ready]
  (first (filter (fn [t] (and (= (:left t) [tbl col]) (contains? ops (:op t))
                              (zero? (bit-and (long (:prereq-right t)) (bit-not (long ready))))))
                 (:terms P))))

(defn path-satisfies-order-by
  "wherePathSatisfiesOrderBy. path: vector of loops (outer to inner), nloop: number of loops of the path
  before pLast. Returns [n rev-mask]."
  [P order kind wflags path nloop last-loop]
  (let [nob (count order)]
    (if (> nob 62)
      [0 0]
      (let [obdone (dec (bit nob))
            eqop (cond-> #{:eq :is :isnull}
                   (contains? wflags :orderby-limit) (conj :in))
            groupby? (contains? #{:groupby :distinctby} kind)]
        (loop [iloop 0 obsat 0 distinct? true odm 0 ready 0 rev-mask 0 prev nil]
          (if-not (and distinct? (< obsat obdone) (<= iloop nloop))
            ;; finished
            (cond (= obsat obdone) [nob rev-mask]
                  (not distinct?) [(or (first (for [i (range (dec nob) 0 -1)
                                                    :let [m (dec (bit i))]
                                                    :when (= (bit-and obsat m) m)] i))
                                       0)
                                   rev-mask]
                  :else [-1 rev-mask])
            (let [ready (if (> iloop 0) (bit-or ready (long (:self prev))) ready)
                  lp (if (< iloop nloop) (nth path iloop) last-loop)]
              (if (and (< iloop nloop) (contains? wflags :orderby-limit))
                (recur (inc iloop) obsat distinct? odm ready rev-mask lp)
                (let [tab-i (:tab lp)
                      ;; mark off ORDER BY terms with X IS NULL / X=? on this loop's table
                      obsat
                      (reduce (fn [obsat i]
                                (if (not (zero? (bit-and (bit i) obsat)))
                                  obsat
                                  (let [o (nth order i)]
                                    (if (or (not= (:t o) tab-i) (nil? (:c o)))
                                      obsat
                                      (let [t (find-term P tab-i (:c o) eqop ready)]
                                        (cond
                                          (nil? t) obsat
                                          (and (= (:op t) :in) (not (some #(and % (= (:id %) (:id t))) (:lterms lp)))) obsat
                                          (and (contains? eq-ops (:op t)) (>= (long (:c o)) 0)
                                               (not= (str/upper-case (or (:coll o) "BINARY")) (str/upper-case (or (:coll t) "BINARY"))))
                                          obsat
                                          :else (bit-or obsat (bit i))))))))
                              obsat (range nob))
                      flags (:flags lp)
                      onerow? (contains? flags :onerow)
                      ;; one-row loops contribute nothing more
                      [obsat distinct? rev-mask]
                      (if onerow?
                        [obsat distinct? rev-mask]
                        (let [ipk? (contains? flags :ipk)
                              pidx (:idx lp)]
                          (cond
                            (and (not ipk?) (or (nil? pidx) (= pidx :auto) (:unordered pidx)))
                            [nil nil nil]   ; cannot be ordered
                            :else
                            (let [nkey (if ipk? 0 (long (:nkey pidx)))
                                  ncolumn (if ipk? 1 (long (:ncol pidx)))
                                  distinct0 (if ipk? distinct?
                                                (and (boolean (:unique pidx)) (not (contains? flags :skipscan))))
                                  neq (long (:neq lp)) nskip (long (:nskip lp))]
                              (loop [j 0 obsat obsat distinct? distinct0 rev nil distinct-cols false rev-mask rev-mask]
                                (if (>= j ncolumn)
                                  [obsat (if distinct-cols true distinct?) rev-mask]
                                  (let [skip-eq
                                        (when (and (< j neq) (>= j nskip))
                                          (let [t (nth (:lterms lp) j)
                                                eop (:op t)]
                                            (cond (contains? eqop eop) [:eq (contains? #{:isnull :is} eop)]
                                                  (= eop :in) [:in (boolean (some (fn [k] (and (> k j) (< k neq) (= (:expr-id (nth (:lterms lp) k)) (:expr-id t))))
                                                                                  (range (inc j) neq)))]
                                                  :else nil)))]
                                    (if (= :eq (first skip-eq))
                                      (recur (inc j) obsat (if (second skip-eq) false distinct?) rev distinct-cols rev-mask)
                                      (let [bonce? (not (and (= :in (first skip-eq)) (second skip-eq)))
                                            icol (if ipk? -1 (let [c (:c (nth (:cols pidx) j (nth (:cols pidx) (dec (count (:cols pidx))))))] c))
                                            icol (if (and (not ipk?) (>= j (count (:cols pidx)))) -1 icol)
                                            pc (when (and (not ipk?) (< j (count (:cols pidx)))) (nth (:cols pidx) j))
                                            rev-idx (boolean (when pc (:desc pc)))
                                            distinct? (if distinct?
                                                        (cond (and (>= (long icol) 0) (>= j neq) (not (:notnull pc))) false
                                                              (and pc (:expr pc)) false
                                                              :else distinct?)
                                                        distinct?)
                                            ;; find the matching ORDER BY term
                                            matched
                                            (loop [i 0 bonce bonce?]
                                              (cond
                                                (or (not bonce) (>= i nob)) nil
                                                (not (zero? (bit-and (bit i) obsat))) (recur (inc i) bonce)
                                                :else
                                                (let [o (nth order i)
                                                      bonce (if groupby? bonce false)
                                                      ok (and (if (and pc (:expr pc))
                                                                (and (:expr o) (= (:expr o) (:expr pc)) (= (:t o) tab-i))
                                                                (and (= (:t o) tab-i) (:c o) (= (long (:c o)) (long icol))))
                                                              (or (= icol -1)
                                                                  (= (str/upper-case (or (:coll o) "BINARY")) (str/upper-case (or (:coll pc) "BINARY")))))]
                                                  (if ok i (recur (inc i) bonce)))))
                                            ;; sort direction compatibility
                                            [matched rev rev-mask]
                                            (if (and matched (not (= kind :groupby)))
                                              (let [odesc (boolean (:desc (nth order matched)))]
                                                (if (some? rev)
                                                  (if (not= (not= rev rev-idx) odesc) [nil rev rev-mask] [matched rev rev-mask])
                                                  (let [r (not= rev-idx odesc)]
                                                    [matched r (if r (bit-or rev-mask (bit iloop)) rev-mask)])))
                                              [matched rev rev-mask])
                                            matched (if (and matched (:bignull (nth order matched)))
                                                      (if (= j neq) matched nil)
                                                      matched)]
                                        (if matched
                                          (recur (inc j) (bit-or obsat (bit matched)) distinct?
                                                 rev (or distinct-cols (= icol -1)) rev-mask)
                                          [obsat (if (or (= j 0) (< j nkey)) false distinct?) rev-mask]))))))))))
                      ]
                  (if (nil? obsat)
                    [0 0]
                    ;; mark off other ORDER BY terms that depend only on order-distinct loops
                    (let [odm (if distinct? (bit-or odm (long (:self lp))) odm)
                          obsat (if distinct?
                                  (reduce (fn [obsat i]
                                            (if (not (zero? (bit-and (bit i) obsat)))
                                              obsat
                                              (let [o (nth order i)
                                                    m (long (or (:other-mask o) 0))]
                                                (if (and (zero? m) (not (:const o)))
                                                  obsat
                                                  (if (zero? (bit-and m (bit-not odm))) (bit-or obsat (bit i)) obsat)))))
                                          obsat (range nob))
                                  obsat)]
                      (recur (inc iloop) obsat distinct? odm ready rev-mask lp))))))))))))

;; ====================================================================== path solver

(defn- sorting-cost [P nrow nob nsorted]
  (let [ncol (le (quot (+ (long (:ncols P)) 59) 30))
        r (+ (long nrow) ncol)
        r (if (pos? nsorted) (+ r (- (le (quot (* (- nob nsorted) 100) nob)) 66)) r)
        nrow2 (cond (and (:limit P) (< (long (:limit P)) (long nrow))) (long (:limit P))
                    (:want-distinct P) (if (> (long nrow) 10) (- (long nrow) 10) (long nrow))
                    :else (long nrow))]
    (+ r (est-log nrow2))))

(defn- recompute-mx
  "mxI/mxCost/mxUnsorted of the best-so-far set (SQLite initialises mxUnsorted from nRow, a quirk kept here)."
  [t]
  (loop [k 1 best {:idx 0 :rcost (:rcost (nth t 0)) :unsorted (:nrow (nth t 0))}]
    (if (>= k (count t))
      best
      (let [pt (nth t k)]
        (recur (inc k)
               (if (or (> (long (:rcost pt)) (long (:rcost best)))
                       (and (= (long (:rcost pt)) (long (:rcost best))) (> (long (:runsorted pt)) (long (:unsorted best)))))
                 {:idx k :rcost (:rcost pt) :unsorted (:runsorted pt)}
                 best))))))

(defn- solve
  "wherePathSolver. nrow-est 0 ignores the ORDER BY. Returns {:path [loops] :nrow :rcost :ordered :rev}."
  [P loops nrow-est]
  (let [nloop (count (:tables P))
        mx-choice (cond (<= nloop 1) 1 (= nloop 2) 5 :else 10)
        order (:order P)
        nob (if (or (empty? order) (zero? (long nrow-est))) 0 (count order))
        wflags (or (:wflags P) #{})
        kind (:order-kind P)
        sort-cost (volatile! {})
        init {:path [] :mask 0 :nrow (min (long (or (:nquery-loop P) 0)) 48) :rcost 0 :runsorted 0
              :ordered (if (pos? nob) -1 0) :rev 0}]
    (loop [iloop 0 from [init]]
      (if (>= iloop nloop)
        (reduce (fn [b x] (if (> (long (:rcost b)) (long (:rcost x))) x b)) (first from) (rest from))
        (let [to (volatile! [])
              mx (volatile! nil)]
          (doseq [pf from
                  lp loops]
            (when-not (or (not (zero? (bit-and (long (:prereq lp)) (bit-not (long (:mask pf))))))
                          (not (zero? (bit-and (long (:self lp)) (long (:mask pf)))))
                          (and (contains? (:flags lp) :auto-index) (< (long (:nrow pf)) 3)))
              (let [ru (le-add (long (:setup lp)) (+ (long (:run lp)) (long (:nrow pf))))
                    ru (le-add ru (long (:runsorted pf)))
                    nout (+ (long (:nrow pf)) (long (:nout lp)))
                    mask-new (bit-or (long (:mask pf)) (long (:self lp)))
                    [ordered rev] (if (< (long (:ordered pf)) 0)
                                    (path-satisfies-order-by P order kind wflags (:path pf) iloop lp)
                                    [(:ordered pf) (:rev pf)])
                    [rcost ru] (if (and (>= (long ordered) 0) (< (long ordered) nob))
                                 (let [sc (or (get @sort-cost ordered)
                                              (let [c (sorting-cost P nrow-est nob ordered)] (vswap! sort-cost assoc ordered c) c))]
                                   [(+ (long (le-add ru sc)) 3) ru])
                                 [ru (- ru 2)])
                    cand {:path (conj (:path pf) lp) :mask mask-new :nrow nout :rcost rcost :runsorted ru
                          :ordered ordered :rev rev}
                    jj (first (keep-indexed (fn [k pt] (when (and (= (:mask pt) mask-new)
                                                                   (= (neg? (long (:ordered pt))) (neg? (long ordered))))
                                                          k))
                                            @to))]
                (if (nil? jj)
                  (when-not (and (>= (count @to) mx-choice)
                                 (let [{mc :rcost mu :unsorted} @mx]
                                   (or (> rcost (long mc)) (and (= rcost (long mc)) (>= ru (long mu))))))
                    (if (< (count @to) mx-choice)
                      (vswap! to conj cand)
                      (vswap! to assoc (:idx @mx) cand))
                    (when (>= (count @to) mx-choice)
                      (vreset! mx (recompute-mx @to))))
                  (let [pt (nth @to jj)]
                    (when-not (or (< (long (:rcost pt)) rcost)
                                  (and (= (long (:rcost pt)) rcost) (<= (long (:nrow pt)) nout)))
                      (vswap! to assoc jj cand)
                      (when (>= (count @to) mx-choice)
                        (vreset! mx (recompute-mx @to)))))))))
          (recur (inc iloop) @to))))))

(defn- short-cut
  "whereShortCut: a single table with an equality on the rowid or on all columns of a unique index."
  [P]
  (let [tab (first (:tables P))
        i (:i tab)
        self (bit (long i))
        base (map->Loop {:tab i :self self :prereq 0 :setup 0 :run 0 :nout 1 :flags #{} :idx nil :neq 0 :nbtm 0 :ntop 0
                         :nskip 0 :lterms [] :sortidx 0})]
    (when (and (not (:indexed-by tab)) (not (:not-indexed tab)) (not (:virtual tab)))
      (if-let [t (first (filter #(zero? (long (:prereq-right %)))
                                (scan-terms (:terms P) i -1 #{:eq :is} nil nil)))]
        (assoc base :flags #{:column-eq :ipk :onerow} :lterms [t] :neq 1 :run 33)
        (first
         (for [ix (:indexes tab)
               :when (and (:unique ix) (not (:partial ix)) (<= (long (:nkey ix)) 3))
               :let [opmask (if (:uniq-not-null ix) #{:eq :is} #{:eq})
                     ts (for [j (range (:nkey ix))]
                          (let [c (nth (:cols ix) j)]
                            (first (filter #(zero? (long (:prereq-right %)))
                                           (scan-terms (:terms P) i (if (:expr c) {:expr (:expr c)} (:c c)) opmask (:coll c) (:aff c))))))]
               :when (every? some? ts)]
           (assoc base :flags (cond-> #{:column-eq :onerow :indexed} (:covering ix) (conj :idx-only))
                  :idx ix :lterms (vec ts) :neq (count ts) :run 39)))))))

(defn plan
  "Chooses the loops for problem P. Returns {:path [loop ...] :nrow-out n :ordered n :rev-mask m} or nil."
  [P]
  (let [P (assoc P :terms (vec (map-indexed (fn [i t] (assoc t :id i)) (:terms P))))
        sc (when (= 1 (count (:tables P))) (short-cut P))]
    (if sc
      {:path [sc] :nrow-out 1 :ordered (count (:order P)) :rev-mask 0 :rcost 0 :loops [sc]}
      (plan* P))))

(defn- plan*
  [P]
  (let [loops (add-all-loops P)
        r0 (solve P loops 0)
        r (if (seq (:order P))
            (solve P loops (inc (long (:nrow r0))))
            r0)]
    {:path (:path r) :nrow-out (:nrow r0) :ordered (:ordered r) :rev-mask (:rev r) :rcost (:rcost r)
     :loops loops}))

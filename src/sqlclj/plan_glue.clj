(ns sqlclj.plan-glue
  "Builds planner problems (sqlclj.planner) from the SQL AST of a SELECT core, and turns plans into
  EXPLAIN QUERY PLAN rows and scan orders."
  (:require [clojure.string :as str]
            [clojure.walk]
            [sqlclj.engine :as e]
            [sqlclj.planner :as pl]
            [sqlclj.values :as v]))

(def ^:private lc e/lc)

(defn- aff-class [a]
  (case a (:integer :real :numeric) :numeric :text :text :none))

(defn- bit ^long [^long i] (bit-shift-left 1 i))

;; ---------------------------------------------------------------------- expression helpers

(defn- unwrap-likely
  "Strips likely()/unlikely()/likelihood() wrappers; returns [expr truth-or-nil]."
  [x]
  (if (and (map? x) (= :fn (:op x)) (#{"likely" "unlikely" "likelihood"} (:name x)) (seq (:args x)))
    (let [[a p] (:args x)
          prob (case (:name x)
                 "likely" 0.9375 "unlikely" 0.0625
                 (when (and p (= :lit (:op p)) (#{:int :float} (:kind p)))
                   (let [d (Double/parseDouble (:text p))] (max 0.0 (min 1.0 d)))))
          truth (when prob (- (pl/le (long (* prob 134217728.0))) 270))]
      [a truth])
    [x nil]))

(defn- strip-paren [x] x)

(defn- col-ref
  "[tbl-index col] for a plain column reference of the query's own tables (col -1 = rowid), else nil."
  [G x]
  (when (map? x)
    (case (:op x)
      :colref (when-let [ti (get (:slot->i G) (:slot x))] [ti (:idx x)])
      :col (try
             (let [r (e/resolve-column (:ctx G) (:table x) (:name x) nil)]
               (when (and (zero? (long (:depth r))) (:slot r))
                 (when-let [ti (get (:slot->i G) (:slot r))]
                   (let [src (nth (:sources G) ti)
                         i (:idx r)]
                     [ti (cond (>= i (long (:ncols src))) -1
                               (and (:table src) (= i (:ipk (:table src)))) -1
                               :else i)]))))
             (catch clojure.lang.ExceptionInfo _ nil))
      nil)))

(defn- explicit-coll
  "Explicit COLLATE name at the top of an expression (through unary plus), or nil."
  [x]
  (when (map? x)
    (case (:op x)
      :collate (:coll x)
      nil)))

(defn- expr-tables
  "Bitmask of the query's tables an expression refers to."
  [G x]
  (let [m (volatile! 0)]
    (letfn [(walk [x]
              (cond
                (map? x)
                (cond
                  (#{:col :colref} (:op x)) (when-let [[ti _] (col-ref G x)] (vswap! m bit-or (bit (long ti))))
                  :else (doseq [[k vv] x :when (not (#{:ps :pe} k))] (walk vv)))
                (sequential? x) (doseq [y x] (walk y))))]
      (walk x))
    @m))

(defn- col-aff [G [ti c]]
  (let [src (nth (:sources G) ti)]
    (if (and (>= (long c) 0) (< (long c) (long (:ncols src)))) (nth (:affs src) c) :integer)))

(defn- col-coll [G [ti c]]
  (let [src (nth (:sources G) ti)]
    (if (and (>= (long c) 0) (< (long c) (long (:ncols src)))) (or (nth (:colls src) c) "BINARY") "BINARY")))

(defn- expr-aff
  "sqlite3ExprAffinity: affinity keyword of a column/cast expression, nil otherwise."
  [G x]
  (cond
    (not (map? x)) nil
    (#{:col :colref} (:op x)) (when-let [r (col-ref G x)] (if (= -1 (second r)) :integer (col-aff G r)))
    (= :collate (:op x)) (expr-aff G (:e x))
    (= :cast (:op x)) (v/type-affinity (:type x))
    :else nil))

(defn- cmp-affinity
  "comparisonAffinity: :none :text or :numeric for the comparison l op r."
  [G l r]
  (let [a1 (expr-aff G l) a2 (expr-aff G r)
        num? (fn [a] (#{:integer :real :numeric} a))]
    (cond
      (and a1 a2)
      (if (or (num? a1) (num? a2)) :numeric :none)
      :else (let [a (or a1 a2)] (aff-class a)))))

(defn- cmp-coll
  "sqlite3ExprCompareCollSeq: explicit COLLATE on the left, else on the right, else the left column's
  collation, else the right column's, else BINARY."
  [G l r]
  (or (some-> (explicit-coll l) str/upper-case)
      (some-> (explicit-coll r) str/upper-case)
      (let [l' (if (= :collate (:op l)) (:e l) l) r' (if (= :collate (:op r)) (:e r) r)]
        (or (when-let [c (col-ref G l')] (some-> (col-coll G c) str/upper-case))
            (when-let [c (col-ref G r')] (some-> (col-coll G c) str/upper-case))))
      "BINARY"))

(defn- commute-op [o] (case o :lt :gt :gt :lt :le :ge :ge :le o))

(defn- null-lit? [x] (and (map? x) (= :lit (:op x)) (nil? (:kind x)) (nil? (:v x))))

(defn- int01? [x]
  (let [x (if (and (map? x) (= :unary (:op x)) (= :plus (:o x))) (:e x) x)]
    (cond (and (map? x) (= :lit (:op x)) (= :int (:kind x))) (<= 0 (Long/parseLong (:text x)) 1)
          (and (map? x) (= :unary (:op x)) (= :neg (:o x)) (= :lit (:op (:e x))) (= :int (:kind (:e x))))
          (= 1 (Long/parseLong (:text (:e x))))
          :else false)))

(defn- const-expr? [x] (e/const-term? x))

;; ---------------------------------------------------------------------- terms

(defn- new-term [G expr & {:as m}]
  (merge {:expr expr :virtual false :on-of nil :truth nil :prereq-all (expr-tables G expr)} m))

(defn- like-info
  "For x LIKE/GLOB pattern with a constant string prefix: {:coll :lower :upper} or nil."
  [G e]
  (let [l (:e e) p (:pat e) esc (:esc e)]
    (when (and (not (:not e)) (nil? esc) (map? p) (= :lit (:op p)) (string? (:v p)))
      (let [lr (col-ref G l)]
        (when (and lr (>= (long (second lr)) 0) (= :text (aff-class (col-aff G lr))))
          (let [glob? (= :glob (:o e))
                z (:v p)
                wcs (if glob? #{\* \? \[} #{\% \_})
                cnt (count (take-while #(not (wcs %)) z))]
            (when (pos? cnt)
              {:coll (if glob? "BINARY" (if (:case-sensitive-like G) "BINARY" "NOCASE"))
               :prefix (subs z 0 cnt)
               :left lr})))))))

(defn analyze-where
  "Builds the planner's term vector from the WHERE/ON conjuncts. conjuncts: [{:expr :on-of}]."
  [G conjuncts]
  (let [terms (volatile! [])
        insert! (fn [t] (vswap! terms conj t) (dec (count @terms)))
        ;; analyze term idx (exprAnalyze)
        analyze!
        (fn analyze! [idx]
          (let [t (nth @terms idx)
                [x truth0] (unwrap-likely (:expr t))
                t (cond-> t truth0 (assoc :truth truth0))
                set-term! (fn [t] (vswap! terms assoc idx t))
                add-copy! (fn [t'] (insert! t'))]
            (set-term! t)
            (case (:op x)
              :bin
              (let [o (:o x)]
                (when (#{:eq :lt :le :gt :ge} o)
                  (let [l (:l x) r (:r x)
                        l* (if (= :collate (:op l)) l l)
                        lcol (col-ref G (if (= :collate (:op l)) (:e l) l))
                        rcol (col-ref G (if (= :collate (:op r)) (:e r) r))
                        lcol (when (and lcol (not (:no-collate-left G))) lcol)
                        pl-mask (expr-tables G l) pr-mask (expr-tables G r)
                        coll (cmp-coll G l r)
                        aff (cmp-affinity G l r)
                        i01 (int01? r)
                        equiv? (and (= o :eq) lcol rcol (nil? (:on-of t))
                                    (let [a1 (expr-aff G l) a2 (expr-aff G r)
                                          num? #(#{:integer :real :numeric} %)]
                                      (or (= a1 a2) (and (num? a1) (num? a2))))
                                    (let [c (cmp-coll G l r)]
                                      (or (= c "BINARY") (= (some-> (col-coll G lcol) str/upper-case) (some-> (col-coll G rcol) str/upper-case)))))
                        base (fn [op left right-col pr]
                               (assoc t :op op :left left :right-col right-col :prereq-right pr :coll coll :cmp-aff aff
                                      :int01 i01 :equiv (and equiv? true)
                                      :auto-ok (and left (>= (long (second left)) 0)
                                                    (pl/aff-ok? aff (aff-class (col-aff G left))))))]
                    (cond
                      lcol
                      (do (set-term! (base o lcol rcol pr-mask))
                          (when rcol
                            (add-copy! (-> (base (commute-op o) rcol lcol pl-mask)
                                           (assoc :virtual true :parent idx :copy true)))))
                      rcol
                      (set-term! (base (commute-op o) rcol nil pl-mask))))))
              :is
              (when-not (:not x)
                (let [l (:l x) r (:r x)]
                  (if (null-lit? r)
                    (when-let [lcol (col-ref G (if (= :collate (:op l)) (:e l) l))]
                      (set-term! (assoc t :op :isnull :left lcol :prereq-right 0 :coll nil :cmp-aff :none)))
                    (let [lcol (col-ref G (if (= :collate (:op l)) (:e l) l))
                          rcol (col-ref G (if (= :collate (:op r)) (:e r) r))
                          pl-mask (expr-tables G l) pr-mask (expr-tables G r)
                          coll (cmp-coll G l r) aff (cmp-affinity G l r)
                          base (fn [left right-col pr]
                                 (assoc t :op :is :left left :right-col right-col :prereq-right pr :coll coll :cmp-aff aff
                                        :auto-ok (and left (>= (long (second left)) 0)
                                                      (pl/aff-ok? aff (aff-class (col-aff G left))))))]
                      (cond lcol (do (set-term! (base lcol rcol pr-mask))
                                     (when rcol (add-copy! (-> (base rcol lcol pl-mask) (assoc :virtual true :parent idx :copy true)))))
                            rcol (set-term! (base rcol nil pl-mask)))))))
              :isnull
              (when-let [lcol (col-ref G (if (= :collate (:op (:e x))) (:e (:e x)) (:e x)))]
                (set-term! (assoc t :op :isnull :left lcol :prereq-right 0 :coll nil :cmp-aff :none)))
              :in
              (when (and (not (:not x)) (or (:list x) (:select x)))
                (let [l (:e x)
                      lcol (col-ref G (if (= :collate (:op l)) (:e l) l))]
                  (when lcol
                    (if (and (:list x) (= 1 (count (:list x))) (const-expr? (first (:list x))))
                      ;; x IN (const) is x = +const
                      (let [r (first (:list x))]
                        (set-term! (assoc t :op :eq :left lcol :prereq-right 0 :coll (cmp-coll G l r)
                                          :cmp-aff (cmp-affinity G l nil)
                                          :auto-ok (>= (long (second lcol)) 0))))
                      (let [pr (expr-tables G (or (:list x) (:select x)))]
                        (set-term! (assoc t :op :in :left lcol :prereq-right pr :coll (cmp-coll G l nil)
                                          :cmp-aff (cmp-affinity G l (when (:list x) (first (:list x))))
                                          :select-in (boolean (:select x))
                                          :nin (count (:list x)) :expr-id idx)))))))
              :between
              (when (and (not (:not x)))
                (let [l (:e x)]
                  (doseq [[o r] [[:ge (:lo x)] [:le (:hi x)]]]
                    (let [idx2 (insert! (new-term G {:op :bin :o o :l l :r r} :virtual true :parent idx :on-of (:on-of t)))]
                      (analyze! idx2)))))
              :like
              (when-let [li (like-info G x)]
                (let [l (:e x)
                      lr (:left li)
                      pre (:prefix li)
                      ;; the lower and upper bound terms
                      mk (fn [o]
                           (new-term G {:op :bin :o o :l (assoc {:op :collate :e l} :coll (:coll li)) :r {:op :lit :v pre}}
                                     :virtual true :parent idx :on-of (:on-of t) :likeopt true))
                      i1 (insert! (mk :ge))
                      _ (analyze! i1)
                      i2 (insert! (mk :lt))
                      _ (analyze! i2)]
                  nil))
              nil)))]
    (doseq [c conjuncts]
      (let [[x truth] (unwrap-likely (:expr c))]
        (insert! (new-term G (:expr c) :on-of (:on-of c) :truth truth))))
    (let [nbase (count @terms)]
      (doseq [i (range (dec nbase) -1 -1)] (analyze! i))
      ;; term-level default
      (vec (map-indexed (fn [i t] (assoc t :id i)) @terms)))))

(defn ucols
  "Map table-index -> set of its columns (>= 0) an expression refers to."
  [G x]
  (let [m (volatile! {})]
    (letfn [(walk [x]
              (cond
                (map? x)
                (if (#{:col :colref} (:op x))
                  (when-let [[ti c] (col-ref G x)] (when (>= (long c) 0) (vswap! m update ti (fnil conj #{}) c)))
                  (doseq [[k vv] x :when (not (#{:ps :pe} k))] (walk vv)))
                (sequential? x) (doseq [y x] (walk y))))]
      (walk x))
    @m))

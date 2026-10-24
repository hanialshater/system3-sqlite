(ns sqlclj.engine
  "Compiles parsed statements into closures over an Object[] environment and runs them against an
  immutable database value."
  (:require [clojure.string :as str]
            [clojure.walk]
            [sqlclj.values :as v]
            [sqlclj.num :as num]
            [sqlclj.lexer :as lx]
            [sqlclj.parser :as p]
            [sqlclj.ext :as ext]
            [sqlclj.schema :as schema])
  (:import [sqlclj.values Blob]
           [java.util TreeMap TreeSet Comparator ArrayList]))

(set! *warn-on-reflection* true)

(defn err [msg] (throw (lx/sql-error msg)))

(defn lc ^String [s] (when s (str/lower-case s)))

;; ====================================================================== compile context

(defrecord Ctx [db sql scope slots aggs alias-cols agg-ok])
;; scope: {:sources [...] :parent scope-or-nil}
;; source: {:alias lc :tname :cols [names] :lcols [lc] :affs :colls :slot :rowid? :hidden #{} :ncols}

(defn new-slot! [ctx] (let [a (:slots ctx)] (swap! a inc) (dec @a)))

(defn text-of-node [ctx e] (subs (:sql ctx) (:ps e) (:pe e)))

(def agg-names #{"count" "sum" "total" "avg" "min" "max" "group_concat" "string_agg"})

(defn agg-call? [e]
  (and (= :fn (:op e)) (not (:over e))
       (let [n (:name e) k (count (:args e))]
         (case n
           ("min" "max") (and (= k 1) (not (:star e)))
           "count" (or (:star e) (<= k 1))
           ("sum" "total" "avg") true
           ("group_concat" "string_agg") true
           false))))

;; ====================================================================== name resolution

(defn- find-in-source [src lname]
  (let [i (.indexOf ^java.util.List (:lcols src) lname)]
    (when (>= i 0) i)))

(defn- rowid-name? [lname] (#{"rowid" "oid" "_rowid_"} lname))

(defn resolve-column
  "Returns {:slot :idx :aff :coll :depth :src} or throws."
  [ctx tq name dq]
  (let [lname (lc name) ltq (lc tq)]
    (loop [scope (:scope ctx) depth 0]
      (if (nil? scope)
        (cond
          (and (nil? tq) (= lname "true")) {:const 1}
          (and (nil? tq) (= lname "false")) {:const 0}
          :else
          (err (str "no such column: "
                    (if tq (str tq "." name) (if dq (str "\"" name "\" - should this be a string literal in single-quotes?") name)))))
        (let [srcs (:sources scope)
              hits (if ltq
                     (concat
                      (for [s srcs :when (= (:alias s) ltq)
                            :let [i (find-in-source s lname)
                                  i (or i (when (and (rowid-name? lname) (:rowid? s)) (:ncols s)))]
                            :when i]
                        [s i])
                      (for [s srcs :when (and (:quals s) (not= (:alias s) ltq))
                            :let [i (get-in (:quals s) [ltq lname])]
                            :when i]
                        [s i]))
                     (for [s srcs
                           :let [i (find-in-source s lname)]
                           :when (and i (not (:qualified-only s)) (not (contains? (:hidden s) lname)))]
                       [s i]))
              hits (if (and (empty? hits) (nil? ltq) (rowid-name? lname))
                     (let [rs (filter :rowid? srcs)]
                       (if (= 1 (count rs)) [[(first rs) (:ncols (first rs))]] (if (seq rs) [[(first rs) (:ncols (first rs))]] [])))
                     hits)]
          (cond
            (> (count hits) 1)
            (err (str "ambiguous column name: " (if tq (str tq "." name) name)))
            (= 1 (count hits))
            (let [[s i] (first hits)]
              (when-let [refs (:refs ctx)] (swap! refs conj [(:slot s) i]))
              {:slot (:slot s) :idx i :depth depth :src s
               :alt (when (and (nil? ltq) (< i (:ncols s))) (get (:coalesce s) lname))
               :nocoll (and (:table s) (or (= i (:ncols s)) (= i (:ipk (:table s)))))
               :aff (if (< i (:ncols s)) (nth (:affs s) i) :integer)
               :coll (if (< i (:ncols s)) (nth (:colls s) i) nil)})
            (and ltq (some #(or (= (:alias %) ltq) (get-in % [:quals ltq])) srcs))
            (err (str "no such column: " tq "." name))
            :else (recur (:parent scope) (inc depth))))))))

;; ====================================================================== expressions

(declare cx compile-select run-select-rows)

(defn- bool->int [b] (if b 1 0))

(defn lit-value [e]
  (case (:kind e)
    :int (let [t (:text e)
               [iv rc] (num/atoi64 t)]
           (if (= 0 (long rc)) iv (first (num/atof t))))
    :float (first (num/atof (:text e)))
    :hex (let [h (subs (:text e) 2)
               h2 (str/replace h #"^0+" "")]
           (if (> (count h2) 16)
             (err (str "hex literal too big: " (:text e)))
             (if (empty? h2) 0 (Long/parseUnsignedLong h2 16))))
    :blob (let [h (:text e)
                n (quot (count h) 2)
                b (byte-array n)]
            (dotimes [i n] (aset b i (unchecked-byte (Integer/parseInt (subs h (* 2 i) (+ 2 (* 2 i))) 16))))
            (Blob. b))
    (:v e)))

(defn- strip-paren [e] e)

(defn- neg-literal
  "Value of -literal, or ::none if e is not a numeric literal."
  [e]
  (if (= :lit (:op e))
    (case (:kind e)
      :int (let [t (:text e)
                 [iv rc] (num/atoi64 (str "-" t))]
             (if (or (= 0 (long rc))) iv (- (double (first (num/atof t))))))
      :float (- (double (first (num/atof (:text e)))))
      :hex (let [x (lit-value e)] (v/arith :sub 0 x))
      ::none)
    ::none))

(defn const-c [val] {:f (fn [_] val) :const true :val val})

(defn- mk [f & {:as m}] (merge {:f f} m))

(defn- has-collate? [c] (boolean (:ecoll c)))

(defn- coll-walk
  "Collation of a compiled binary/function node per sqlite3ExprCollSeq."
  [parts]
  (when-let [p (first (filter has-collate? parts))]
    (:coll p)))

(defn binary-coll [l r]
  (cond (has-collate? l) (:coll l)
        (has-collate? r) (:coll r)
        :else (or (:coll l) (:coll r))))

(def valid-colls #{"BINARY" "NOCASE" "RTRIM"})

(defn check-coll [c]
  (let [u (str/upper-case c)]
    (if (valid-colls u) u (err (str "no such collation sequence: " c)))))

(defn- compare-fn [op]
  (case op
    :eq (fn [^long c] (= c 0)) :ne (fn [^long c] (not= c 0))
    :lt (fn [^long c] (< c 0)) :le (fn [^long c] (<= c 0))
    :gt (fn [^long c] (> c 0)) :ge (fn [^long c] (>= c 0))))

(defn make-compare
  "Compiled comparison between compiled exprs l and r."
  [op l r]
  (let [aff (v/compare-affinity (:aff l) (:aff r))
        coll (or (binary-coll l r) "BINARY")
        fl (:f l) fr (:f r)
        test (compare-fn op)]
    (mk (fn [env]
          (let [a (fl env)]
            (if (nil? a)
              (do (fr env) nil)
              (let [b (fr env)]
                (if (nil? b) nil
                    (let [[a b] (v/comparison-coerce a b aff)]
                      (bool->int (test (v/sql-cmp a b coll)))))))))
        :ecoll (or (has-collate? l) (has-collate? r))
        :coll (coll-walk [l r]))))

(defn compare-values
  "Runtime comparison of two values with affinity/collation info (used by CASE, IN)."
  [a b aff coll]
  (let [[a b] (v/comparison-coerce a b aff)]
    (v/sql-cmp a b coll)))

(declare compile-function)

(declare cx* const-term?)

(defn cx
  "Compiles expression AST e in ctx. Returns {:f (fn [env]) :aff :coll :ecoll ...}.
  At the top of an expression, errors recorded during resolution are raised afterwards."
  [ctx e]
  (if (:pending ctx)
    (cx* ctx e)
    (let [pend (atom nil)
          r (cx* (assoc ctx :pending pend) e)]
      (when @pend (err @pend))
      r)))

(defn- null-lit? [e] (and (= :lit (:op e)) (nil? (:kind e)) (nil? (:v e))))

(defn- skip-collate-likely [e]
  (cond (= :collate (:op e)) (recur (:e e))
        (and (= :fn (:op e)) (#{"likely" "unlikely" "likelihood"} (:name e)) (seq (:args e))) (recur (first (:args e)))
        :else e))

(defn- is-rfirst
  "For IS / IS NOT, SQLite resolves a bare-identifier right operand (looking through COLLATE) first."
  [e]
  (when (= :is (:op e))
    (let [r (skip-collate-likely (:r e))]
      (when (and (= :col (:op r)) (nil? (:table r))) r))))

(defn- null-test-operand [e]
  (when (or (#{:isnull :notnull} (:op e)) (and (= :is (:op e)) (null-lit? (:r e))))
    (if (= :is (:op e)) (:l e) (:e e))))

(defn- nonnull-literal? [e]
  (let [e (loop [e e] (if (and (= :unary (:op e)) (#{:neg :pos} (:o e))) (recur (:e e)) e))]
    (and (= :lit (:op e)) (not (null-lit? e)))))

(defn- null-test?
  "IS NULL / NOT NULL: SQLite resolves the operand in a nested walk whose errors do not stop the outer
  walk; only an operand folded to a constant (a non-NULL literal) lets a pending error abort."
  [e]
  (when-let [o (null-test-operand e)]
    (not (nonnull-literal? o))))

(defn condf
  "The function to use when an expression is a condition (WHERE, ON, HAVING, CASE WHEN, CHECK): AND/OR
  short-circuit there, while in value context both operands are evaluated."
  [c] (or (:cft c) (:f c)))

(defn condff
  "Condition function when deciding whether an expression is FALSE (CHECK constraints, under NOT)."
  [c] (or (:cff c) (:f c)))

(defn- tvl "truth value: nil, 0 or 1" [x] (when-not (nil? x) (if (v/bool-value x) 1 0)))
(defn- falsy? [x] (and (not (nil? x)) (not (v/bool-value x))))
(defn- truthy? [x] (and (not (nil? x)) (v/bool-value x)))

(defn- and-cond
  "AND in condition context. mode :t decides TRUE (NULL left short-circuits like FALSE);
  mode :f decides FALSE (only a FALSE left short-circuits)."
  [fa fb mode]
  (if (= mode :t)
    (fn [env] (let [a (fa env)]
                (if (or (nil? a) (falsy? a)) 0
                    (tvl (fb env)))))
    (fn [env] (let [a (fa env)]
                (if (falsy? a) 0
                    (let [b (fb env)]
                      (cond (falsy? b) 0 (or (nil? a) (nil? b)) nil :else 1)))))))

(defn- or-cond
  [fa fb mode]
  (if (= mode :t)
    (fn [env] (let [a (fa env)]
                (if (truthy? a) 1
                    (let [b (fb env)]
                      (cond (truthy? b) 1 (or (nil? a) (nil? b)) nil :else 0)))))
    (fn [env] (let [a (fa env)]
                (if (or (nil? a) (truthy? a)) (tvl a)
                    (tvl (fb env)))))))

(defn- contained
  "Compiles e; a resolution error is recorded instead of thrown (SQLite resolves the operand of
  IS NULL / NOT NULL with a nested walk whose failure does not stop the outer walk)."
  [ctx e]
  (try (cx ctx e)
       (catch clojure.lang.ExceptionInfo ex
         (if-let [m (:sql-error (ex-data ex))]
           (do (when (:pending ctx) (reset! (:pending ctx) m)) (const-c nil))
           (throw ex)))))

(defn- simplified-and-or
  "sqlite3ExprSimplifiedAndOr: with an always-true/false integer literal operand, AND/OR reduce to one
  operand, whose truth value is the result. Returns the compiled replacement or nil."
  [op l r]
  (let [t? (fn [c] (and (:intlit c) (not= 0 (long (:intlit c)))))
        f? (fn [c] (and (:intlit c) (= 0 (long (:intlit c)))))
        ch (cond (or (t? l) (f? r)) (if (= op :and) r l)
                 (or (t? r) (f? l)) (if (= op :and) l r)
                 :else nil)]
    (when ch
      (let [tv (fn [g] (fn [env] (tvl (g env))))]
        (cond-> (mk (tv (:f ch)) :cft (tv (condf ch)) :cff (tv (condff ch)))
          (:intlit ch) (assoc :intlit (if (zero? (long (:intlit ch))) 0 1) :const true))))))

;; ---------------------------------------------------------------------- row values

(defn- multi-col-ast? [sel]
  (let [b (loop [b (:body sel)] (if (= :compound (:type b)) (recur (:left b)) b))]
    (or (not= :core (:type b)) (> (count (:cols b)) 1) (some :star (:cols b)))))

(defn- row-like? [e]
  (or (= :vector (:op e)) (and (= :subq (:op e)) (multi-col-ast? (:select e)))))

(defn- row-parts
  "A row value: {:n :row (fn [env] -> values) :parts [{:aff :coll ...}]}, or nil for a scalar."
  [ctx e]
  (case (:op e)
    :vector (let [cs (mapv (fn [x] (when (row-like? x) (err "row value misused")) (cx ctx x)) (:items e))
                  fs (mapv :f cs)]
              {:n (count cs) :parts cs :row (fn [env] (mapv #(% env) fs))})
    :subq (when (multi-col-ast? (:select e))
            (let [sub (compile-select ctx (:select e))
                  n (count (:names sub))]
              (when (> n 1)
                (let [cache (when-not (:correlated sub) (volatile! nil))
                      run (fn [env] (or (first (run-select-rows sub env 1)) (vec (repeat n nil))))]
                  {:n n
                   :parts (vec (for [i (range n)] {:aff (nth (:affs sub) i) :coll (nth (:colls sub) i)
                                                   :ecoll (nth (:ecolls sub) i)}))
                   :row (if cache (fn [env] (or @cache (vreset! cache (run env)))) run)}))))
    nil))

(defn- scalar-part [ctx e]
  (let [c (cx ctx e)] {:n 1 :parts [c] :row (let [f (:f c)] (fn [env] [(f env)]))}))

(defn- pair-specs [lp rp]
  (when (not= (:n lp) (:n rp)) (err "row value misused"))
  (mapv (fn [l r] [(v/compare-affinity (:aff l) (:aff r)) (or (binary-coll l r) "BINARY")])
        (:parts lp) (:parts rp)))

(defn- row-eq
  "Three-valued equality of two value vectors."
  [specs ls rs]
  (loop [i 0 null false]
    (if (= i (count specs))
      (if null nil 1)
      (let [a (nth ls i) b (nth rs i)]
        (if (or (nil? a) (nil? b))
          (recur (inc i) true)
          (let [[aff coll] (nth specs i)]
            (if (zero? (long (compare-values a b aff coll))) (recur (inc i) null) 0)))))))

(defn- compile-row-compare [op lp rp]
  (let [specs (pair-specs lp rp)
        n (count specs)
        lf (:row lp) rf (:row rp)]
    (mk (fn [env]
          (let [ls (lf env) rs (rf env)]
            (case op
              :eq (row-eq specs ls rs)
              :ne (let [r (row-eq specs ls rs)] (when r (- 1 (long r))))
              (:lt :le :gt :ge)
              (loop [i 0]
                (let [a (nth ls i) b (nth rs i)]
                  (if (or (nil? a) (nil? b))
                    nil
                    (let [[aff coll] (nth specs i)
                          c (long (compare-values a b aff coll))]
                      (if (= i (dec n))
                        (bool->int (case op :lt (< c 0) :le (<= c 0) :gt (> c 0) :ge (>= c 0)))
                        (if (zero? c) (recur (inc i))
                            (bool->int (if (#{:lt :le} op) (< c 0) (> c 0)))))))))))))))

(defn- compile-row-is [isnot lp rp]
  (let [specs (pair-specs lp rp)
        lf (:row lp) rf (:row rp)]
    (mk (fn [env]
          (let [ls (lf env) rs (rf env)
                eq (every? (fn [i] (let [a (nth ls i) b (nth rs i)]
                                     (cond (and (nil? a) (nil? b)) true
                                           (or (nil? a) (nil? b)) false
                                           :else (let [[aff coll] (nth specs i)]
                                                   (zero? (long (compare-values a b aff coll)))))))
                           (range (count specs)))]
            (bool->int (if isnot (not eq) eq)))))))

(defn- compile-row-in [ctx e lp]
  (let [n (:n lp) lf (:row lp) negate (:not e)
        finish (fn [r] (if negate (when-not (nil? r) (- 1 (long r))) r))]
    (cond
      (:list e)
      (let [items (mapv (fn [it]
                          (let [rp (or (row-parts ctx it) (scalar-part ctx it))]
                            (when (not= n (:n rp))
                              (err (str "IN(...) element has " (:n rp) " term" (when (not= 1 (:n rp)) "s") " - expected " n)))
                            rp))
                        (:list e))
            specs (mapv #(pair-specs lp %) items)]
        (mk (fn [env]
              (let [ls (lf env)]
                (finish (loop [k 0 null false]
                          (if (= k (count items))
                            (if null nil 0)
                            (let [r (row-eq (nth specs k) ls ((:row (nth items k)) env))]
                              (cond (= r 1) 1 (nil? r) (recur (inc k) true) :else (recur (inc k) null))))))))))
      (:select e)
      (let [sub (compile-select ctx (:select e))
            m (count (:names sub))
            _ (when (not= m n) (err (str "sub-select returns " m " columns - expected " n)))
            specs (pair-specs lp {:n m :parts (vec (for [i (range m)] {:aff (nth (:affs sub) i) :coll (nth (:colls sub) i)
                                                                       :ecoll (nth (:ecolls sub) i)}))})
            cache (when-not (:correlated sub) (volatile! nil))
            rows-of (fn [env] (if cache (or @cache (vreset! cache (vec (run-select-rows sub env)))) (run-select-rows sub env)))]
        (mk (fn [env]
              (let [ls (lf env) rows (rows-of env)]
                (finish (loop [rows (seq rows) null false]
                          (if (empty? rows)
                            (if null nil 0)
                            (let [r (row-eq specs ls (first rows))]
                              (cond (= r 1) 1 (nil? r) (recur (rest rows) true) :else (recur (rest rows) null))))))))))
      :else (err "row value misused"))))

(defn cx*
  [ctx e]
  ;; once an error has been recorded, SQLite's resolver aborts at the next non-column, non-function node
  (when-let [pend (:pending ctx)]
    (when (and @pend (not (#{:col :fn} (:op e))) (not (null-test? e)) (not (is-rfirst e)))
      (err @pend)))
  (case (:op e)
    :lit (let [v (lit-value e)
               c (const-c v)]
           ;; integer literals stored as 32-bit values count as always true/false for AND/OR simplification
           (if (and (#{:int :hex} (:kind e)) (instance? Long v) (<= 0 (long v) 2147483647))
             (assoc c :intlit v)
             c))
    :col (let [r (resolve-column ctx (:table e) (:name e) (:dq e))]
           (if (contains? r :const)
             (assoc (const-c (:const r)) :intlit (:const r))
             (let [slot (long (:slot r)) idx (long (:idx r))
                   [aslot aidx] (:alt r)]
               (when (and (:outer-refs ctx) (pos? (long (:depth r))))
                 (reset! (:outer-refs ctx) true))
               (mk (if aslot
                     (let [aslot (long aslot) aidx (long aidx)]
                       (fn [^objects env] (let [x (nth (aget env slot) idx)] (if (nil? x) (nth (aget env aslot) aidx) x))))
                     (fn [^objects env] (nth (aget env slot) idx)))
                   :aff (:aff r) :coll (if (:nocoll r) nil (or (:coll r) "BINARY")) :col r))))
    :unary
    (let [o (:o e) inner (:e e)]
      (case o
        :neg (let [nl (neg-literal inner)]
               (if (not= nl ::none)
                 (const-c nl)
                 (let [c (cx ctx inner) f (:f c)]
                   (mk (fn [env] (v/arith :sub 0 (f env))) :ecoll (:ecoll c) :coll (when (:ecoll c) (:coll c))))))
        :pos (let [c (cx ctx inner)] (assoc c :aff nil :const (:const c) :col nil :intlit nil))
        :bitnot (let [c (cx ctx inner) f (:f c)]
                  (mk (fn [env] (let [x (f env)] (when-not (nil? x) (bit-not (v/int-value x)))))
                      :ecoll (:ecoll c) :coll (when (:ecoll c) (:coll c))))))
    :not (let [c (cx ctx (:e e)) f (:f c) ct (condf c) cfl (condff c)
               neg (fn [g] (fn [env] (let [x (g env)] (when-not (nil? x) (if (v/bool-value x) 0 1)))))]
           (mk (neg f) :cft (neg cfl) :cff (neg ct)
               :ecoll (:ecoll c) :coll (when (:ecoll c) (:coll c))))
    :collate (let [c (cx ctx (:e e))
                   name (check-coll (:coll e))]
               (assoc c :coll name :ecoll true :col nil))
    :cast (let [c (cx ctx (:e e)) f (:f c)
                aff (v/type-affinity (:type e))]
            (mk (fn [env] (v/cast-value (f env) aff)) :aff aff :coll (:coll c) :ecoll (:ecoll c)))
    :bin
    (if-let [rc (when (and (#{:eq :ne :lt :le :gt :ge} (:o e)) (or (row-like? (:l e)) (row-like? (:r e))))
                  (let [lp (row-parts ctx (:l e)) rp (row-parts ctx (:r e))]
                    (when (or lp rp)
                      (compile-row-compare (:o e) (or lp (scalar-part ctx (:l e))) (or rp (scalar-part ctx (:r e)))))))]
      rc
    (let [o (:o e)
          l (cx ctx (:l e)) r (cx ctx (:r e))
          fl (:f l) fr (:f r)
          extra {:ecoll (or (has-collate? l) (has-collate? r)) :coll (coll-walk [l r])}]
      (case o
        :and (if-let [ch (simplified-and-or :and l r)]
               ch
             (merge (mk (fn [env]
                          (let [a (fl env) b (fr env)]
                            (cond (falsy? a) 0 (falsy? b) 0 (or (nil? a) (nil? b)) nil :else 1)))
                        :cft (and-cond (condf l) (condf r) :t)
                        :cff (and-cond (condff l) (condff r) :f))
                    extra))
        :or (if-let [ch (simplified-and-or :or l r)]
              ch
            (merge (mk (fn [env]
                         (let [a (fl env) b (fr env)]
                           (cond (truthy? a) 1 (truthy? b) 1 (or (nil? a) (nil? b)) nil :else 0)))
                       :cft (or-cond (condf l) (condf r) :t)
                       :cff (or-cond (condff l) (condff r) :f))
                   extra))
        (:eq :ne :lt :le :gt :ge) (make-compare o l r)
        (:add :sub :mul :div :rem) (merge (mk (fn [env] (v/arith o (fl env) (fr env)))) extra)
        (:bitand :bitor :shl :shr) (merge (mk (fn [env] (v/bitop o (fl env) (fr env)))) extra)
        :concat (merge (mk (fn [env] (v/concat-values (fl env) (fr env)))) extra)
        (:ptr :ptr2) (err "no such function: ->"))))
    :is (if (or (row-like? (:l e)) (row-like? (:r e)))
          (compile-row-is (:not e) (or (row-parts ctx (:l e)) (scalar-part ctx (:l e))) (or (row-parts ctx (:r e)) (scalar-part ctx (:r e))))
        (let [rcol (is-rfirst e)
              rfirst (boolean rcol)
              _ (when rcol
                  (cx ctx rcol)
                  (when (and (:pending ctx) @(:pending ctx)) (err @(:pending ctx))))
              r0 (when rfirst (cx ctx (:r e)))
              l (if (null-test? e) (contained ctx (:l e)) (cx ctx (:l e)))
              _ (when (and rfirst (:pending ctx) @(:pending ctx)) (err @(:pending ctx)))
              r (or r0 (if (null-lit? (:r e)) (const-c nil) (cx ctx (:r e))))
              fl (:f l) fr (:f r)
              aff (v/compare-affinity (:aff l) (:aff r))
              coll (or (binary-coll l r) "BINARY")
              isnot (:not e)]
          (mk (fn [env]
                (let [a (fl env) b (fr env)
                      eq (cond (and (nil? a) (nil? b)) true
                               (or (nil? a) (nil? b)) false
                               :else (zero? (long (compare-values a b aff coll))))]
                  (bool->int (if isnot (not eq) eq))))
              :ecoll (or (has-collate? l) (has-collate? r)) :coll (coll-walk [l r]))))
    :isnull (let [c (if (null-test? e) (contained ctx (:e e)) (cx ctx (:e e))) f (:f c)] (mk (fn [env] (bool->int (nil? (f env))))))
    :notnull (let [c (if (null-test? e) (contained ctx (:e e)) (cx ctx (:e e))) f (:f c)] (mk (fn [env] (bool->int (not (nil? (f env)))))))
    :between
    (if (row-like? (:e e))
      (let [xp (or (row-parts ctx (:e e)) (scalar-part ctx (:e e)))
            lp (or (row-parts ctx (:lo e)) (scalar-part ctx (:lo e)))
            hp (or (row-parts ctx (:hi e)) (scalar-part ctx (:hi e)))
            f1 (:f (compile-row-compare :ge xp lp)) f2 (:f (compile-row-compare :le xp hp))
            negate (:not e)]
        (mk (fn [env] (let [a (f1 env) b (f2 env)
                            r (cond (or (= a 0) (= b 0)) 0 (or (nil? a) (nil? b)) nil :else 1)]
                        (if negate (when-not (nil? r) (- 1 (long r))) r)))))
    (let [x (cx ctx (:e e)) lo (cx ctx (:lo e)) hi (cx ctx (:hi e))
          fx (:f x) flo (:f lo) fhi (:f hi)
          aff1 (v/compare-affinity (:aff x) (:aff lo)) coll1 (or (binary-coll x lo) "BINARY")
          aff2 (v/compare-affinity (:aff x) (:aff hi)) coll2 (or (binary-coll x hi) "BINARY")
          negate (:not e)]
      (mk (fn [env]
            (let [a (fx env)
                  b (flo env)
                  c (fhi env)
                  c1 (when-not (or (nil? a) (nil? b)) (bool->int (>= (long (compare-values a b aff1 coll1)) 0)))
                  c2 (when-not (or (nil? a) (nil? c)) (bool->int (<= (long (compare-values a c aff2 coll2)) 0)))
                  r (cond (or (= c1 0) (= c2 0)) 0
                          (or (nil? c1) (nil? c2)) nil
                          :else 1)]
              (if negate (when-not (nil? r) (- 1 (long r))) r)))
          :cft (let [bt (fn [mode]
                          ;; x BETWEEN lo AND hi is coded as (x>=lo AND x<=hi) with x evaluated once
                          (fn [env]
                            (let [a (fx env)
                                  b (flo env)
                                  c1 (when-not (or (nil? a) (nil? b)) (bool->int (>= (long (compare-values a b aff1 coll1)) 0)))]
                              (if (if (= mode :t) (or (nil? c1) (= c1 0)) (= c1 0))
                                0
                                (let [c (fhi env)
                                      c2 (when-not (or (nil? a) (nil? c)) (bool->int (<= (long (compare-values a c aff2 coll2)) 0)))]
                                  (cond (= c2 0) 0 (or (nil? c1) (nil? c2)) nil :else 1))))))
                     neg (fn [g] (fn [env] (let [r (g env)] (when-not (nil? r) (- 1 (long r))))))]
                 (if negate (neg (bt :f)) (bt :t)))
          :cff (let [bt (fn [mode]
                          (fn [env]
                            (let [a (fx env)
                                  b (flo env)
                                  c1 (when-not (or (nil? a) (nil? b)) (bool->int (>= (long (compare-values a b aff1 coll1)) 0)))]
                              (if (if (= mode :t) (or (nil? c1) (= c1 0)) (= c1 0))
                                0
                                (let [c (fhi env)
                                      c2 (when-not (or (nil? a) (nil? c)) (bool->int (<= (long (compare-values a c aff2 coll2)) 0)))]
                                  (cond (= c2 0) 0 (or (nil? c1) (nil? c2)) nil :else 1))))))
                     neg (fn [g] (fn [env] (let [r (g env)] (when-not (nil? r) (- 1 (long r))))))]
                 (if negate (neg (bt :t)) (bt :f)))
          :ecoll (some has-collate? [x lo hi]) :coll (coll-walk [x lo hi]))))
    :like
    (let [x (cx ctx (:e e)) pt (cx ctx (:pat e)) es (when (:esc e) (cx ctx (:esc e)))
          fx (:f x) fp (:f pt) fe (:f es)
          glob? (= :glob (:o e))
          negate (:not e)]
      (when (#{:match :regexp} (:o e)) (err (str "no such function: " (name (:o e)))))
      (mk (fn [env]
            (let [s (fx env) pat (fp env) esc (when fe (fe env))]
              (if (or (nil? s) (nil? pat) (and fe (nil? esc)))
                nil
                (let [esc-cp (when fe
                               (let [es (v/text-of esc)]
                                 (if (not= 1 (.codePointCount ^String es 0 (count es)))
                                   (err "ESCAPE expression must be a single character")
                                   (.codePointAt ^String es 0))))
                      m (if glob?
                          (v/glob-match (v/text-of pat) (v/text-of s))
                          (v/like-match (v/text-of pat) (v/text-of s) esc-cp))]
                  (bool->int (if negate (not m) m))))))
          :ecoll (some has-collate? [x pt]) :coll (coll-walk [x pt])))
    :case
    (let [base (when (:base e) (cx ctx (:base e)))
          whens (mapv (fn [[w t]] [(cx ctx w) (cx ctx t)]) (:whens e))
          els (when (:else e) (cx ctx (:else e)))
          fe (:f els)]
      (if base
        (let [fb (:f base)
              ws (mapv (fn [[w t]] [(:f w) (:f t) (v/compare-affinity (:aff base) (:aff w))
                                    (or (binary-coll base w) "BINARY")]) whens)]
          (mk (fn [env]
                (let [b (fb env)]
                  (loop [ws ws]
                    (if (empty? ws)
                      (when fe (fe env))
                      (let [[fw ft aff coll] (first ws)
                            w (fw env)]
                        (if (and (not (nil? b)) (not (nil? w)) (zero? (long (compare-values b w aff coll))))
                          (ft env)
                          (recur (rest ws))))))))))
        (let [ws (mapv (fn [[w t]] [(condf w) (:f t)]) whens)]
          (mk (fn [env]
                (loop [ws ws]
                  (if (empty? ws)
                    (when fe (fe env))
                    (let [[fw ft] (first ws) w (fw env)]
                      (if (and (not (nil? w)) (v/bool-value w))
                        (ft env)
                        (recur (rest ws)))))))))))
    :in
    (if (row-like? (:e e))
      (compile-row-in ctx e (or (row-parts ctx (:e e)) (scalar-part ctx (:e e))))
    (let [x (cx ctx (:e e)) fx (:f x) negate (:not e)]
      (cond
        (:list e)
        (let [items (mapv #(cx ctx %) (:list e))
              aff (:aff x)
              coll (or (:coll x) "BINARY")
              fs0 (mapv :f items)
              ;; a constant list of 3+ items becomes an ephemeral table: every item is evaluated (once)
              fs (if (and (>= (count fs0) 3) (every? const-term? (:list e)))
                   (let [cache (volatile! nil)
                         all (fn [env] (or @cache (vreset! cache (mapv #(% env) fs0))))]
                     (vec (map-indexed (fn [i _] (fn [env] (nth (all env) i))) fs0)))
                   fs0)]
          (when (= :vector (:op (:e e))) (err "row value misused"))
          (mk (fn [env]
                (if (empty? fs)
                  (bool->int negate)
                  (let [a (fx env)]
                    (if (nil? a)
                      (do (doseq [f fs] (f env)) nil)
                      (let [r (loop [fs fs saw-null false]
                                (if (empty? fs)
                                  (if saw-null nil 0)
                                  (let [b ((first fs) env)]
                                    (cond (nil? b) (recur (rest fs) true)
                                          (zero? (long (compare-values a b aff coll))) 1
                                          :else (recur (rest fs) saw-null)))))]
                        (if negate (when-not (nil? r) (- 1 (long r))) r))))))
              :ecoll (has-collate? x) :coll (when (has-collate? x) (:coll x))))
        (:select e)
        (let [sub (compile-select ctx (:select e))
              _ (when (not= 1 (count (:names sub)))
                  (err (str "sub-select returns " (count (:names sub)) " columns - expected 1")))
              saff (first (:affs sub))
              aff (v/compare-affinity saff (:aff x))
              scoll {:coll (first (:colls sub)) :ecoll (first (:ecolls sub))}
              coll (or (binary-coll x scoll) "BINARY")
              cache (when-not (:correlated sub) (volatile! nil))
              rows-of (fn [env]
                        (if cache
                          (or @cache (vreset! cache (mapv first (run-select-rows sub env))))
                          (mapv first (run-select-rows sub env))))]
          (mk (fn [env]
                (let [a (fx env)
                      vals (rows-of env)]
                  (if (empty? vals)
                    (bool->int negate)
                    (if (nil? a)
                      nil
                      (let [r (loop [vs vals saw-null false]
                                (if (empty? vs)
                                  (if saw-null nil 0)
                                  (let [b (first vs)]
                                    (cond (nil? b) (recur (rest vs) true)
                                          (zero? (long (compare-values a b aff coll))) 1
                                          :else (recur (rest vs) saw-null)))))]
                        (if negate (when-not (nil? r) (- 1 (long r))) r))))))))
        (:table e)
        (cx ctx (-> e (dissoc :table) (assoc :select {:body {:type :core :cols [{:star true}] :from [{:table (:table e)}]}})))
        :else (err (str "no such table: " (:table e))))))
    :subq
    (let [sub (compile-select ctx (:select e))
          _ (when (not= 1 (count (:names sub)))
              (err (str "sub-select returns " (count (:names sub)) " columns - expected 1")))
          cache (when-not (:correlated sub) (volatile! ::none))]
      (mk (fn [env]
            (if cache
              (let [c @cache]
                (if (= c ::none)
                  (vreset! cache (first (first (run-select-rows sub env 1))))
                  c))
              (first (first (run-select-rows sub env 1)))))
          :aff (first (:affs sub)) :coll (first (:colls sub)) :ecoll (first (:ecolls sub))))
    :exists
    (let [sub (compile-select ctx (:select e))
          cache (when-not (:correlated sub) (volatile! nil))]
      (mk (fn [env]
            (if cache
              (or @cache (vreset! cache (bool->int (seq (run-select-rows sub env 1)))))
              (bool->int (seq (run-select-rows sub env 1)))))))
    :vector (err "row value misused")
    :fn (compile-function ctx e)
    (err (str "unsupported expression " (:op e)))))

;; ====================================================================== scalar functions

(defn- arity-err [name] (err (str "wrong number of arguments to function " name "()")))

(defn- substr-impl [x start len has-len]
  (if (or (nil? x) (nil? start) (and has-len (nil? len)))
    nil
    (let [blob? (v/blob? x)
          p1 (v/int-value start)
          p2 (if has-len (v/int-value len) nil)]
      (if blob?
        (if (zero? (alength (v/blob-bytes x)))
          nil
        (let [^bytes b (v/blob-bytes x)
              n (alength b)
              [p1 p2] (let [p1 (long p1)
                            p2 (if has-len (long p2) (long 1000000000))
                            [p1 p2] (if (< p2 0) [(- p1 (- p2)) (- p2)] [p1 p2])
                            neg2 (and has-len (< (long (v/int-value len)) 0))]
                        [p1 p2])
              p1 (long p1) p2 (long p2)
              ;; follow SQLite substrFunc exactly
              [p1 p2] (let [orig-p2 (if has-len (long (v/int-value len)) 1000000000)
                            p1o (long (v/int-value start))
                            negp2 (< orig-p2 0)
                            p2a (if negp2 (- orig-p2) orig-p2)
                            [p1 p2] (cond
                                      (< p1o 0) (let [p1 (+ p1o n)] (if (< p1 0) [0 (max 0 (+ p2a p1))] [p1 p2a]))
                                      (> p1o 0) [(dec p1o) p2a]
                                      :else [0 (if (> p2a 0) (dec p2a) p2a)])
                            [p1 p2] (if negp2
                                      (let [p1b (- p1 p2)] (if (< p1b 0) [0 (+ p2 p1b)] [p1b p2]))
                                      [p1 p2])]
                        [p1 p2])
              p1 (long p1) p2 (long p2)
              p2 (if (> (+ p1 p2) n) (max 0 (- n p1)) p2)]
          (if (or (>= p1 n) (<= p2 0)) (Blob. (byte-array 0))
              (Blob. (java.util.Arrays/copyOfRange b (int p1) (int (+ p1 p2)))))))
        (let [^bytes tb0 (v/str->utf8 (v/text-of x))
              ;; text is scanned up to its first NUL
              ^bytes tb (let [k (loop [i 0] (if (and (< i (alength tb0)) (not (zero? (aget tb0 i)))) (recur (inc i)) i))]
                          (java.util.Arrays/copyOf tb0 (int k)))
              nb (alength tb)
              ;; character start offsets as SQLITE_SKIP_UTF8 sees them
              starts (loop [i 0 acc (transient [])]
                       (if (>= i nb)
                         (persistent! (conj! acc nb))
                         (let [c (bit-and (aget tb i) 0xff)
                               j (if (>= c 0xc0)
                                   (loop [j (inc i)] (if (and (< j nb) (= 0x80 (bit-and (aget tb j) 0xc0))) (recur (inc j)) j))
                                   (inc i))]
                           (recur j (conj! acc i)))))
              n (dec (count starts))
              orig-p2 (if has-len (long p2) 1000000000)
              p1o (long p1)
              negp2 (< orig-p2 0)
              p2a (if negp2 (- orig-p2) orig-p2)
              [p1 p2] (cond
                        (< p1o 0) (let [p1 (+ p1o n)] (if (< p1 0) [0 (max 0 (+ p2a p1))] [p1 p2a]))
                        (> p1o 0) [(dec p1o) p2a]
                        :else [0 (if (> p2a 0) (dec p2a) p2a)])
              [p1 p2] (if negp2
                        (let [p1b (- (long p1) (long p2))] (if (< p1b 0) [0 (+ (long p2) p1b)] [p1b p2]))
                        [p1 p2])
              p1 (long p1) p2 (long p2)]
          (if (or (>= p1 n) (<= p2 0))
            ""
            (let [end (min n (+ p1 p2))]
              (v/utf8->str (java.util.Arrays/copyOfRange tb (int (nth starts p1)) (int (nth starts end)))))))))))

(defn- sqlite-length [x]
  (cond (nil? x) nil
        (v/blob? x) (alength (v/blob-bytes x))
        :else (let [^bytes b (v/str->utf8 (v/text-of x))
                    n (alength b)]
                (loop [i 0 cnt 0]
                  (if (or (>= i n) (zero? (aget b i)))
                    cnt
                    (let [c (bit-and (aget b i) 0xff)
                          i (inc i)
                          i (if (>= c 0xc0)
                              (loop [i i] (if (and (< i n) (= 0x80 (bit-and (aget b i) 0xc0))) (recur (inc i)) i))
                              i)]
                      (recur i (inc cnt))))))))

(defn- ascii-upper ^String [^String s]
  (let [sb (StringBuilder. s)]
    (dotimes [i (.length sb)]
      (let [c (.charAt sb i)] (when (and (>= (int c) 97) (<= (int c) 122)) (.setCharAt sb i (char (- (int c) 32))))))
    (.toString sb)))

(defn- ascii-lower ^String [^String s]
  (let [sb (StringBuilder. s)]
    (dotimes [i (.length sb)]
      (let [c (.charAt sb i)] (when (and (>= (int c) 65) (<= (int c) 90)) (.setCharAt sb i (char (+ (int c) 32))))))
    (.toString sb)))

(defn- utf8-char-seqs
  "The characters of b (up to the first NUL when nul-stop) as byte vectors, segmented like SQLITE_SKIP_UTF8."
  [^bytes b nul-stop]
  (let [n (alength b)]
    (loop [i 0 acc []]
      (if (or (>= i n) (and nul-stop (zero? (aget b i))))
        acc
        (let [c (bit-and (aget b i) 0xff)
              j (if (>= c 0xc0)
                  (loop [j (inc i)] (if (and (< j n) (= 0x80 (bit-and (aget b j) 0xc0))) (recur (inc j)) j))
                  (inc i))]
          (recur j (conj acc (java.util.Arrays/copyOfRange b i j))))))))

(defn- trim-impl
  "Port of trimFunc: byte-level, with the character set read up to its first NUL."
  [x chars mode]
  (if (or (nil? x) (and chars (nil? (first chars))))
    nil
    (let [^bytes b (v/str->utf8 (v/text-of x))
          set-chars (if chars (utf8-char-seqs (v/str->utf8 (v/text-of (first chars))) true) [(byte-array [32])])
          starts-with (fn [^bytes c ^long from ^long to]
                        (let [l (alength c)]
                          (and (<= l (- to from))
                               (java.util.Arrays/equals c (java.util.Arrays/copyOfRange b (int from) (int (+ from l)))))))
          ends-with (fn [^bytes c ^long from ^long to]
                      (let [l (alength c)]
                        (and (<= l (- to from))
                             (java.util.Arrays/equals c (java.util.Arrays/copyOfRange b (int (- to l)) (int to))))))
          n (alength b)
          st (if (and (seq set-chars) (#{:both :left} mode))
               (loop [i 0]
                 (if-let [c (when (< i n) (first (filter #(starts-with % i n) set-chars)))]
                   (recur (+ i (alength ^bytes c)))
                   i))
               0)
          en (if (and (seq set-chars) (#{:both :right} mode))
               (loop [j n]
                 (if-let [c (when (> j st) (first (filter #(ends-with % st j) set-chars)))]
                   (recur (- j (alength ^bytes c)))
                   j))
               n)]
      (v/utf8->str (java.util.Arrays/copyOfRange b (int st) (int en))))))

(defn- quote-impl [x]
  (cond (nil? x) "NULL"
        (instance? Long x) (str x)
        (instance? Double x) (num/real->text (double x))
        (string? x) (let [k (.indexOf ^String x (int 0)) x (if (neg? k) x (subs x 0 k))]
                      (str "'" (str/replace x "'" "''") "'"))
        :else (str "X'" (v/hex-upper (v/blob-bytes x)) "'")))

(defn- replace-impl [x from to]
  (if (or (nil? x) (nil? from) (nil? to))
    nil
    (let [^String s (v/text-of x) ^String f (v/text-of from) ^String t (v/text-of to)]
      (if (or (= "" f) (= \u0000 (.charAt f 0)))
        s
        (let [^bytes sb (v/str->utf8 s) ^bytes fb (v/str->utf8 f) ^bytes tb (v/str->utf8 t)
              n (alength sb) m (alength fb)
              out (java.io.ByteArrayOutputStream. n)]
          (loop [i 0]
            (if (> (+ i m) n)
              (.write out sb i (- n i))
              (if (java.util.Arrays/equals (java.util.Arrays/copyOfRange sb i (+ i m)) fb)
                (do (.write out tb 0 (alength tb)) (recur (+ i m)))
                (do (.write out (int (aget sb i))) (recur (inc i))))))
          (v/utf8->str (.toByteArray out)))))))

(defn- instr-impl [h n]
  (if (or (nil? h) (nil? n))
    nil
    (if (and (v/blob? h) (v/blob? n))
      (let [^bytes hb (v/blob-bytes h) ^bytes nb (v/blob-bytes n)
            hn (alength hb) nn (alength nb)]
        (loop [i 0]
          (cond (> (+ i nn) hn) 0
                (java.util.Arrays/equals (java.util.Arrays/copyOfRange hb i (+ i nn)) nb) (inc i)
                :else (recur (inc i)))))
      (let [^bytes hb (v/str->utf8 (v/text-of h)) ^bytes nb (v/str->utf8 (v/text-of n))
            hn (alength hb) nn (alength nb)]
        ;; byte search; the position counts characters by skipping UTF-8 continuation bytes
        (loop [i 0 cnt 1]
          (cond (> (+ i nn) hn) 0
                (java.util.Arrays/equals (java.util.Arrays/copyOfRange hb i (+ i nn)) nb) cnt
                :else (let [j (loop [j (inc i)] (if (and (< j hn) (= 0x80 (bit-and (aget hb j) 0xc0))) (recur (inc j)) j))]
                        (recur j (inc cnt)))))))))

(defn- char-impl [args]
  (let [sb (StringBuilder.)]
    (doseq [a args]
      (let [x (v/int-value a)
            x (if (or (< x 0) (> x 0x10ffff)) 0xfffd x)]
        (.appendCodePoint sb (int x))))
    (.toString sb)))

(defn- round-impl [x d]
  (if (or (nil? x) (and d (nil? (first d))))
    nil
    (let [n (if d (v/int-value (first d)) 0)
          n (cond (> n 30) 30 (< n 0) 0 :else n)]
      (if (and (instance? Long x) (or true))
        (if (instance? Long x) (double (long x))
            nil)
        nil)
      (let [r (v/real-value x)]
        (cond
          (or (< r -4503599627370496.0) (> r 4503599627370496.0)) r
          (= n 0) (let [r2 (double (long (+ (Math/abs r) 0.5)))] (if (neg? r) (- r2) r2))
          :else (let [s (format (str "%." n "f") r)]
                  (first (num/atof (num/real->text (Double/parseDouble s))))))))))

(defn- hex-impl [x]
  (if (nil? x) "" (v/hex-upper (v/bytes-of x))))

(defn- unicode-impl
  "sqlite3Utf8Read on the first character of the UTF-8 bytes."
  [x]
  (when-not (nil? x)
    (let [^bytes b (v/str->utf8 (v/text-of x))
          n (alength b)]
      (when (and (pos? n) (not= 0 (aget b 0)))
        (let [c0 (bit-and (aget b 0) 0xff)]
          (if (< c0 0xc0)
            c0
            (let [t1 (cond (< c0 0xe0) (bit-and c0 0x1f) (< c0 0xf0) (bit-and c0 0x0f) (< c0 0xf8) (bit-and c0 0x07)
                           (< c0 0xfc) (bit-and c0 0x03) (< c0 0xfe) (bit-and c0 0x01) :else 0)
                  c (loop [i 1 c t1]
                      (if (and (< i n) (= 0x80 (bit-and (aget b i) 0xc0)))
                        (recur (inc i) (bit-and 0xffffffff (+ (bit-shift-left c 6) (bit-and (aget b i) 0x3f))))
                        c))]
              (if (or (< c 0x80) (= 0xD800 (bit-and c 0xFFFFF800)) (= 0xFFFE (bit-and c 0xFFFFFFFE)))
                0xFFFD
                c))))))))

(defn- minmax-scalar [args coll max?]
  (if (some nil? args)
    nil
    (reduce (fn [best a]
              (let [c (v/sql-cmp a best coll)]
                (if (if max? (> c 0) (< c 0)) a best)))
            (first args) (rest args))))

(defn- abs-impl [x]
  (cond (nil? x) nil
        (instance? Long x) (let [i (long x)]
                             (cond (= i Long/MIN_VALUE) (err "integer overflow")
                                   (neg? i) (- i)
                                   :else i))
        (instance? Double x) (Math/abs (double x))
        :else (Math/abs (v/real-value x))))

(defn compile-scalar-fn
  "Returns (fn [vals]) for scalar function name with n args, or nil if unknown."
  [name n coll]
  (let [ar (fn [ok] (when-not ok (arity-err name)))]
    (case name
      "abs" (do (ar (= n 1)) (fn [[x]] (abs-impl x)))
      "length" (do (ar (= n 1)) (fn [[x]] (sqlite-length x)))
      "octet_length" (do (ar (= n 1)) (fn [[x]] (when-not (nil? x) (alength (v/bytes-of x)))))
      "upper" (do (ar (= n 1)) (fn [[x]] (when-not (nil? x) (ascii-upper (v/text-of x)))))
      "lower" (do (ar (= n 1)) (fn [[x]] (when-not (nil? x) (ascii-lower (v/text-of x)))))
      ("substr" "substring") (do (ar (or (= n 2) (= n 3))) (fn [[x s l]] (substr-impl x s l (= n 3))))
      "instr" (do (ar (= n 2)) (fn [[h nn]] (instr-impl h nn)))
      "replace" (do (ar (= n 3)) (fn [[x f t]] (replace-impl x f t)))
      "trim" (do (ar (or (= n 1) (= n 2))) (fn [[x & c]] (trim-impl x c :both)))
      "ltrim" (do (ar (or (= n 1) (= n 2))) (fn [[x & c]] (trim-impl x c :left)))
      "rtrim" (do (ar (or (= n 1) (= n 2))) (fn [[x & c]] (trim-impl x c :right)))
      "hex" (do (ar (= n 1)) (fn [[x]] (hex-impl x)))
      "quote" (do (ar (= n 1)) (fn [[x]] (quote-impl x)))
      "unicode" (do (ar (= n 1)) (fn [[x]] (unicode-impl x)))
      "char" (fn [args] (char-impl args))
      "typeof" (do (ar (= n 1)) (fn [[x]] (v/type-name x)))
      "nullif" (do (ar (= n 2)) (fn [[a b]] (if (and (not (nil? a)) (not (nil? b)) (zero? (v/sql-cmp a b coll))) nil a)))
      "max" (do (when (< n 2) (arity-err name)) (fn [args] (minmax-scalar args coll true)))
      "min" (do (when (< n 2) (arity-err name)) (fn [args] (minmax-scalar args coll false)))
      "round" (do (ar (or (= n 1) (= n 2))) (fn [[x & d]] (round-impl x d)))
      "sign" (do (ar (= n 1)) (fn [[x]] (when-not (nil? x)
                                         (let [y (v/numeric-type-value x)]
                                           (if (or (instance? Long y) (instance? Double y))
                                             (let [r (v/real-value y)] (cond (pos? r) 1 (neg? r) -1 :else 0))
                                             nil)))))
      "zeroblob" (do (ar (= n 1)) (fn [[x]] (Blob. (byte-array (max 0 (v/int-value x))))))
      ("likely" "unlikely") (do (ar (= n 1)) (fn [[x]] x))
      "likelihood" (do (ar (= n 2)) (fn [[x _]] x))
      "concat" (do (when (< n 1) (arity-err name)) (fn [args] (apply str (map #(or (v/text-of %) "") args))))
      "concat_ws" (do (when (< n 2) (arity-err name))
                      (fn [[sep & args]] (when-not (nil? sep)
                                           (str/join (v/text-of sep) (keep v/text-of args)))))
      "like" (do (ar (or (= n 2) (= n 3)))
                 (fn [[pat s esc]]
                   (if (or (nil? pat) (nil? s)) nil
                       (bool->int (v/like-match (v/text-of pat) (v/text-of s)
                                                (when esc (.codePointAt ^String (v/text-of esc) 0)))))))
      "glob" (do (ar (= n 2)) (fn [[pat s]] (if (or (nil? pat) (nil? s)) nil (bool->int (v/glob-match (v/text-of pat) (v/text-of s))))))
      "unhex" (do (ar (or (= n 1) (= n 2)))
                  (fn [[x]] (when-not (nil? x)
                              (let [s (v/text-of x)]
                                (when (and (even? (count s)) (re-matches #"[0-9a-fA-F]*" s))
                                  (let [b (byte-array (quot (count s) 2))]
                                    (dotimes [i (alength b)] (aset b i (unchecked-byte (Integer/parseInt (subs s (* 2 i) (+ 2 (* 2 i))) 16))))
                                    (Blob. b)))))))
      "sqlite_version" (do (ar (= n 0)) (fn [_] "3.53.4"))
      (ext/compile-fn name n coll))))

(def ^:private known-unsupported
  #{"random" "randomblob" "changes" "total_changes" "last_insert_rowid"
    "date" "time" "datetime" "julianday" "strftime" "unixepoch" "json"})

(defn- function-level-error
  "Errors SQLite reports for the function node itself (before resolving its arguments)."
  [ctx e]
  (let [name (:name e) n (count (:args e))]
    (cond
      (agg-call? e)
      (cond (nil? (:aggs ctx)) (str "misuse of aggregate: " name "()")
            (:in-agg ctx) (str "misuse of aggregate function " name "()")
            (and (#{"sum" "total" "avg"} name) (not= n 1)) (str "wrong number of arguments to function " name "()")
            (and (= name "group_concat") (not (#{1 2} n))) (str "wrong number of arguments to function " name "()")
            (and (= name "string_agg") (not= n 2)) (str "wrong number of arguments to function " name "()")
            (and (:distinct e) (not= 1 n)) "DISTINCT aggregates must have exactly one argument"
            :else nil)
      (#{"min" "max"} name) (when (< n 1) (str "wrong number of arguments to function " name "()"))
      (#{"coalesce"} name) (when (< n 2) (str "wrong number of arguments to function " name "()"))
      (#{"ifnull"} name) (when (not= n 2) (str "wrong number of arguments to function " name "()"))
      (#{"row_number" "rank" "dense_rank" "percent_rank" "cume_dist" "ntile" "lag" "lead"
         "first_value" "last_value" "nth_value"} name) (str "misuse of window function " name "()")
      (#{"iif" "if"} name) (when-not (#{2 3} n) (str "wrong number of arguments to function " name "()"))
      :else
      (try (if (compile-scalar-fn name n "BINARY")
             (when (:distinct e) (str "DISTINCT aggregates must have exactly one argument"))
             (if (known-unsupported name)
               (str "unsupported function " name "()")
               (str "no such function: " name)))
           (catch clojure.lang.ExceptionInfo ex (:sql-error (ex-data ex)))))))

(defn- compile-args
  "Compiles function arguments; a resolution error inside them is recorded (not thrown) and stops the
  remaining arguments, as SQLite's resolver does. Returns the compiled args or nil."
  [ctx args]
  (try (mapv #(cx ctx %) args)
       (catch clojure.lang.ExceptionInfo ex
         (if-let [m (:sql-error (ex-data ex))]
           (do (when (:pending ctx) (reset! (:pending ctx) m)) nil)
           (throw ex)))))

(declare compile-window compile-function*)

(defn compile-function [ctx e]
  (if (:over e)
    (compile-window ctx e)
    (compile-function* ctx e)))

(defn- compile-function* [ctx e]
  (let [name (:name e) n (count (:args e))
        _ (when (and (:filter e) (not (agg-call? e)))
            (err (str "FILTER may not be used with non-aggregate " name "()")))
        ferr (function-level-error ctx e)
        _ (when ferr (reset! (:pending ctx) ferr))
        agg (and (agg-call? e) (:aggs ctx) (not (:in-agg ctx)))
        actx (if agg (assoc ctx :in-agg true :aggs nil :wins nil) ctx)
        args (compile-args actx (:args e))]
    (cond
      (or ferr (nil? args)) (const-c nil)
      agg
      (let [aggs (:aggs ctx)
            idx (count @aggs)
            slot (long (:agg-slot ctx))]
        (swap! aggs conj {:name name :args args :star (:star e) :distinct (:distinct e)
                          :filter (when (:filter e) (condf (cx actx (:filter e))))
                          :coll (or (:coll (first args)) "BINARY")})
        (mk (fn [^objects env] (aget ^objects (aget env slot) idx))
            :coll (when (#{"min" "max"} name) (:coll (first args)))
            :ecoll (when (#{"min" "max"} name) (:ecoll (first args)))))
      :else
      (case name
        ("coalesce" "ifnull")
        (let [fs (mapv :f args)]
          (mk (fn [env] (loop [fs fs] (when (seq fs) (let [x ((first fs) env)] (if (nil? x) (recur (rest fs)) x)))))
              :coll (coll-walk args) :ecoll (some has-collate? args)))
        ("iif" "if")
        (let [[fc ft fe] (cons (condf (first args)) (map :f (rest args)))]
          (mk (fn [env] (let [c (fc env)] (if (and (not (nil? c)) (v/bool-value c)) (ft env) (when fe (fe env)))))
              :coll (coll-walk args) :ecoll (some has-collate? args)))
        (let [coll (or (some #(when (or (:ecoll %) (:col %) (:coll %)) (:coll %)) args) "BINARY")
              coll (or (coll-walk args) coll)
              impl (compile-scalar-fn name n coll)
              fs (mapv :f args)]
          (mk (fn [env] (let [r (impl (mapv #(% env) fs))] (if (instance? Integer r) (long r) r)))
              :coll (coll-walk args) :ecoll (some has-collate? args)))))))

;; ====================================================================== aggregates runtime

(defn- kbn-step [^doubles st ^double r]
  ;; st: [rSum rErr]
  (let [s (aget st 0) t (+ s r)]
    (if (> (Math/abs s) (Math/abs r))
      (aset st 1 (+ (aget st 1) (+ (- s t) r)))
      (aset st 1 (+ (aget st 1) (+ (- r t) s))))
    (aset st 0 t)))

(defn- kbn-step-int [st ^long iv]
  (if (or (<= iv -4503599627370496) (>= iv 4503599627370496))
    (let [ism (rem iv 16384) ibig (- iv ism)]
      (kbn-step st (double ibig)) (kbn-step st (double ism)))
    (kbn-step st (double iv))))

(defn- kbn-init [^doubles st ^long iv]
  (if (or (<= iv -4503599627370496) (>= iv 4503599627370496))
    (let [ism (rem iv 16384)]
      (aset st 0 (double (- iv ism))) (aset st 1 (double ism)))
    (do (aset st 0 (double iv)) (aset st 1 0.0))))

(deftype SumCtx [^doubles rs ^:unsynchronized-mutable ^long isum ^:unsynchronized-mutable ^long cnt
                 ^:unsynchronized-mutable approx ^:unsynchronized-mutable ovrfl]
  clojure.lang.IFn
  (invoke [this x]
    (let [t (v/numeric-type-value x)]
      (when-not (nil? t)
        (set! cnt (inc cnt))
        (if (not approx)
          (if (not (instance? Long t))
            (do (kbn-init rs isum) (set! approx true) (kbn-step rs (v/real-value x)))
            (let [a isum b (long t) r (unchecked-add a b)]
              (if (and (= (neg? a) (neg? b)) (not= (neg? r) (neg? a)))
                (do (set! ovrfl true) (kbn-init rs isum) (set! approx true) (kbn-step-int rs b))
                (set! isum r))))
          (if (instance? Long t)
            (kbn-step-int rs (long t))
            (do (set! ovrfl false) (kbn-step rs (v/real-value x))))))))
  (invoke [this kind _]
    (let [overflow? (fn [^double d] (or (Double/isInfinite d) (Double/isNaN d)))]
      (case kind
        :sum (when (> cnt 0)
               (if approx
                 (cond ovrfl (err "integer overflow")
                       (not (overflow? (aget rs 1))) (+ (aget rs 0) (aget rs 1))
                       :else (aget rs 0))
                 isum))
        :avg (when (> cnt 0)
               (let [r (if approx
                         (let [r (aget rs 0)] (if (overflow? (aget rs 1)) r (+ r (aget rs 1))))
                         (double isum))]
                 (/ r (double cnt))))
        :total (if approx
                 (let [r (aget rs 0)] (if (overflow? (aget rs 1)) r (+ r (aget rs 1))))
                 (double isum))))))

(defn make-acc
  "Returns {:step (fn [vals] -> updated?) :final (fn [])} for one aggregate spec."
  [spec]
  (let [name (:name spec) coll (:coll spec)
        seen (when (:distinct spec)
               (TreeSet. ^Comparator (fn [a b] (v/sql-cmp a b coll))))
        dist-ok (fn [x] (or (nil? seen) (nil? x) (.add ^TreeSet seen x)))]
    (case name
      "count" (let [c (long-array 1)]
                {:step (if (:star spec)
                         (fn [_] (aset c 0 (inc (aget c 0))) false)
                         (fn [[x]] (when (and (not (nil? x)) (dist-ok x)) (aset c 0 (inc (aget c 0)))) false))
                 :final (fn [] (aget c 0))})
      ("sum" "total" "avg")
      (let [s (SumCtx. (double-array 2) 0 0 false false)
            kind (keyword name)]
        {:step (fn [[x]] (when (and (not (nil? x)) (dist-ok x)) (s x)) false)
         :final (fn [] (.invoke ^clojure.lang.IFn s kind nil))})
      ("min" "max")
      (let [best (object-array 1) has (boolean-array 1) mx (= name "max") last-hit (boolean-array 1)]
        {:step (fn [[x]]
                 (let [r
                 (if (nil? x)
                   (not (aget has 0))
                 (if (dist-ok x)
                   (if (not (aget has 0))
                     (do (aset best 0 x) (aset has 0 true) true)
                     (let [c (v/sql-cmp (aget best 0) x coll)]
                       (if (if mx (< c 0) (> c 0))
                         (do (aset best 0 x) true)
                         false)))
                   (aget last-hit 0)))]
                   (aset last-hit 0 (boolean r))
                   r))
         :final (fn [] (aget best 0))
         :minmax true})
      ("group_concat" "string_agg")
      (let [sb (StringBuilder.) has (boolean-array 1)
            two (= 2 (count (:args spec)))]
        {:step (fn [[x sep]]
                 (when (and (not (nil? x)) (dist-ok x))
                   (if (aget has 0)
                     (if two
                       (when-not (nil? sep) (.append sb (v/text-of sep)))
                       (.append sb ","))
                     (aset has 0 true))
                   (.append sb (v/text-of x)))
                 false)
         :final (fn [] (when (aget has 0) (.toString sb)))}))))

;; ====================================================================== window functions

(declare row-comparator compare-keys-fn)

(def window-only #{"row_number" "rank" "dense_rank" "percent_rank" "cume_dist" "ntile" "lag" "lead"
                   "first_value" "last_value" "nth_value"})

(def ^:private win-arity
  {"row_number" #{0} "rank" #{0} "dense_rank" #{0} "percent_rank" #{0} "cume_dist" #{0}
   "ntile" #{1} "lag" #{1 2 3} "lead" #{1 2 3} "first_value" #{1} "last_value" #{1} "nth_value" #{2}})

(defn- strip-pos [e]
  (cond (map? e) (into {} (for [[k x] e :when (not (#{:ps :pe :paren} k))] [k (strip-pos x)]))
        (vector? e) (mapv strip-pos e)
        (seq? e) (mapv strip-pos e)
        :else e))

(defn- resolve-window-def
  "The effective window definition of an OVER clause (a name or a def with an optional base)."
  [ctx over]
  (let [named (:windefs ctx)
        lookup (fn [n] (or (get named (lc n)) (err (str "no such window: " n))))]
    (if (string? over)
      (lookup over)
      (if-let [b (:base over)]
        (let [bw (lookup b)]
          (when (:part over) (err (str "cannot override PARTITION clause of window: " b)))
          (when (and (:order over) (:order bw)) (err (str "cannot override ORDER BY clause of window: " b)))
          (when (:frame bw) (err (str "cannot override frame specification of window: " b)))
          {:part (:part bw) :order (or (:order over) (:order bw)) :frame (:frame over)})
        over))))

(defn compile-window
  "Compiles a window function call; registers it in (:wins ctx) and returns a reader of its value."
  [ctx e]
  (let [name (:name e) n (count (:args e))
        aggf (agg-call? (dissoc e :over))]
    (when (nil? (:wins ctx)) (err (str "misuse of window function " name "()")))
    (cond
      (window-only name)
      (when-not (contains? (win-arity name) n) (err (str "wrong number of arguments to function " name "()")))
      aggf nil
      :else (if (or (agg-names name) (try (compile-scalar-fn name n "BINARY") (catch clojure.lang.ExceptionInfo _ true)))
              (if (agg-names name)
                (err (str "wrong number of arguments to function " name "()"))
                (err (str name "() may not be used as a window function")))
              (err (str "no such function: " name))))
    (when (:distinct e) (err "DISTINCT is not supported for window functions"))
    (when (and (:filter e) (not aggf)) (err "FILTER clause may only be used with aggregate window functions"))
    (let [wd (resolve-window-def ctx (:over e))
          ictx (assoc ctx :wins nil)
          args (mapv #(cx ictx %) (:args e))
          filt (when (:filter e) (condf (cx ictx (:filter e))))
          part (mapv #(cx ictx %) (:part wd))
          order (mapv (fn [t] (let [c (cx ictx (:e t))]
                                {:f (:f c) :coll (or (:coll c) "BINARY") :desc (:desc t) :nulls (:nulls t)}))
                      (:order wd))
          fr (:frame wd)
          _ (when (and fr (= :range (:type fr))
                       (or (#{:preceding :following} (:kind (:start fr))) (#{:preceding :following} (:kind (:end fr))))
                       (not= 1 (count order)))
              (err "RANGE with offset PRECEDING/FOLLOWING requires one ORDER BY expression"))
          bound (fn [b] (cond-> b (:e b) (assoc :f (:f (cx (assoc ictx :aggs nil) (:e b))))))
          frame (when fr (assoc fr :start (bound (:start fr)) :end (bound (:end fr))))
          wins (:wins ctx)
          idx (count @wins)
          slot (long (:win-slot ctx))]
      (swap! wins conj {:name name :agg aggf :args args :filter filt :part part :order order :frame frame
                        :star (:star e) :coll (or (:coll (first args)) "BINARY")
                        :gkey (strip-pos [(:part wd) (mapv (fn [t] [(:e t) (:desc t) (:nulls t)]) (:order wd))])})
      (mk (fn [^objects env] (aget ^objects (aget env slot) idx))
          :coll (when (#{"min" "max" "first_value" "last_value" "nth_value" "lag" "lead"} name) (:coll (first args)))))))

(defn- win-key-cmp
  "Comparator over [pvals ovals] for a window group."
  [spec]
  (let [pc (row-comparator (map #(or (:coll %) "BINARY") (:part spec)))
        oc (compare-keys-fn (map (fn [o] [(:coll o) (:desc o) (:nulls o)]) (:order spec)))]
    (reify Comparator
      (compare [_ a b]
        (let [c (.compare ^Comparator pc (first a) (first b))]
          (if (zero? c) (.compare ^Comparator oc (second a) (second b)) c))))))

(defn- frame-offset [b ^objects env0 start? range?]
  (let [x ((:f b) env0)
        x (if (string? x) (v/apply-numeric-text x true) x)
        x (if (and (not range?) (instance? Double x)) (v/integer-affinity (double x)) x)
        what (if start? "starting" "ending")]
    (if range?
      (if (and (number? x) (>= (double x) 0.0)) x
          (err (str "frame " what " offset must be a non-negative number")))
      (if (and (instance? Long x) (>= (long x) 0)) (long x)
          (err (str "frame " what " offset must be a non-negative integer"))))))

(defn- pos-int-arg [x msg]
  (let [x (if (string? x) (v/apply-numeric-text x true) x)
        x (if (instance? Double x) (v/integer-affinity (double x)) x)]
    (if (and (instance? Long x) (pos? (long x))) (long x) (err msg))))

(defn- eval-partition
  "Computes spec's values for one partition (positions -> snapshot indices), storing them into results."
  [spec ^objects snaps prow okeys ^objects results widx env0]
  (let [m (count prow)
        name (:name spec)
        snap (fn ^objects [k] (aget snaps (long (nth prow k))))
        ;; peer groups
        ocmp (compare-keys-fn (map (fn [o] [(:coll o) (:desc o) (:nulls o)]) (:order spec)))
        grp (int-array m)
        _ (dotimes [k m]
            (when (pos? k)
              (aset grp k (if (zero? (.compare ^Comparator ocmp (nth okeys (dec k)) (nth okeys k)))
                            (aget grp (dec k)) (inc (aget grp (dec k)))))))
        ng (if (pos? m) (inc (aget grp (dec m))) 0)
        gs (int-array ng) ge (int-array ng)
        _ (dotimes [k m] (let [g (aget grp k)] (when (or (zero? k) (not= g (aget grp (dec k)))) (aset gs g k)) (aset ge g k)))
        argv (let [cache (object-array m)]
               (fn [k] (or (aget cache k) (let [r (mapv #((:f %) (snap k)) (:args spec))] (aset cache k r) r))))
        put! (fn [k val] (aset ^objects (aget ^objects (snap k) (long (:win-slot spec))) (long widx) val))
        fr (or (:frame spec) {:type :range :start {:kind :unbounded-preceding} :end {:kind :current}})
        ftype (:type fr)
        range? (= ftype :range)
        st (:start fr) en (:end fr)
        soff (when (:f st) (frame-offset st env0 true range?))
        eoff (when (:f en) (frame-offset en env0 false range?))
        excl (:exclude fr)
        o1 (first (:order spec))
        desc (:desc o1)
        ocoll (:coll o1)
        key0 (fn [k] (first (nth okeys k)))
        nonnull (when (and range? o1 (or soff eoff))
                  (let [ks (filter #(not (nil? (key0 %))) (range m))]
                    (if (seq ks) [(first ks) (last ks)] [0 -1])))
        rtarget (fn [v off plus]
                  (if (number? v) (v/arith (if plus :add :sub) v off) v))
        range-start (fn [k off kind]
                      (let [v (key0 k)]
                        (if (nil? v) (aget gs (aget grp k))
                            (let [t (rtarget v off (if desc (= kind :preceding) (= kind :following)))
                                  [a b] nonnull]
                              (loop [j a] (if (and (<= j b) (let [c (v/sql-cmp (key0 j) t ocoll)] (if desc (> c 0) (< c 0))))
                                            (recur (inc j)) j))))))
        range-end (fn [k off kind]
                    (let [v (key0 k)]
                      (if (nil? v) (aget ge (aget grp k))
                          (let [t (rtarget v off (if desc (= kind :preceding) (= kind :following)))
                                [a b] nonnull]
                            (loop [j b] (if (and (>= j a) (let [c (v/sql-cmp (key0 j) t ocoll)] (if desc (< c 0) (> c 0))))
                                          (recur (dec j)) j))))))
        lo-of (fn [k]
                (let [g (aget grp k)]
                  (case (:kind st)
                    :unbounded-preceding 0
                    :current (if (= ftype :rows) k (aget gs g))
                    :preceding (case ftype
                                 :rows (- k (long soff))
                                 :groups (aget gs (max 0 (- g (long soff))))
                                 :range (range-start k soff :preceding))
                    :following (case ftype
                                 :rows (+ k (long soff))
                                 :groups (if (< (+ g (long soff)) ng) (aget gs (+ g (long soff))) m)
                                 :range (range-start k soff :following)))))
        hi-of (fn [k]
                (let [g (aget grp k)]
                  (case (:kind en)
                    :unbounded-following (dec m)
                    :current (if (= ftype :rows) k (aget ge g))
                    :preceding (case ftype
                                 :rows (- k (long eoff))
                                 :groups (if (>= (- g (long eoff)) 0) (aget ge (- g (long eoff))) -1)
                                 :range (range-end k eoff :preceding))
                    :following (case ftype
                                 :rows (+ k (long eoff))
                                 :groups (aget ge (min (dec ng) (+ g (long eoff))))
                                 :range (range-end k eoff :following)))))
        frame-pos (fn [k]
                    (let [lo (max 0 (long (lo-of k))) hi (min (dec m) (long (hi-of k)))
                          g (aget grp k)]
                      (for [j (range lo (inc hi))
                            :when (case excl
                                    :current-row (not= j k)
                                    :group (not= (aget grp j) g)
                                    :ties (or (= j k) (not= (aget grp j) g))
                                    true)]
                        j)))]
    (case name
      "row_number" (dotimes [k m] (put! k (long (inc k))))
      "rank" (dotimes [k m] (put! k (long (inc (aget gs (aget grp k))))))
      "dense_rank" (dotimes [k m] (put! k (long (inc (aget grp k)))))
      "percent_rank" (dotimes [k m] (put! k (if (> m 1) (/ (double (aget gs (aget grp k))) (double (dec m))) 0.0)))
      "cume_dist" (dotimes [k m] (put! k (/ (double (inc (aget ge (aget grp k)))) (double m))))
      "ntile" (dotimes [k m]
                (let [nn (pos-int-arg (first (argv k)) "argument of ntile must be a positive integer")
                      size (quot m nn)]
                  (put! k (if (zero? size)
                            (long (inc k))
                            (let [nlarge (- m (* nn size)) ismall (* nlarge (inc size))]
                              (if (< k ismall) (long (inc (quot k (inc size))))
                                  (long (+ 1 nlarge (quot (- k ismall) size)))))))))
      ("lag" "lead")
      (dotimes [k m]
        (let [a (argv k)
              off (if (> (count a) 1) (nth a 1) 1)
              off (if (instance? Double off) (v/integer-affinity (double off)) off)
              dflt (when (> (count a) 2) (nth a 2))]
          (put! k (if (instance? Long off)
                    (let [j (if (= name "lag") (- k (long off)) (+ k (long off)))]
                      (if (and (>= j 0) (< j m)) (first (argv j)) dflt))
                    dflt))))
      ("first_value" "last_value" "nth_value")
      (dotimes [k m]
        (let [ps (frame-pos k)]
          (put! k (case name
                    "first_value" (when-let [j (first ps)] (first (argv j)))
                    "last_value" (when-let [j (last ps)] (first (argv j)))
                    "nth_value" (let [nn (pos-int-arg (second (argv k)) "second argument to nth_value must be a positive integer")]
                                  (when-let [j (first (drop (dec nn) ps))] (first (argv j))))))))
      ;; aggregates over the frame
      (let [filt (:filter spec)
            step (fn [acc j] (when (or (nil? filt) (let [x (filt (snap j))] (and (not (nil? x)) (v/bool-value x))))
                               ((:step acc) (argv j))))
            running (and (= :unbounded-preceding (:kind st)) (nil? excl) (not (:f en)))]
        (if running
          (let [acc (make-acc spec) done (long-array [-1])]
            (dotimes [k m]
              (let [hi (min (dec m) (long (hi-of k)))]
                (loop [j (inc (aget done 0))] (when (<= j hi) (step acc j) (recur (inc j))))
                (aset done 0 (max (aget done 0) hi))
                (put! k ((:final acc))))))
          (dotimes [k m]
            (let [acc (make-acc spec)]
              (doseq [j (frame-pos k)] (step acc j))
              (put! k ((:final acc))))))))))

(defn- partition-by-cmp [xs same?]
  (if (empty? xs) []
      (reduce (fn [acc x] (let [l (peek acc)]
                            (if (same? (peek l) x) (conj (pop acc) (conj l x)) (conj acc [x]))))
              [[(first xs)]] (rest xs))))

(defn run-windows
  "Computes all window functions over the snapshots (env arrays). Returns the snapshots in output order."
  [specs ^java.util.List snap-list win-slot nslots]
  (let [snaps (.toArray snap-list)
        n (alength snaps)
        nspec (count specs)
        env0 (object-array nslots)
        specs (mapv #(assoc % :win-slot win-slot) specs)
        gkeys (vec (distinct (map :gkey specs)))
        rep (into {} (for [g gkeys] [g (first (filter #(= g (:gkey %)) specs))]))
        keys-of (into {} (for [g gkeys]
                           (let [s (rep g)]
                             [g (mapv (fn [^objects sn] [(mapv #((:f %) sn) (:part s)) (mapv #((:f %) sn) (:order s))])
                                      snaps)])))]
    (dotimes [i n] (aset ^objects (aget snaps i) (long win-slot) (object-array nspec)))
    (doseq [g gkeys]
      (let [s (rep g)
            ks (keys-of g)
            cmp (win-key-cmp s)
            pc (row-comparator (map #(or (:coll %) "BINARY") (:part s)))
            idx (sort (fn [a b] (let [c (.compare ^Comparator cmp (nth ks a) (nth ks b))] (if (zero? c) (compare a b) c)))
                      (range n))
            parts (partition-by-cmp idx (fn [a b] (zero? (.compare ^Comparator pc (first (nth ks a)) (first (nth ks b))))))]
        (doseq [prow parts]
          (let [prow (vec prow) okeys (mapv #(second (nth ks %)) prow)]
            (doseq [[wi sp] (map-indexed vector specs) :when (= g (:gkey sp))]
              (eval-partition sp snaps prow okeys nil wi env0))))))
    (let [cmps (mapv (fn [g] [(keys-of g) (win-key-cmp (rep g))]) gkeys)
          order (sort (fn [a b]
                        (loop [cs cmps]
                          (if (empty? cs) (compare a b)
                              (let [[ks c] (first cs) r (.compare ^Comparator c (nth ks a) (nth ks b))]
                                (if (zero? r) (recur (rest cs)) r)))))
                      (range n))]
      (map #(aget snaps (long %)) order))))


;; ====================================================================== SELECT compilation

(defn- table-of
  ([ctx name] (table-of ctx name nil))
  ([ctx name sch]
   (let [t (get-in (:db ctx) [:tables (lc name)])
         ls (lc sch)
         t (when (and t (or (nil? ls) (and (= ls "main") (not (:temp t))) (and (= ls "temp") (:temp t)))) t)]
     (or t (schema/master-table (:db ctx) (lc name) sch)))))

(defn- view-of [ctx name]
  (get-in (:db ctx) [:views (lc name)]))

(defn row-comparator
  "Comparator over vectors of values with per-position collations."
  [colls]
  (let [colls (vec colls) n (count colls)]
    (reify Comparator
      (compare [_ a b]
        (loop [i 0]
          (if (>= i n) 0
              (let [c (v/sql-cmp (nth a i) (nth b i) (nth colls i))]
                (if (zero? c) (recur (inc i)) c))))))))

(defn table-rows
  "Rows of a table in rowid order (each row a vector with the rowid appended)."
  [tbl]
  (vals (:rows tbl)))

(declare index-order-rows check-indexed-by)

(defonce ^{:doc "Table-valued function resolvers: each (fn [lc-name db]) -> nil or
  {:cols [visible column names] :hidden [hidden argument column names] :affs [optional]
   :rows (fn [db arg-values]) -> seq of row vectors of the visible columns}.
  Arguments are bound to the hidden columns in order; the hidden columns read back the arguments."}
  tvf-resolvers (atom []))

(defn- tvf-entry [db lname]
  (some #(% lname db) @tvf-resolvers))

(defn dedup-names
  "sqlite3ColumnsFromExprList: makes column names unique (case-insensitively) by appending :N."
  [names]
  (let [seen (java.util.HashSet.)]
    (mapv (fn [^String n]
            (loop [nm n cnt 1]
              (if (.contains seen (lc nm))
                (let [base (let [j (loop [j (dec (count nm))] (if (and (> j 0) (Character/isDigit (.charAt nm j))) (recur (dec j)) j))]
                             (if (and (pos? (count nm)) (= \: (.charAt nm (int j)))) (subs nm 0 j) nm))]
                  (recur (str base ":" cnt) (inc cnt)))
                (do (.add seen (lc nm)) nm))))
          names)))

(declare compile-cte)

(defn- sub-source [sub alias tname cols]
  (let [cols (vec cols)]
    {:alias (lc alias) :tname tname :cols cols :lcols (mapv lc cols)
     :affs (:affs sub) :colls (:colls sub) :rowid? false :ncols (count cols) :hidden #{} :sub sub}))

(defn- make-source [ctx item]
  (cond
    (:tvf item)
    (let [lname (lc (:tvf item))
          ent (or (tvf-entry (:db ctx) lname) (err (str "no such table: " (:tvf item))))
          nargs (count (:args item))
          _ (when (> nargs (count (:hidden ent)))
              (err (str "too many arguments on " (:tvf item) "() - max " (count (:hidden ent)))))
          cols (vec (concat (:cols ent) (:hidden ent)))
          slot (new-slot! ctx)]
      {:alias (lc (or (:alias item) (:tvf item))) :tname (:tvf item) :cols cols :lcols (mapv lc cols)
       :affs (vec (take (count cols) (concat (:affs ent) (repeat nil)))) :colls (vec (repeat (count cols) nil))
       :slot slot :rowid? false :ncols (count cols) :hidden (set (map lc (:hidden ent)))
       :tvf ent :tvf-args (:args item)})
    (and (:table item) (nil? (:schema item)) (get (:ctes ctx) (lc (:table item))))
    (let [lname (lc (:table item))
          cte (get (:ctes ctx) lname)]
      (if (:rec-ref item)
        (let [rc (get (:rec-cells ctx) lname)
              cell (:cell rc)]
          (assoc (sub-source {:affs (:affs rc) :colls (:colls rc) :rows-fn (fn [_] @cell) :lazy true}
                             (or (:alias item) (:table item)) (:name cte) (:cols rc))
                 :slot (new-slot! ctx)))
        (do
          (when-let [m (get (:cte-err ctx) lname)] (err (str m (:name cte))))
          (let [sub (compile-cte ctx cte)]
            (assoc (sub-source sub (or (:alias item) (:table item)) (:name cte) (:cte-cols sub))
                   :slot (new-slot! ctx))))))
    (:table item)
    (let [name (:table item)
          tbl (table-of ctx name (:schema item))
          view (when-not tbl (view-of ctx name))]
      (cond
        tbl (let [slot (new-slot! ctx)
                  cols (mapv :name (:cols tbl))]
              (when (and (:indexed-by item) (not= :none (:indexed-by item)))
                (when-not (get-in (:db ctx) [:indexes (lc (:indexed-by item))])
                  (err (str "no such index: " (:indexed-by item)))))
              {:alias (lc (or (:alias item) name)) :tname (:name tbl) :cols cols :lcols (mapv lc cols)
               :affs (mapv :aff (:cols tbl)) :colls (mapv :coll (:cols tbl))
               :slot slot :rowid? true :ncols (count cols) :hidden #{} :table tbl
               :indexed-by (:indexed-by item)})
        view (let [sub (compile-select (assoc ctx :scope nil :ctes nil :cte-err nil :rec-cells nil :sql (or (:src view) (:sql ctx))) (:select view))
                   slot (new-slot! ctx)
                   cols (dedup-names (or (:cols view) (:names sub)))]
               {:alias (lc (or (:alias item) name)) :tname name :cols (vec cols) :lcols (mapv lc cols)
                :affs (:affs sub) :colls (:colls sub) :slot slot :rowid? false :ncols (count cols)
                :hidden #{} :sub sub})
        :else (err (str "no such table: " (if (:schema item) (str (:schema item) "." name) name)))))
    (:nested item)
    (let [items (:nested item)]
      (if (and (= 1 (count items)) (nil? (:alias item)))
        (make-source ctx (merge (first items) (select-keys item [:join :on :using])))
        (let [sel {:body {:type :core :cols [{:star true}] :from items}}
              sub (compile-select ctx sel)
              slot (new-slot! ctx)
              cols (dedup-names (:names sub))
              ;; qualified names of the inner tables still resolve: alias -> {lcol idx}
              quals (reduce (fn [m [i [a c]]] (if (or (nil? a) (get-in m [a c])) m (assoc-in m [a c] i)))
                            {} (for [[i ps] (map-indexed vector (:star-quals sub)) pr ps] [i pr]))]
          (assoc (sub-source sub (:alias item) nil cols) :slot slot :quals quals))))
    (:subq item)
    (let [sub (compile-select ctx (:subq item))
          slot (new-slot! ctx)
          cols (dedup-names (:names sub))]
      {:alias (lc (:alias item)) :tname nil :cols (vec cols) :lcols (mapv lc cols)
       :affs (:affs sub) :colls (:colls sub) :slot slot :rowid? false :ncols (count cols) :hidden #{} :sub sub})
    :else (err "unsupported FROM item")))

(defn- split-and [e]
  (if (and (= :bin (:op e)) (= :and (:o e)))
    (concat (split-and (:l e)) (split-and (:r e)))
    [e]))

(defn const-term?
  "An expression with no column references or subqueries (SQLite evaluates such WHERE terms once,
  before scanning)."
  [e]
  (letfn [(walk [e]
            (cond (map? e) (and (not (#{:col :subq :exists} (:op e)))
                                (not (and (= :in (:op e)) (or (:select e) (:table e))))
                                (every? walk (vals e)))
                  (sequential? e) (every? walk e)
                  :else true))]
    (walk e)))

(defn where-precheck
  "Compiles the constant top-level terms of a WHERE clause; returns (fn [env]) -> true if scanning may proceed."
  [ctx where-ast]
  (let [fs (vec (for [t (split-and where-ast) :when (const-term? t)] (condf (cx ctx t))))]
    (when (seq fs)
      (fn [env] (every? (fn [f] (let [x (f env)] (and (not (nil? x)) (v/bool-value x)))) fs)))))

(defn- expand-star
  "Result column list with * expanded: [{:e ast-or-nil :src-col [slot idx] :name ...}]"
  [ctx sources cols]
  (vec
   (mapcat
    (fn [rc]
      (cond
        (and (:star rc) (nil? (:table rc)))
        (if (empty? sources)
          (err "no tables specified")
          (if-let [[dup col] (first (for [[a ss] (group-by :alias sources) :when (> (count ss) 1)
                                          :let [d (second ss)
                                                c (first (remove #(contains? (:hidden d) (lc %)) (:cols d)))]
                                          :when c]
                                      [d c]))]
            (err (str "ambiguous column name: main." (:tname dup) "." col))
          (for [s sources, i (range (:ncols s))
                :when (not (contains? (:hidden s) (nth (:lcols s) i)))]
            {:star-col [s i] :name (nth (:cols s) i) :unq true})))
        (:star rc)
        (let [s (first (filter #(= (:alias %) (lc (:table rc))) sources))]
          (when-not s (err (str "no such table: " (:table rc))))
          (for [i (range (:ncols s))] {:star-col [s i] :name (nth (:cols s) i)}))
        :else [rc]))
    cols)))

(defn- col-name [ctx rc]
  (cond
    (:alias rc) (:alias rc)
    (:star-col rc) (:name rc)
    (= :col (:op (:e rc))) (:name (:e rc))
    :else (text-of-node ctx (:e rc))))

(defn- order-term-index
  "If ORDER BY term refers to a result column (integer or alias), returns its 0-based index."
  [ctx term names aliases nth-word]
  (let [e (:e term)]
    (cond
      (and (= :lit (:op e)) (= :int (:kind e)))
      (let [k (long (lit-value e))]
        (if (or (< k 1) (> k (count names)))
          (err (str nth-word " ORDER BY term out of range - should be between 1 and " (count names)))
          (dec k)))
      (and (= :col (:op e)) (nil? (:table e)))
      (let [i (.indexOf ^java.util.List aliases (lc (:name e)))]
        (when (>= i 0) i))
      :else nil)))

(defn ordinal [n]
  (let [n (long n)
        suf (if (and (>= (mod n 100) 11) (<= (mod n 100) 13))
              "th"
              (case (int (mod n 10)) 1 "st" 2 "nd" 3 "rd" "th"))]
    (str n suf)))

(defn- compare-keys-fn
  "Builds comparator over sort-key vectors with [coll desc nulls] specs."
  [specs]
  (let [specs (vec specs) n (count specs)]
    (reify Comparator
      (compare [_ a b]
        (loop [i 0]
          (if (>= i n) 0
              (let [[coll desc nulls] (nth specs i)
                    x (nth a i) y (nth b i)
                    c (cond
                        (and nulls (nil? x) (nil? y)) 0
                        (and nulls (nil? x)) (if (= nulls :first) -1 1)
                        (and nulls (nil? y)) (if (= nulls :first) 1 -1)
                        :else (let [c (v/sql-cmp x y coll)] (if desc (- c) c)))]
                (if (zero? c) (recur (inc i)) c))))))))

(defn- eval-limit [ctx e what]
  (when e
    (let [c (cx (assoc ctx :scope nil) e)
          x ((:f c) nil)
          x (if (string? x) (v/apply-numeric-text x true) x)
          x (if (instance? Double x) (v/integer-affinity (double x)) x)]
      (if (instance? Long x)
        x
        (err "datatype mismatch")))))

(defn- core-has-agg? [core]
  (let [found (atom false)]
    (letfn [(walk [e]
              (cond
                (map? e) (cond
                           (#{:subq :exists} (:op e)) nil
                           (and (= :in (:op e)) (:select e)) (walk (:e e))
                           (agg-call? e) (reset! found true)
                           :else (doseq [[k x] e :when (not= k :select)] (walk x)))
                (sequential? e) (doseq [x e] (walk x))))]
      (doseq [rc (:cols core)] (walk (:e rc)))
      (walk (:having core)))
    @found))


;; ---------------------------------------------------------------------- covering-index full scans

(defn- log-est
  "sqlite3LogEst."
  ^long [^long x]
  (let [a [0 2 3 5 6 7 8 9]]
    (if (< x 8)
      (if (< x 2) 0
          (loop [y 40 x x] (if (< x 8) (recur (- y 10) (bit-shift-left x 1)) (+ (nth a (bit-and x 7)) y -10))))
      (let [i (- 60 (Long/numberOfLeadingZeros x))]
        (+ (nth a (bit-and (unsigned-bit-shift-right x i) 7)) 40 (* i 10) -10)))))

(defn- col-size-est
  "Column width estimate from the declared type (sqlite3AffinityType's szEst)."
  ^long [col]
  (let [t (:type col)]
    (if (or (nil? t) (= "" t))
      1
      (let [aff (v/type-affinity t)
            u (str/upper-case t)]
        (if (#{:text :blob} aff)
          (let [ci (.indexOf u "CHAR")
                vv (if (>= ci 0)
                     (let [m (re-find #"[0-9]+" (subs u ci))] (if m (Long/parseLong m) 0))
                     (if (and (= aff :blob) (re-find #"BLOB\(" u))
                       (let [m (re-find #"[0-9]+" (subs u (.indexOf u "BLOB")))] (if m (Long/parseLong m) 0))
                       16))]
            (min 255 (inc (quot vv 4))))
          1)))))

(defn- term-target
  "If top-level WHERE conjunct t can drive a lookup on table source src, returns [col-idx class coll]
  where class is :eq or :range and col-idx is the table column (ncols = rowid)."
  [src t]
  (let [ncols (:ncols src)
        tbl (:table src)
        col-idx (fn [e] (when (and (= :col (:op e))
                                   (or (nil? (:table e)) (= (lc (:table e)) (:alias src))))
                          (let [i (.indexOf ^java.util.List (:lcols src) (lc (:name e)))]
                            (cond (>= i 0) (if (= i (:ipk tbl)) ncols i)
                                  (#{"rowid" "oid" "_rowid_"} (lc (:name e))) ncols
                                  :else nil))))
        refs-me? (fn refs [e] (cond (map? e) (or (some? (col-idx e)) (some refs (vals e)))
                                    (sequential? e) (some refs e)
                                    :else false))
        coll-of (fn [ce other]
                  (cond (= :collate (:op other)) (str/upper-case (:coll other))
                        (= :collate (:op ce)) (str/upper-case (:coll ce))
                        :else (let [i (col-idx ce)] (or (when (and i (< i ncols)) (:coll (nth (:cols tbl) i))) "BINARY"))))
        side (fn [a b cls]
               (let [a0 (if (= :collate (:op a)) (:e a) a)
                     i (col-idx a0)]
                 (when (and i (not (refs-me? b)))
                   [i cls (coll-of a b)])))]
    (case (:op t)
      :bin (when-let [cls (case (:o t) :eq :eq (:lt :le :gt :ge) :range nil)]
             (or (side (:l t) (:r t) cls) (side (:r t) (:l t) cls)))
      :is (when-not (:not t) (or (side (:l t) (:r t) :eq) (side (:r t) (:l t) :eq)))
      :in (when (and (not (:not t)) (or (:list t) (:select t)) (not (refs-me? (:list t))))
            (side (:e t) {:op :lit} :eq))
      :between (when (and (not (:not t)) (not (refs-me? (:lo t))) (not (refs-me? (:hi t))))
                 (side (:e t) {:op :lit} :range))
      :isnull (side (:e t) {:op :lit} :eq)
      nil)))

(defn- idx-width ^long [tbl idx]
  (let [ncols (count (:cols tbl)) ipk (:ipk tbl)]
    (+ 1 (reduce + (map (fn [ic] (let [c (:col ic)]
                                   (if (or (= c ncols) (= c ipk)) 1 (col-size-est (nth (:cols tbl) c)))))
                        (:cols idx))))))

(defn search-index
  "The index SQLite would search for the WHERE terms, :rowid for a rowid lookup, or nil if none."
  [db src terms refcols]
  (let [tbl (get-in db [:tables (lc (:tname src))])
        ncols (count (:cols tbl))
        ipk (:ipk tbl)
        targets (keep #(term-target src %) terms)]
    (cond
      (empty? targets) nil
      (some #(= ncols (first %)) targets) :rowid
      :else
      (let [need (set (remove #(or (= % ncols) (= % ipk)) refcols))
            cands (for [idx (sort-by :seq > (filter #(= (:table %) (lc (:tname src))) (vals (:indexes db))))
                        :when (and (nil? (:where idx)) (every? :col (:cols idx)))
                        :let [ic0 (first (:cols idx))
                              c0 (let [c (:col ic0)] (if (= c ipk) ncols c))
                              hits (filter #(and (= (first %) c0) (= (nth % 2) (:coll ic0))) targets)]
                        :when (seq hits)
                        :let [cls (if (some #(= :eq (second %)) hits) 0 1)
                              covering (if (every? (set (map :col (:cols idx))) need) 0 1)
                              w (idx-width tbl idx)]]
                    [[cls covering w] idx])]
        (when (seq cands)
          (second (reduce (fn [best c] (if (neg? (compare (first c) (first best))) c best)) (first cands) (rest cands))))))))

(defn covering-index
  "The index SQLite would use for a full scan of src covering the referenced columns, or nil."
  [db src refcols]
  (let [tbl (get-in db [:tables (lc (:tname src))])
        ncols (count (:cols tbl))
        ipk (:ipk tbl)
        need (set (remove #(or (= % ncols) (= % ipk)) refcols))
        sz-tab (log-est (* 4 (+ (reduce + (map col-size-est (:cols tbl))) (if ipk 0 1))))
        cands (for [idx (sort-by :seq > (filter #(= (:table %) (lc (:tname src))) (vals (:indexes db))))
                    :when (and (nil? (:where idx)) (every? :col (:cols idx)))
                    :let [cs (set (map :col (:cols idx)))]
                    :when (every? cs need)
                    :let [w (+ 1 (reduce + (map (fn [ic] (let [c (:col ic)]
                                                           (if (or (= c ncols) (= c ipk)) 1 (col-size-est (nth (:cols tbl) c)))))
                                                (:cols idx))))
                          sz (log-est (* 4 w))]
                    :when (< sz sz-tab)]
                [(quot (* 15 sz) sz-tab) idx])]
    (when (seq cands)
      (second (reduce (fn [best c] (if (< (long (first c)) (long (first best))) c best)) (first cands) (rest cands))))))

(defn- make-scanner
  "Returns fn [env] -> seq of rows for a source."
  [ctx src where-terms]
  (cond
    (:tvf src)
    (let [ent (:tvf src)
          argfs (mapv #(:f (cx ctx %)) (:tvf-args src))
          nh (count (:hidden ent))
          db (:db ctx)]
      (fn [env]
        (let [args (mapv #(% env) argfs)
              hid (vec (take nh (concat args (repeat nil))))]
          (map (fn [r] (into (vec r) hid)) ((:rows ent) db args)))))
    (:table src)
    (let [tname (lc (:tname src))
          ib (:indexed-by src)]
      (if (and ib (not= ib :none))
        (let [idx (get-in (:db ctx) [:indexes (lc ib)])]
          (when (and (:where idx) (not (check-indexed-by ctx idx where-terms src)))
            (err "no query solution"))
          (fn [_] (index-order-rows (:db ctx) idx)))
        (let [plan (when (and (:scan-refs src) (nil? ib))
                     (let [si (search-index (:db ctx) src (or where-terms []) (:scan-refs src))]
                       (cond (= si :rowid) nil
                             si si
                             :else (or (first (for [idx (sort-by :seq > (filter #(= (:table %) tname) (vals (:indexes (:db ctx)))))
                                                    :when (and (:where idx) (seq where-terms)
                                                               (check-indexed-by ctx idx where-terms src))]
                                                idx))
                                       (covering-index (:db ctx) src (:scan-refs src))))))]
          (if plan
            (fn [_] (index-order-rows (:db ctx) plan))
            (if (:virtual (:table src))
              (let [t (:table src)] (fn [_] (table-rows t)))
              (fn [_] (table-rows (get-in (:db ctx) [:tables tname]))))))))
    (:sub src)
    (let [sub (:sub src)]
      (if (or (:correlated sub) (:lazy sub))
        (fn [env] (run-select-rows sub env))
        (let [cache (volatile! nil)]
          (fn [env] (or @cache (vreset! cache (vec (run-select-rows sub env))))))))))

(defn compile-core
  "Compiles a simple SELECT core. Returns {:names :affs :colls :run (fn [env emit])} where emit receives
  [row sort-keys]. order-terms are resolved here when provided (for simple selects)."
  [ctx core order limit offset]
  (let [ctx (assoc ctx :pending nil :merge nil)]
  (if (= :values (:type core))
    (let [rows (mapv (fn [r] (mapv #(cx (assoc ctx :scope (:scope ctx)) %) r)) (:rows core))
          n (count (first rows))]
      (when (some #(not= n (count %)) rows)
        (err "all VALUES must have the same number of terms"))
      {:names (vec (for [i (range n)] (str "column" (inc i))))
       :affs (vec (repeat n nil)) :colls (vec (repeat n nil)) :ecolls (vec (repeat n nil))
       :exprs (first rows)
       :outer-refs (atom false)
       :order-info [[] []]
       :run (fn [env emit]
              (doseq [r rows] (emit (mapv #((:f %) env) r) nil)))})
    (let [from (:from core)
          ;; FROM-subqueries see the outer scope only
          sources (vec (for [item (or from [])] (make-source ctx item)))
          ;; USING / NATURAL hide right-hand duplicate columns from *
          [sources join-conds]
          (loop [i 0 srcs sources conds []]
            (if (>= i (count srcs))
              [srcs conds]
              (let [item (nth from i)
                    s (nth srcs i)
                    jt (:join item)
                    using (cond (:using item) (:using item)
                                (#{:natural :natural-left :natural-right :natural-full} jt)
                                (let [prev-cols (set (mapcat :lcols (take i srcs)))]
                                  (filter #(prev-cols (lc %)) (:cols s)))
                                :else nil)
                    s (if using (update s :hidden into (map lc using)) s)
                    srcs (assoc srcs i s)
                    ;; RIGHT/FULL JOIN ... USING: the unqualified column is coalesce(left, right)
                    srcs (if (and using (#{:right :full :natural-right :natural-full} jt))
                           (reduce (fn [srcs u]
                                     (let [lu (lc u)
                                           li (first (filter #(find-in-source (nth srcs %) lu) (range i)))
                                           ri (find-in-source s lu)]
                                       (if (and li ri) (assoc-in srcs [li :coalesce lu] [(:slot s) ri]) srcs)))
                                   srcs using)
                           srcs)]
                (recur (inc i) srcs (conj conds using)))))
          scope {:sources sources :parent (:scope ctx)}
          outer-refs (atom false)
          ctx2 (assoc ctx :scope scope :outer-refs outer-refs)
          left-join (vec (for [item (or from [])] (boolean (#{:left :natural-left :full :natural-full} (:join item)))))
          right-join (vec (for [item (or from [])] (boolean (#{:right :full :natural-right :natural-full} (:join item)))))
          any-rj (some true? right-join)
          rcols (expand-star ctx2 sources (:cols core))
          names (mapv #(col-name ctx2 %) rcols)
          aliases (mapv #(lc (:alias %)) rcols)
          is-agg (or (seq (:group core)) (core-has-agg? core))
          aggs (atom [])
          agg-slot (new-slot! ctx)
          actx (if is-agg (assoc ctx2 :aggs aggs :agg-slot agg-slot) (assoc ctx2 :aggs nil))
          wins (atom [])
          win-slot (new-slot! ctx)
          windefs (reduce (fn [m [n d]] (assoc m (lc n) (resolve-window-def (assoc actx :windefs m) d)))
                          {} (:windows core))
          wctx (assoc actx :wins wins :win-slot win-slot :windefs windefs)
          ;; result column compile
          rcomp (mapv (fn [rc]
                        (if-let [[s i] (:star-col rc)]
                          (let [sl (long (:slot s)) i (long i)
                                [aslot aidx] (when (:unq rc) (get (:coalesce s) (nth (:lcols s) i)))]
                            (when-let [refs (:refs ctx)] (swap! refs conj [sl i]))
                            {:f (if aslot
                                  (let [aslot (long aslot) aidx (long aidx)]
                                    (fn [^objects env] (let [x (nth (aget env sl) i)] (if (nil? x) (nth (aget env aslot) aidx) x))))
                                  (fn [^objects env] (nth (aget env sl) i)))
                             :aff (nth (:affs s) i)
                             :coll (if (and (:table s) (= i (:ipk (:table s)))) nil (or (nth (:colls s) i) "BINARY"))
                             :col {:slot (:slot s) :idx i :depth 0}})
                          (cx wctx (:e rc))))
                      rcols)
          ;; alias substitution for WHERE/GROUP BY/HAVING is approximated by compiling the aliased expr
          alias-map (into {} (for [[a rc] (map vector aliases rcols) :when a] [a (:e rc)]))
          subst (fn subst [e]
                  (cond
                    (and (map? e) (= :col (:op e)) (nil? (:table e)) (contains? alias-map (lc (:name e))))
                    (try (resolve-column ctx2 nil (:name e) nil) e
                         (catch clojure.lang.ExceptionInfo _ (get alias-map (lc (:name e)))))
                    (map? e) (if (#{:subq :exists} (:op e)) e (into {} (map (fn [[k x]] [k (if (#{:select} k) x (subst x))]) e)))
                    (vector? e) (mapv subst e)
                    :else e))
          where-c (when (:where core)
                    (let [c (cx (assoc ctx2 :aggs nil) (subst (:where core)))] c))
          where-f (when where-c (condf where-c))
          where-pre (when (:where core) (where-precheck (assoc ctx2 :aggs nil) (subst (:where core))))
          ;; rowid IN (uncorrelated subquery): SQLite drives the scan from the subquery, which therefore runs first
          in-pre (when (and (:where core) (= 1 (count sources)) (:table (first sources)))
                   (let [s0 (first sources)]
                     (vec (for [t (split-and (:where core))
                                :when (and (= :in (:op t)) (:select t) (not (:not t)) (= :col (:op (:e t)))
                                           (let [ce (:e t) ln (lc (:name ce))
                                                 i (.indexOf ^java.util.List (:lcols s0) ln)]
                                             (and (or (nil? (:table ce)) (= (lc (:table ce)) (:alias s0)))
                                                  (if (>= i 0) (= i (:ipk (:table s0))) (#{"rowid" "oid" "_rowid_"} ln)))))
                                :let [sub (compile-select (assoc ctx2 :aggs nil) (:select t))]
                                :when (not (:correlated sub))]
                            sub))))
          ;; join conditions
          on-fns (vec (for [i (range (count sources))]
                        (let [item (nth from i)
                              using (nth join-conds i)
                              s (nth sources i)]
                          (cond
                            (:on item) (let [c (cx (assoc ctx2 :aggs nil) (:on item))] (condf c))
                            (seq using)
                            (let [preds (doall (for [u using]
                                          (let [lu (lc u)
                                                left (first (filter #(find-in-source % lu) (take i sources)))
                                                _ (when (or (nil? left) (nil? (find-in-source s lu)))
                                                    (err (str "cannot join using column " u " - column not present in both tables")))
                                                li (find-in-source left lu) ri (find-in-source s lu)
                                                lcmp {:f (let [sl (long (:slot left)) li (long li)] (fn [^objects env] (nth (aget env sl) li)))
                                                      :aff (nth (:affs left) li) :coll (or (nth (:colls left) li) "BINARY")}
                                                rcmp {:f (let [sl (long (:slot s)) ri (long ri)] (fn [^objects env] (nth (aget env sl) ri)))
                                                      :aff (nth (:affs s) ri) :coll (or (nth (:colls s) ri) "BINARY")}]
                                            (:f (make-compare :eq lcmp rcmp)))))]
                              (fn [env] (if (every? (fn [pf] (let [r (pf env)] (and (not (nil? r)) (v/bool-value r)))) preds) 1 0)))
                            :else nil))))
          nagg-before (count @aggs)
          group-c (mapv (fn [g]
                          (let [g (subst g)]
                            (if (and (= :lit (:op g)) (= :int (:kind g)))
                              (let [k (long (lit-value g))]
                                (if (or (< k 1) (> k (count rcols)))
                                  (err (str (ordinal 1) " GROUP BY term out of range - should be between 1 and " (count rcols)))
                                  (nth rcomp (dec k))))
                              (let [c (cx (assoc ctx2 :aggs aggs :agg-slot agg-slot :no-agg-msg true) g)]
                                c))))
                        (:group core))
          _ (when (and (seq (:group core)) (> (count @aggs) nagg-before))
              (err "aggregate functions are not allowed in the GROUP BY clause"))
          having-c (when (:having core)
                     (when-not is-agg (err "a GROUP BY clause is required before HAVING"))
                     (cx actx (subst (:having core))))
          ;; ORDER BY
          order-c (when order
                    (vec (map-indexed
                          (fn [i term]
                            (let [ri (order-term-index ctx2 term names aliases (ordinal (inc i)))
                                  c (if ri (nth rcomp ri) (cx wctx (:e term)))]
                              {:f (:f c) :ri ri :coll (or (:coll c) "BINARY") :desc (:desc term) :nulls (:nulls term)
                               :col (:col c)}))
                          order)))
          nsrc (count sources)
          win-specs @wins
          win? (boolean (seq win-specs))
          rowid-order (when (and order-c (= 1 nsrc) (:table (first sources)) (not is-agg) (not (:distinct core)) (not win?)
                                 (nil? (:indexed-by (first sources))))
                        (let [o (first order-c) col (:col o) s0 (first sources)]
                          (when (and col (= (:slot col) (:slot s0)) (zero? (long (:depth col)))
                                     (or (= (:idx col) (:ncols s0)) (= (:idx col) (:ipk (:table s0)))))
                            (if (:desc o) :desc :asc))))
          null-rows (mapv (fn [s] (vec (repeat (inc (:ncols s)) nil))) sources)
          scanners (if (= rowid-order :desc)
                     (let [tname (lc (:tname (first sources)))]
                       [(fn [_] (map val (rseq (:rows (get-in (:db ctx2) [:tables tname])))))])
                     (mapv #(make-scanner ctx2
                                          (if (and (= 1 nsrc) (not rowid-order) (:refs ctx2) (:table %))
                                            (assoc % :scan-refs (set (keep (fn [[sl i]] (when (= sl (:slot %)) i)) @(:refs ctx2))))
                                            %)
                                          (when (:where core) (split-and (:where core))))
                           sources))
          rfs (mapv :f rcomp)
          ofs (mapv :f order-c)
          agg-specs @aggs
          minmax-only (and (seq agg-specs) (every? #(#{"min" "max"} (:name %)) agg-specs))
          has-minmax (some #(#{"min" "max"} (:name %)) agg-specs)
          control (last (keep-indexed (fn [i sp] (when (#{"min" "max"} (:name sp)) i)) agg-specs))
          gfs (mapv :f group-c)
          gcolls (mapv #(or (:coll %) "BINARY") group-c)
          having-f (when having-c (condf having-c))
          distinct (:distinct core)
          slots (mapv :slot sources)
          nslots-a (:slots ctx)
          produce (if win?
                    (fn [^objects env ^java.util.List sfl _ _] (.add sfl (aclone env)))
                    (fn [^objects env _ emit seen]
                      (let [row (mapv #(% env) rfs)]
                        (when (or (nil? seen) (.add ^TreeSet seen row))
                          (emit row (when (seq ofs) (mapv #(% env) ofs)))))))
          finish (fn [^java.util.List sfl emit seen]
                   (when win?
                     (doseq [^objects env (run-windows win-specs sfl win-slot (+ 2 (long @nslots-a)))]
                       (let [row (mapv #(% env) rfs)]
                         (when (or (nil? seen) (.add ^TreeSet seen row))
                           (emit row (when (seq ofs) (mapv #(% env) ofs))))))))
          ;; nested loop over sources
          scan (fn scan [^objects env k]
                 (let [rsets (when any-rj (mapv (fn [r] (when r (java.util.HashSet.))) right-join))]
                 (letfn [(level [i]
                           (if (= i nsrc)
                             (when (or (nil? where-f)
                                       (let [w (where-f env)] (and (not (nil? w)) (v/bool-value w))))
                               (k))
                             (let [slot (long (nth slots i))
                                   onf (nth on-fns i)
                                   lj (nth left-join i)
                                   ^java.util.HashSet rset (when rsets (nth rsets i))
                                   matched (boolean-array 1)
                                   cnt (long-array 1)]
                               (doseq [row ((nth scanners i) env)]
                                 (aset env slot row)
                                 (when (or (nil? onf) (let [r (onf env)] (and (not (nil? r)) (v/bool-value r))))
                                   (aset matched 0 true)
                                   (when rset (.add rset (aget cnt 0)))
                                   (level (inc i)))
                                 (aset cnt 0 (inc (aget cnt 0))))
                               (when (and lj (not (aget matched 0)))
                                 (aset env slot (nth null-rows i))
                                 (level (inc i))))))]
                   (when (or (nil? where-pre) (where-pre env))
                     (doseq [sub in-pre] (doall (run-select-rows sub env)))
                     (level 0)
                     ;; RIGHT/FULL JOIN fix-up: right rows never matched, with NULLs for every table to the left
                     (when rsets
                       (dotimes [i nsrc]
                         (when (nth right-join i)
                           (let [^java.util.HashSet rset (nth rsets i)
                                 slot (long (nth slots i))]
                             (doseq [[j row] (map-indexed vector ((nth scanners i) env))]
                               (when-not (.contains rset (long j))
                                 (dotimes [q i] (aset env (long (nth slots q)) (nth null-rows q)))
                                 (aset env slot row)
                                 (level (inc i))))))))))))
          run
          (if is-agg
            (fn [^objects env emit]
              (let [groups (if (seq gfs) (TreeMap. ^Comparator (row-comparator gcolls)) nil)
                    single (volatile! nil)
                    new-group (fn [] {:accs (mapv make-acc agg-specs) :snap (object-array nsrc) :init (boolean-array 1)})
                    snapshot! (fn [g] (let [^objects sn (:snap g)] (dotimes [i nsrc] (aset sn i (aget env (long (nth slots i)))))))]
                (scan env
                      (fn []
                        (let [g (if groups
                                  (let [key (mapv #(% env) gfs)]
                                    (or (.get ^TreeMap groups key)
                                        (let [g (new-group)] (.put ^TreeMap groups key g) g)))
                                  (or @single (vreset! single (new-group))))
                              results (mapv (fn [acc spec]
                                              (if (or (nil? (:filter spec))
                                                      (let [x ((:filter spec) env)] (and (not (nil? x)) (v/bool-value x))))
                                                ((:step acc) (mapv #((:f %) env) (:args spec)))
                                                false))
                                            (:accs g) agg-specs)
                              updated (when control (nth results control))]
                          (when (if control updated (not (aget ^booleans (:init g) 0)))
                            (aset ^booleans (:init g) 0 true)
                            (snapshot! g)))))
                (let [gs (if groups (vals groups) [(or @single (new-group))])
                      seen (when distinct (TreeSet. ^Comparator (row-comparator (map #(or (:coll %) "BINARY") rcomp))))]
                  (let [sfl (java.util.ArrayList.)]
                    (doseq [g gs]
                      (let [^objects sn (:snap g)]
                        (dotimes [i nsrc]
                          (aset env (long (nth slots i)) (or (aget sn i) (nth null-rows i)))))
                      (aset env (long agg-slot) (object-array (map #((:final %)) (:accs g))))
                      (when (or (nil? having-f) (let [h (having-f env)] (and (not (nil? h)) (v/bool-value h))))
                        (produce env sfl emit seen)))
                    (finish sfl emit seen)))))
            (fn [^objects env emit]
              (let [seen (when distinct (TreeSet. ^Comparator (row-comparator (map #(or (:coll %) "BINARY") rcomp))))
                    sfl (java.util.ArrayList.)]
                (scan env (fn [] (produce env sfl emit seen)))
                (finish sfl emit seen))))]
      {:names names
       :affs (mapv :aff rcomp)
       :colls (mapv :coll rcomp)
       :ecolls (mapv :ecoll rcomp)
       :exprs rcomp
       :order-specs (mapv (fn [o] [(:coll o) (:desc o) (:nulls o)]) order-c)
       :presorted (boolean rowid-order)
       :order-info [aliases (mapv (fn [rc] (if (:star-col rc) {:op :col :name (:name rc)} (:e rc))) rcols)]
       :star-quals (mapv (fn [rc] (when-let [[s i] (:star-col rc)]
                                    (cons [(:alias s) (nth (:lcols s) i)]
                                          (for [[a m] (:quals s) [c j] m :when (= j i)] [a c]))))
                         rcols)
       :outer-refs outer-refs
       :run run}))))

(defn- compound-colls [l r]
  (mapv (fn [a b] (or a b "BINARY")) (:colls l) (:colls r)))

(defn- compile-body
  "Compiles a select body (core or compound) without ORDER BY. Returns {:names :affs :colls :rows (fn [env]) }."
  [ctx body]
  (if (= :compound (:type body))
    (let [l (compile-body ctx (:left body))
          r (compile-body ctx (:right body))
          _ (when (not= (count (:names l)) (count (:names r)))
              (err (str "SELECTs to the left and right of "
                        (case (:op body) :union "UNION" :union-all "UNION ALL" :intersect "INTERSECT" :except "EXCEPT")
                        " do not have the same number of result columns")))
          colls (mapv (fn [a b] (or a b "BINARY")) (:colls l) (:colls r))
          cmp (row-comparator colls)
          op (:op body)
          merge? (:merge ctx)]
      {:names (:names l) :affs (:affs l) :colls colls :ecolls (:ecolls l) :exprs (:exprs l)
       :leftmost (or (:leftmost l) l)
       :order-infos (into (or (:order-infos l) [(:order-info l)]) (or (:order-infos r) [(:order-info r)]))
       :correlated (or (:correlated l) (:correlated r))
       :rows (fn [env]
               (let [lr ((:rows l) env) rr ((:rows r) env)]
                 (case op
                   :union-all (concat lr rr)
                   :union (if true
                            ;; ORDER BY compounds are merged: among equal rows the right-hand one is kept
                            (let [tl (TreeSet. ^Comparator cmp) tr (TreeSet. ^Comparator cmp)]
                              (doseq [x lr] (.add tl x))
                              (doseq [x rr] (.add tr x))
                              (doseq [x tr] (.remove tl x))
                              (doseq [x tr] (.add tl x))
                              (seq tl))
                            (let [ts (TreeSet. ^Comparator cmp)] (doseq [x (concat lr rr)] (.add ts x)) (seq ts)))
                   :intersect (let [rs (TreeSet. ^Comparator cmp) ts (TreeSet. ^Comparator cmp)]
                                (doseq [x rr] (.add rs x))
                                (doseq [x lr] (when (.contains rs x) (.add ts x)))
                                (seq ts))
                   :except (let [rs (TreeSet. ^Comparator cmp) ts (TreeSet. ^Comparator cmp)]
                             (doseq [x rr] (.add rs x))
                             (doseq [x lr] (when-not (.contains rs x) (.add ts x)))
                             (seq ts)))))})
    (let [c (compile-core ctx body nil nil nil)]
      (assoc c
             :correlated @(:outer-refs c)
             :rows (fn [env]
                     (let [out (ArrayList.)]
                       ((:run c) env (fn [row _] (.add out row)))
                       (seq out)))))))

(defn- apply-limit [rows limit offset]
  (let [rows (if (and offset (> (long offset) 0)) (drop offset rows) rows)
        rows (if (and limit (>= (long limit) 0)) (take limit rows) rows)]
    rows))

(declare compile-select* expr-equal?)

(defn- compound-order-index
  "resolveCompoundOrderBy: an integer, else an AS alias or an equal result expression of any SELECT,
  the left-most first."
  [ctx e n infos nth-word]
  (if (and (= :lit (:op e)) (= :int (:kind e)))
    (let [k (long (lit-value e))]
      (if (or (< k 1) (> k n))
        (err (str nth-word " ORDER BY term out of range - should be between 1 and " n))
        (dec k)))
    (let [strip-q (fn sq [e] (cond (and (map? e) (= :col (:op e))) (dissoc e :table :dq)
                                   (map? e) (into {} (for [[k x] e] [k (sq x)]))
                                   (vector? e) (mapv sq e)
                                   :else e))
          e0 (strip-q e)]
      (some (fn [[aliases asts]]
              (or (when (and (= :col (:op e)) (nil? (:table e)))
                    (let [i (.indexOf ^java.util.List aliases (lc (:name e)))] (when (>= i 0) i)))
                  (first (keep-indexed (fn [i a] (when (and a (expr-equal? (strip-q a) e0)) i)) asts))))
            infos))))

(def ^:dynamic *with* nil)

(defn add-ctes [ctx w]
  (let [env (atom nil)
        entries (reduce (fn [m c]
                          (let [ln (lc (:name c))]
                            (when (contains? m ln) (err (str "duplicate WITH table name: " (:name c))))
                            (assoc m ln (assoc c :env env))))
                        {} (:ctes w))
        all (merge (:ctes ctx) entries)]
    (reset! env all)
    (assoc ctx :ctes all)))

(defn compile-select
  "Compiles a full select (body + ORDER BY + LIMIT). Returns {:names :affs :colls :rows-fn (fn [env])}."
  [ctx sel]
  (if-let [w (:with sel)]
    (compile-select* (add-ctes ctx w) (dissoc sel :with))
    (compile-select* ctx sel)))

(defn- compound-terms
  "Flattens a left-deep compound body into [[nil t0] [op1 t1] ...]."
  [body]
  (if (= :compound (:type body))
    (conj (compound-terms (:left body)) [(:op body) (:right body)])
    [[nil body]]))

(defn- rebuild-compound [terms]
  (reduce (fn [l [op t]] {:type :compound :op op :left l :right t}) (second (first terms)) (rest terms)))

(defn- self-refs [core lname]
  (when (= :core (:type core))
    (count (filter #(and (:table %) (nil? (:schema %)) (= lname (lc (:table %)))) (:from core)))))

(defn- cte-check-cols [cte names]
  (when (and (:cols cte) (not= (count (:cols cte)) (count names)))
    (err (str "table " (:name cte) " has " (count names) " values for " (count (:cols cte)) " columns")))
  (dedup-names (or (:cols cte) names)))

(defn compile-cte
  "Compiles a CTE reference. Returns a compiled select with :cte-cols."
  [ctx cte]
  (let [lname (lc (:name cte))
        sel (:select cte)
        body (:body sel)
        ctx (assoc ctx :ctes @(:env cte))
        terms (compound-terms body)
        last-op (first (last terms))
        rec-idx (when (#{:union :union-all} last-op)
                  (loop [i (dec (count terms)) found nil]
                    (if (<= i 0)
                      found
                      (let [[op t] (nth terms i)
                            n (or (self-refs t lname) 0)]
                        (cond (> n 1) (err (str "multiple references to recursive table: " (:name cte)))
                              (and (= op last-op) (pos? n)) (recur (dec i) i)
                              :else found)))))]
    (if-not rec-idx
      (let [sub (compile-select (assoc ctx :cte-err (assoc (:cte-err ctx) lname "circular reference: ")) sel)]
        (assoc sub :cte-cols (cte-check-cols cte (:names sub))))
      (let [setup-sel {:body (rebuild-compound (subvec terms 0 rec-idx))}
            setup (compile-select (assoc ctx :cte-err (assoc (:cte-err ctx) lname "circular reference: ")) setup-sel)
            cols (cte-check-cols cte (:names setup))
            cell (volatile! nil)
            rctx (assoc ctx
                        :cte-err (assoc (:cte-err ctx) lname "recursive reference in a subquery: ")
                        :rec-cells (assoc (:rec-cells ctx) lname {:cell cell :cols cols :affs (:affs setup) :colls (:colls setup)}))
            recs (vec (for [[_ t] (subvec terms rec-idx)]
                        (let [t (update t :from (fn [items] (mapv #(if (and (:table %) (nil? (:schema %)) (= lname (lc (:table %))))
                                                                      (assoc % :rec-ref true) %) items)))]
                          (when (or (seq (:group t)) (core-has-agg? t))
                            (err "recursive aggregate queries not supported"))
                          (let [r (compile-select rctx {:body t})]
                            (when (not= (count (:names r)) (count cols))
                              (err (str "SELECTs to the left and right of "
                                        (case last-op :union "UNION" :union-all "UNION ALL")
                                        " do not have the same number of result columns")))
                            r))))
            colls (mapv #(or % "BINARY") (:colls setup))
            names (:names setup)
            aliases (mapv lc names)
            specs (when (:order sel)
                    (vec (map-indexed
                          (fn [i term]
                            (let [e (:e term)
                                  [e coll] (if (= :collate (:op e)) [(:e e) (check-coll (:coll e))] [e nil])
                                  infos (for [[_ t] terms :when (= :core (:type t))]
                                          [(mapv #(lc (:alias %)) (:cols t)) (mapv :e (:cols t))])
                                  ri (or (compound-order-index ctx e (count names) infos (ordinal (inc i)))
                                         (err (str (ordinal (inc i)) " ORDER BY term does not match any column in the result set")))]
                              [ri [(or coll (nth colls ri)) (:desc term) (:nulls term)]]))
                          (:order sel))))
            kcmp (when specs (compare-keys-fn (map second specs)))
            rowcmp (row-comparator colls)
            union? (= last-op :union)
            limit-e (:limit sel) offset-e (:offset sel)]
        {:names names :affs (:affs setup) :colls (:colls setup) :ecolls (:ecolls setup)
         :correlated (or (:correlated setup) (some :correlated recs))
         :lazy true
         :cte-cols cols
         :rows-fn
         (fn [env]
           (let [limit (eval-limit ctx limit-e "LIMIT")
                 offset (or (eval-limit ctx offset-e "OFFSET") 0)
                 seqno (long-array 1)
                 queue (if kcmp
                         (java.util.PriorityQueue. 16 (reify Comparator
                                                        (compare [_ a b]
                                                          (let [c (.compare ^Comparator kcmp (first a) (first b))]
                                                            (if (zero? c)
                                                              (if union?
                                                                (.compare ^Comparator rowcmp (nth a 2) (nth b 2))
                                                                (Long/compare (long (second a)) (long (second b))))
                                                              c)))))
                         (java.util.ArrayDeque.))
                 seen (when union? (TreeSet. ^Comparator rowcmp))
                 add! (fn [row]
                        (when (or (nil? seen) (.add ^TreeSet seen row))
                          (let [n (aget seqno 0)]
                            (aset seqno 0 (inc n))
                            (.add ^java.util.Queue queue (if kcmp [(mapv #(nth row (first %)) specs) n row] row)))))
                 poll (fn [] (let [x (.poll ^java.util.Queue queue)] (if kcmp (nth x 2) x)))
                 run-rec (fn [row]
                           (vreset! cell [(conj (vec row) nil)])
                           (doseq [r recs] (doseq [x (run-select-rows r env)] (add! x))))]
             (doseq [r (run-select-rows setup env)] (add! r))
             (letfn [(step [skip emitted]
                       (lazy-seq
                        (when (and (not (.isEmpty ^java.util.Collection queue))
                                   (or (nil? limit) (neg? (long limit)) (< (long emitted) (long limit))))
                          (let [row (poll)]
                            (if (pos? (long skip))
                              (do (run-rec row) (step (dec (long skip)) emitted))
                              (cons row (lazy-seq (run-rec row) (step skip (inc (long emitted))))))))))]
               (step offset 0))))}))))

(defn- compile-select*
  [ctx sel]
  (let [body (:body sel)
        order (:order sel)
        limit (eval-limit ctx (:limit sel) "LIMIT")
        offset (eval-limit ctx (:offset sel) "OFFSET")]
    (if (= :compound (:type body))
      (let [b (compile-body (if order (assoc ctx :merge true) ctx) body)
            names (:names b)
            lm (:leftmost b)
            aliases (mapv lc names)
            specs (when order
                    (vec (map-indexed
                          (fn [i term]
                            (let [e (:e term)
                                  [e coll] (if (= :collate (:op e)) [(:e e) (check-coll (:coll e))] [e nil])
                                  ri (or (compound-order-index ctx e (count names) (:order-infos b) (ordinal (inc i)))
                                         (err (str (ordinal (inc i)) " ORDER BY term does not match any column in the result set")))]
                              {:ri ri :coll (or coll (nth (:colls b) ri) "BINARY") :desc (:desc term) :nulls (:nulls term)}))
                          order)))
            specs (when specs
                    (let [used (set (map :ri specs))]
                      (into specs (for [i (range (count names)) :when (not (used i))]
                                    {:ri i :coll (or (nth (:colls b) i) "BINARY") :desc false :nulls nil}))))
            cmp (when specs (compare-keys-fn (map (fn [s] [(:coll s) (:desc s) (:nulls s)]) specs)))]
        {:names names :affs (:affs b) :colls (:colls b) :ecolls (:ecolls b) :correlated (:correlated b)
         :rows-fn (fn [env]
                    (let [rows ((:rows b) env)
                          rows (if cmp
                                 (let [keyed (map (fn [r] [(mapv #(nth r (:ri %)) specs) r]) rows)
                                       arr (to-array keyed)]
                                   (java.util.Arrays/sort arr (reify Comparator (compare [_ a b] (.compare ^Comparator cmp (first a) (first b)))))
                                   (map second arr))
                                 rows)]
                      (apply-limit rows limit offset)))})
      (let [c (compile-core ctx body order limit offset)
            cmp (when (and order (not (:presorted c))) (compare-keys-fn (:order-specs c)))]
        {:names (:names c) :affs (:affs c) :colls (:colls c) :ecolls (:ecolls c)
         :correlated @(:outer-refs c)
         :rows-fn (fn [env]
                    (if cmp
                      (let [out (ArrayList.)]
                        ((:run c) env (fn [row keys] (.add out [keys row])))
                        (let [arr (.toArray out)]
                          (java.util.Arrays/sort arr (reify Comparator (compare [_ a b] (.compare ^Comparator cmp (first a) (first b)))))
                          (apply-limit (map second arr) limit offset)))
                      (let [out (ArrayList.)
                            lim (when (and limit (>= (long limit) 0)) (+ (long limit) (max 0 (long (or offset 0)))))]
                        (try
                          ((:run c) env (fn [row _]
                                          (.add out row)
                                          (when (and lim (>= (.size out) (long lim))) (throw (ex-info "limit" {::stop true})))))
                          (catch clojure.lang.ExceptionInfo ex
                            (when-not (::stop (ex-data ex)) (throw ex))))
                        (apply-limit (seq out) limit offset))))
         :streaming (when-not cmp (:run c))
         :limit limit :offset offset}))))

(defn run-select-rows
  "Runs a compiled select; returns a seq of row vectors. limit-hint is ignored (rows are materialized)."
  ([sub env] ((:rows-fn sub) env))
  ([sub env _hint] ((:rows-fn sub) env)))

;; ====================================================================== indexes helpers

(defn index-key [tbl idx row]
  (mapv (fn [ic] (if (:col ic) (nth row (:col ic)) ((:kf ic) row))) (:cols idx)))

(defn index-order-rows
  "Rows of the index's table that are in the index, in index order."
  [db idx]
  (let [tbl (get-in db [:tables (:table idx)])
        rows (table-rows tbl)
        rows (if (:wf idx) (filter (:wf idx) rows) rows)
        cmps (mapv (fn [ic] [(:coll ic) (:desc ic) nil]) (:cols idx))
        cmp (compare-keys-fn (conj cmps ["BINARY" false nil]))
        keyed (to-array (map (fn [r] [(conj (index-key tbl idx r) (peek r)) r]) rows))]
    (java.util.Arrays/sort keyed (reify Comparator (compare [_ a b] (.compare ^Comparator cmp (first a) (first b)))))
    (map second keyed)))

(defn- expr-equal? [a b]
  (let [strip (fn strip [e] (cond (map? e) (into {} (for [[k x] e :when (not (#{:ps :pe :paren} k))] [k (strip x)]))
                                  (vector? e) (mapv strip e)
                                  :else e))
        norm (fn norm [e] (cond (map? e) (cond-> e (:name e) (update :name lc) (:table e) (update :table lc))
                                :else e))]
    (= (clojure.walk/postwalk norm (strip a)) (clojure.walk/postwalk norm (strip b)))))

(defn check-indexed-by
  "True if the WHERE terms imply the partial index's WHERE clause (approximation of sqlite3ExprImpliesExpr)."
  [ctx idx where-terms src]
  (let [iw (:where-ast idx)
        tbl (:table src)
        always-true? (fn [t]
                       (let [x (cond (= :notnull (:op t)) (:e t)
                                     (and (= :is (:op t)) (:not t) (= :lit (:op (:r t))) (nil? (:kind (:r t))) (nil? (:v (:r t)))) (:l t)
                                     :else nil)]
                         (when (and x (= :col (:op x)) (or (nil? (:table x)) (= (lc (:table x)) (:alias src))))
                           (let [ln (lc (:name x))
                                 i (.indexOf ^java.util.List (:lcols src) ln)]
                             (or (and (>= i 0) (or (= i (:ipk tbl)) (:notnull (nth (:cols tbl) i))))
                                 (and (neg? i) (#{"rowid" "oid" "_rowid_"} ln)))))))
        terms (remove always-true? (or where-terms []))
        strip-q (fn sq [e] (cond (and (map? e) (= :col (:op e))) (dissoc e :table)
                                 (map? e) (into {} (for [[k x] e] [k (sq x)]))
                                 (vector? e) (mapv sq e)
                                 :else e))
        iws (split-and iw)]
    (every? (fn [w]
              (some (fn [t]
                      (or (expr-equal? (strip-q t) (strip-q w))
                          ;; x IS NOT NULL is implied by a comparison on x with a non-null operand
                          (and (= :notnull (:op w))
                               (let [x (strip-q (:e w))
                                     t (strip-q t)]
                                 (or (and (= :bin (:op t)) (#{:eq :lt :le :gt :ge :ne} (:o t))
                                          (or (expr-equal? (:l t) x) (expr-equal? (:r t) x)))
                                     (and (= :is (:op t)) (:not t) (expr-equal? (:l t) x)
                                          (= :lit (:op (:r t))) (nil? (:kind (:r t))) (nil? (:v (:r t))))
                                     (and (= :between (:op t)) (expr-equal? (:e t) x))
                                     (and (= :in (:op t)) (expr-equal? (:e t) x))
                                     (and (= :like (:op t)) (expr-equal? (:e t) x)))))))
                    terms))
            iws)))

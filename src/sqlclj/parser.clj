(ns sqlclj.parser
  "Recursive-descent SQL parser producing a map-based AST. Every expression node carries :s/:e
  (source offsets) so that result column names can use the text as written."
  (:require [clojure.string :as str]
            [sqlclj.lexer :as lx]))

(set! *warn-on-reflection* true)

(def reserved
  #{"ADD" "ALL" "ALTER" "AND" "AS" "AUTOINCREMENT" "BETWEEN" "CASE" "CHECK" "COLLATE" "COMMIT"
    "CONSTRAINT" "CREATE" "DEFAULT" "DEFERRABLE" "DELETE" "DISTINCT" "DROP" "ELSE" "ESCAPE"
    "EXCEPT" "EXISTS" "FOREIGN" "FROM" "GROUP" "HAVING" "IN" "INDEX"
    "INSERT" "INTERSECT" "INTO" "IS" "ISNULL" "JOIN" "LIMIT" "NOT" "NOTNULL" "NULL"
    "ON" "OR" "ORDER" "PRIMARY" "REFERENCES" "SELECT" "SET" "TABLE" "THEN" "TO"
    "TRANSACTION" "UNION" "UNIQUE" "UPDATE" "USING" "VALUES" "WHEN" "WHERE" "RETURNING" "NOTHING"})

(def join-kw #{"CROSS" "FULL" "INNER" "LEFT" "NATURAL" "OUTER" "RIGHT"})

;; ------------------------------------------------------------------ parser state

(deftype P [^String sql toks ^ints ip])

(defn- pos ^long [^P p] (aget ^ints (.-ip p) 0))
(defn- set-pos! [^P p v] (aset ^ints (.-ip p) 0 (int v)))
(defn- tok [^P p] (nth (.-toks p) (aget ^ints (.-ip p) 0)))
(defn- tok-at [^P p k] (nth (.-toks p) (min (+ (aget ^ints (.-ip p) 0) (long k)) (dec (count (.-toks p))))))
(defn- advance! [^P p] (let [t (tok p)] (aset ^ints (.-ip p) 0 (inc (aget ^ints (.-ip p) 0))) t))

(defn- tok-text [^P p t]
  (subs (.-sql p) (:s t) (:e t)))

(defn- syntax-error [^P p t]
  (case (:t t)
    :eof (throw (lx/sql-error "incomplete input"))
    :illegal (throw (lx/sql-error (str "unrecognized token: \"" (tok-text p t) "\"")))
    (throw (lx/sql-error (str "near \"" (tok-text p t) "\": syntax error")))))

(defn- fail! [^P p] (syntax-error p (tok p)))

(defn- check-illegal [^P p]
  (let [t (tok p)] (when (= :illegal (:t t)) (syntax-error p t))))

(defn- kw? [t k] (and (= :id (:t t)) (= (:u t) k)))
(defn- at-kw? [^P p k] (kw? (tok p) k))
(defn- at-op? [^P p o] (let [t (tok p)] (and (= :op (:t t)) (= (:v t) o))))

(defn- accept-kw [^P p k] (when (at-kw? p k) (advance! p)))
(defn- accept-op [^P p o] (when (at-op? p o) (advance! p)))
(defn- expect-kw [^P p k] (or (accept-kw p k) (fail! p)))
(defn- expect-op [^P p o] (or (accept-op p o) (fail! p)))

(defn- ident-tok? [t]
  (or (= :qid (:t t))
      (and (= :id (:t t)) (not (reserved (:u t))))))

(defn- parse-nm
  "A name: identifier, quoted identifier, or string literal."
  [^P p]
  (check-illegal p)
  (let [t (tok p)]
    (cond (ident-tok? t) (do (advance! p) (:v t))
          (= :str (:t t)) (do (advance! p) (:v t))
          :else (fail! p))))

(defn- parse-id
  "An identifier only (no string)."
  [^P p]
  (check-illegal p)
  (let [t (tok p)]
    (if (ident-tok? t) (do (advance! p) (:v t)) (fail! p))))

;; ------------------------------------------------------------------ expressions

(declare parse-expr parse-select parse-expr-prec parse-window-def)
(def ^:dynamic *in-trigger* false)
(def ^:dynamic *trigger-cmd* false)
(def ^:dynamic *trigger-temp* false)

(defn- node [m s e] (assoc m :ps s :pe e))

(defn- name-raw [^P p start]
  (some (fn [t] (when (= (:s t) start) (subs (.-sql p) (:s t) (:e t)))) (.-toks p)))

(defn- prev-end [^P p] (:e (nth (.-toks p) (dec (pos p)))))

(defn- parse-type-name
  "Parses a type name (sequence of identifiers, optional (n[,m])). Returns string or nil."
  [^P p]
  (let [start (pos p)
        names (loop [acc []]
                (let [t (tok p)]
                  (if (and (or (= :id (:t t)) (= :qid (:t t)))
                           (or (= :qid (:t t)) (not (or (reserved (:u t))
                                                    (and (= "GENERATED" (:u t)) (kw? (tok-at p 1) "ALWAYS") (kw? (tok-at p 2) "AS"))))))
                    (do (advance! p) (recur (conj acc (:v t))))
                    acc)))]
    (when (seq names)
      (let [tn (str/join " " names)]
        (if (at-op? p "(")
          (let [s0 (:s (tok p))]
            (advance! p)
            (letfn [(signed-num []
                      (accept-op p "+") (accept-op p "-")
                      (let [t (tok p)]
                        (if (#{:int :float :hex} (:t t)) (advance! p) (fail! p))))]
              (signed-num)
              (when (accept-op p ",") (signed-num))
              (expect-op p ")")
              (str tn (subs (.-sql p) s0 (prev-end p)))))
          tn)))))

(defn- parse-function-args [^P p]
  ;; after '('
  (cond
    (accept-op p ")") {:args []}
    (at-op? p "*") (do (advance! p) (expect-op p ")") {:args [] :star true})
    :else
    (let [distinct (boolean (accept-kw p "DISTINCT"))
          _ (accept-kw p "ALL")
          args (if (at-op? p ")")
                 []
                 (loop [acc [(parse-expr p)]]
                   (if (accept-op p ",") (recur (conj acc (parse-expr p))) acc)))
          order (when (at-kw? p "ORDER")
                  (advance! p) (expect-kw p "BY")
                  (loop [acc []]
                    (let [e (parse-expr p)
                          d (cond (accept-kw p "DESC") true (accept-kw p "ASC") false :else false)
                          acc (conj acc {:e e :desc d})]
                      (if (accept-op p ",") (recur acc) acc))))]
      (expect-op p ")")
      {:args args :distinct distinct :order order})))

(defn- parse-primary [^P p]
  (check-illegal p)
  (let [t (tok p) s (:s t)]
    (case (:t t)
      :int (do (advance! p) (node {:op :lit :kind :int :text (:v t)} s (:e t)))
      :float (do (advance! p) (node {:op :lit :kind :float :text (:v t)} s (:e t)))
      :hex (do (advance! p) (node {:op :lit :kind :hex :text (:v t)} s (:e t)))
      :str (do (advance! p) (node {:op :lit :v (:v t)} s (:e t)))
      :blob (do (advance! p) (node {:op :lit :kind :blob :text (:v t)} s (:e t)))
      :var (do (when *in-trigger* (throw (lx/sql-error "trigger cannot use variables")))
               (advance! p)
               (let [vv ^String (:v t)]
                 (when (and (> (.length vv) 1) (= \? (.charAt vv 0)))
                   (let [x (try (Long/parseLong (subs vv 1)) (catch Exception _ -1))]
                     (when-not (and (> x 0) (<= x 32766))
                       (throw (lx/sql-error "variable number must be between ?1 and ?32766"))))))
               (node {:op :lit :v nil :var true} s (:e t)))
      :op
      (cond
        (= "(" (:v t))
        (do (advance! p)
            (if (or (at-kw? p "SELECT") (at-kw? p "VALUES") (at-kw? p "WITH"))
              (let [sel (parse-select p)]
                (expect-op p ")")
                (node {:op :subq :select sel} s (prev-end p)))
              (let [e (parse-expr p)]
                (if (at-op? p ",")
                  (let [es (loop [acc [e]] (if (accept-op p ",") (recur (conj acc (parse-expr p))) acc))]
                    (expect-op p ")")
                    (node {:op :vector :items es} s (prev-end p)))
                  (do (expect-op p ")")
                      (node (assoc e :paren true) s (prev-end p)))))))
        :else (fail! p))
      :qid
      (do (advance! p)
          (cond
            (and (at-op? p "(") (not (:dq-only t)))
            (do (advance! p)
                (let [fa (parse-function-args p)
                      filt (when (at-kw? p "FILTER")
                             (advance! p) (expect-op p "(") (expect-kw p "WHERE")
                             (let [e (parse-expr p)] (expect-op p ")") e))
                      over (when (accept-kw p "OVER")
                             (if (accept-op p "(")
                               (let [w (parse-window-def p)] (expect-op p ")") w)
                               (parse-nm p)))]
                  (node (cond-> (merge {:op :fn :name (str/lower-case (:v t)) :rawname (:v t)} fa)
                          filt (assoc :filter filt)
                          over (assoc :over over))
                        s (prev-end p))))
            :else
          (if (at-op? p ".")
            (do (advance! p)
                (let [n2 (parse-nm p)]
                  (if (at-op? p ".")
                    (do (advance! p) (let [n3 (parse-nm p)] (node {:op :col :schema (:v t) :table n2 :name n3} s (prev-end p))))
                    (node {:op :col :table (:v t) :name n2} s (prev-end p)))))
            (node {:op :col :name (:v t) :dq (:dq t) :quoted true} s (:e t)))))
      :id
      (let [u (:u t)]
        (cond
          (= u "NULL") (do (advance! p) (node {:op :lit :v nil} s (:e t)))
          (and (= u "RAISE") (let [t2 (tok-at p 1)] (not (and (= :op (:t t2)) (= "(" (:v t2)))))) (do (advance! p) (fail! p))
          (and (#{"CURRENT_TIME" "CURRENT_DATE" "CURRENT_TIMESTAMP"} u))
          (do (advance! p) (node {:op :fn :name (str/lower-case u) :args []} s (:e t)))
          (= u "CASE")
          (do (advance! p)
              (let [base (when-not (at-kw? p "WHEN") (parse-expr p))
                    whens (loop [acc []]
                            (if (accept-kw p "WHEN")
                              (let [w (parse-expr p)
                                    _ (expect-kw p "THEN")
                                    th (parse-expr p)]
                                (recur (conj acc [w th])))
                              acc))
                    _ (when (empty? whens) (fail! p))
                    els (when (accept-kw p "ELSE") (parse-expr p))]
                (expect-kw p "END")
                (node {:op :case :base base :whens whens :else els} s (prev-end p))))
          (= u "CAST")
          (do (advance! p)
              (expect-op p "(")
              (let [e (parse-expr p)
                    _ (expect-kw p "AS")
                    tn (or (parse-type-name p) "")]
                (expect-op p ")")
                (node {:op :cast :e e :type tn} s (prev-end p))))
          (= u "EXISTS")
          (do (advance! p)
              (expect-op p "(")
              (let [sel (parse-select p)]
                (expect-op p ")")
                (node {:op :exists :select sel} s (prev-end p))))
          (and (= u "RAISE") (let [n (tok-at p 1)] (and (= :op (:t n)) (= "(" (:v n)))))
          (do (advance! p) (advance! p)
              (let [k (let [t (advance! p)]
                        (case (:u t) "IGNORE" :ignore "ROLLBACK" :rollback "ABORT" :abort "FAIL" :fail (syntax-error p t)))
                    msg (when (not= k :ignore)
                          (expect-op p ",")
                          (let [t (tok p) t1 (tok-at p 1)]
                            (if (and (= :str (:t t)) (= :op (:t t1)) (= ")" (:v t1)))
                              (do (advance! p) (:v t))
                              (parse-expr p))))]
                (expect-op p ")")
                (node (cond-> {:op :raise :kind k :msg (if (string? msg) msg nil) :msg-e (when-not (string? msg) msg)}
                        (not *in-trigger*) (assoc :outside-trigger true)) s (prev-end p))))
          (reserved u) (fail! p)
          :else
          (do (advance! p)
              (cond
                (at-op? p "(")
                (do (advance! p)
                    (let [fa (parse-function-args p)
                          filt (when (at-kw? p "FILTER")
                                 (advance! p) (expect-op p "(") (expect-kw p "WHERE")
                                 (let [e (parse-expr p)] (expect-op p ")") e))
                          over (when (accept-kw p "OVER")
                                 (if (accept-op p "(")
                                   (let [w (parse-window-def p)] (expect-op p ")") w)
                                   (parse-nm p)))]
                      (node (cond-> (merge {:op :fn :name (str/lower-case (:v t)) :rawname (:v t)} fa)
                              filt (assoc :filter filt)
                              over (assoc :over over))
                            s (prev-end p))))
                (at-op? p ".")
                (do (advance! p)
                    (let [n2 (parse-nm p)]
                      (if (at-op? p ".")
                        (do (advance! p) (let [n3 (parse-nm p)] (node {:op :col :schema (:v t) :table n2 :name n3} s (prev-end p))))
                        (node {:op :col :table (:v t) :name n2} s (prev-end p)))))
                :else (node {:op :col :name (:v t)} s (:e t))))))
      (fail! p))))

(declare parse-order-by)

(defn- parse-frame-bound [^P p start?]
  (cond
    (at-kw? p "UNBOUNDED")
    (do (advance! p)
        (cond (and start? (accept-kw p "PRECEDING")) {:kind :unbounded-preceding}
              (and (not start?) (accept-kw p "FOLLOWING")) {:kind :unbounded-following}
              :else (fail! p)))
    (at-kw? p "CURRENT") (do (advance! p) (expect-kw p "ROW") {:kind :current})
    :else (let [e (parse-expr p)]
            (cond (accept-kw p "PRECEDING") {:kind :preceding :e e}
                  (accept-kw p "FOLLOWING") {:kind :following :e e}
                  :else (fail! p)))))

(defn- parse-window-def
  "Inside OVER ( ... ) or WINDOW w AS ( ... )."
  [^P p]
  (let [base (when (and (ident-tok? (tok p))
                        (not (#{"PARTITION" "ORDER" "RANGE" "ROWS" "GROUPS"} (:u (tok p)))))
               (parse-nm p))
        part (when (at-kw? p "PARTITION")
               (advance! p) (expect-kw p "BY")
               (loop [acc [(parse-expr p)]] (if (accept-op p ",") (recur (conj acc (parse-expr p))) acc)))
        order (when (at-kw? p "ORDER") (advance! p) (expect-kw p "BY") (parse-order-by p))
        frame (when (and (= :id (:t (tok p))) (#{"RANGE" "ROWS" "GROUPS"} (:u (tok p))))
                (let [ft (keyword (str/lower-case (:u (advance! p))))
                      [st en] (if (accept-kw p "BETWEEN")
                                (let [a (parse-frame-bound p true)
                                      _ (expect-kw p "AND")
                                      b (parse-frame-bound p false)]
                                  [a b])
                                [(parse-frame-bound p true) {:kind :current}])
                      excl (when (accept-kw p "EXCLUDE")
                             (cond (accept-kw p "NO") (do (expect-kw p "OTHERS") :no-others)
                                   (accept-kw p "CURRENT") (do (expect-kw p "ROW") :current-row)
                                   (accept-kw p "GROUP") :group
                                   (accept-kw p "TIES") :ties
                                   :else (fail! p)))
                      rank {:unbounded-preceding 0 :preceding 1 :current 2 :following 3 :unbounded-following 4}]
                  (when (or (and (= :current (:kind st)) (= :preceding (:kind en)))
                            (and (= :following (:kind st)) (#{:preceding :current} (:kind en))))
                    (throw (lx/sql-error "unsupported frame specification")))
                  {:type ft :start st :end en :exclude excl}))]
    {:base base :part part :order order :frame frame}))

(defn- parse-unary [^P p]
  (check-illegal p)
  (let [t (tok p) s (:s t)]
    (cond
      (and (= :op (:t t)) (#{"-" "+" "~"} (:v t)))
      (do (advance! p)
          (let [e (parse-expr-prec p 12)]
            (node {:op :unary :o (case (:v t) "-" :neg "+" :pos "~" :bitnot) :e e} s (:pe e))))
      :else (parse-primary p))))

(def ^:private binops
  {"OR" [1 :or] "AND" [2 :and]
   "=" [4 :eq] "==" [4 :eq] "!=" [4 :ne] "<>" [4 :ne]
   "<" [5 :lt] "<=" [5 :le] ">" [5 :gt] ">=" [5 :ge]
   "&" [7 :bitand] "|" [7 :bitor] "<<" [7 :shl] ">>" [7 :shr]
   "+" [8 :add] "-" [8 :sub] "*" [9 :mul] "/" [9 :div] "%" [9 :rem]
   "||" [10 :concat] "->" [10 :ptr] "->>" [10 :ptr2]})

(defn- parse-in-rhs [^P p lhs s negate]
  (cond
    (at-op? p "(")
    (do (advance! p)
        (cond
          (accept-op p ")") (node {:op :in :e lhs :list [] :not negate} s (prev-end p))
          (or (at-kw? p "SELECT") (at-kw? p "VALUES") (at-kw? p "WITH"))
          (let [sel (parse-select p)]
            (expect-op p ")")
            (node {:op :in :e lhs :select sel :not negate} s (prev-end p)))
          :else
          (let [items (loop [acc [(parse-expr p)]] (if (accept-op p ",") (recur (conj acc (parse-expr p))) acc))]
            (expect-op p ")")
            (node {:op :in :e lhs :list items :not negate} s (prev-end p)))))
    :else
    (let [n1 (parse-nm p)
          [sch nm] (if (accept-op p ".") [n1 (parse-nm p)] [nil n1])]
      (when (at-op? p "(") (fail! p))
      (node {:op :in :e lhs :table nm :not negate} s (prev-end p)))))


;; ------------------------------------------------------------------ parse-time folding (as SQLite's parser)

(defn- foldable-literal? [e]
  (let [e (if (and (= :unary (:op e)) (= :neg (:o e))) (:e e) e)]
    (and (= :lit (:op e)) (or (#{:int :float :hex :blob} (:kind e)) (string? (:v e))))))

(defn- fold-null-test
  "<numeric or blob literal> IS [NOT] NULL is folded to the integer 0/1."
  [n operand isnull]
  (if (foldable-literal? operand)
    (assoc n :op :lit :kind :int :text (if isnull "0" "1"))
    n))

(defn- false-literal? [e]
  (and (= :lit (:op e)) (#{:int :hex} (:kind e))
       (let [t (:text e)]
         (try (zero? (if (= :hex (:kind e)) (Long/parseUnsignedLong (subs t 2) 16) (Long/parseLong t)))
              (catch Exception _ false)))))

(defn- has-function? [e]
  (cond (map? e) (or (= :fn (:op e)) (some has-function? (vals (dissoc e :select))))
        (sequential? e) (some has-function? e)
        :else false))

(defn- fold-and
  "X AND Y is replaced by 0 when either side is the integer literal 0 and neither contains a function."
  [n]
  (if (and (= :and (:o n))
           (or (false-literal? (:l n)) (false-literal? (:r n)))
           (not (has-function? (:l n))) (not (has-function? (:r n))))
    {:op :lit :kind :int :text "0" :ps (:ps n) :pe (:pe n)}
    n))

(defn parse-expr-prec [^P p minp]
  (let [minp (long minp)
        left (if (at-kw? p "NOT")
               (let [s (:s (tok p))]
                 (advance! p)
                 (let [e (parse-expr-prec p 4)]
                   (node {:op :not :e e} s (:pe e))))
               (parse-unary p))]
    (loop [left left]
      (check-illegal p)
      (let [t (tok p) s (:ps left)]
        (cond
          ;; COLLATE (level 11)
          (and (kw? t "COLLATE") (<= minp 11))
          (do (advance! p)
              (let [c (parse-nm p)]
                (recur (node {:op :collate :e left :coll c} s (prev-end p)))))
          (and (<= minp 4) (= :id (:t t)) (= "NOT" (:u t))
               (let [t2 (tok-at p 1)]
                 (not (and (= :id (:t t2)) (#{"IN" "LIKE" "GLOB" "BETWEEN" "NULL" "MATCH" "REGEXP"} (:u t2))))))
          (do (advance! p) (fail! p))
          ;; level 4 specials
          (and (<= minp 4) (= :id (:t t))
               (or (#{"IS" "IN" "LIKE" "GLOB" "BETWEEN" "ISNULL" "NOTNULL" "MATCH" "REGEXP"} (:u t))
                   (and (= "NOT" (:u t))
                        (let [t2 (tok-at p 1)]
                          (and (= :id (:t t2)) (#{"IN" "LIKE" "GLOB" "BETWEEN" "NULL" "MATCH" "REGEXP"} (:u t2)))))))
          (let [negate (= "NOT" (:u t))
                _ (when negate (advance! p))
                t (advance! p)
                u (:u t)]
            (case u
              "ISNULL" (recur (fold-null-test (node {:op :isnull :e left} s (prev-end p)) left true))
              "NOTNULL" (recur (fold-null-test (node {:op :notnull :e left} s (prev-end p)) left false))
              "NULL" (recur (fold-null-test (node {:op :notnull :e left} s (prev-end p)) left false))
              "IS" (let [n2 (boolean (accept-kw p "NOT"))
                         dist (when (accept-kw p "DISTINCT") (expect-kw p "FROM") true)
                         r (parse-expr-prec p 5)
                         isnot (if dist (not n2) n2)]
                     (recur (let [n (node {:op :is :not isnot :l left :r r} s (:pe r))]
                              (cond
                                (and (= :lit (:op r)) (nil? (:kind r)) (nil? (:v r)) (not dist))
                                (fold-null-test n left (not isnot))
                                (and (= :col (:op r)) (nil? (:table r)) (not (:quoted r)) (#{"true" "false"} (str/lower-case (str (:name r)))))
                                ;; x IS [NOT] TRUE/FALSE tests truthiness: mode bit 1 = FALSE, bit 2 = NOT
                                (node {:op :fn :name "\u0000truth"
                                       :args [left {:op :lit :kind :int :text (str (+ (if (= "false" (str/lower-case (:name r))) 1 0) (if isnot 2 0)))}]}
                                      s (:pe r))
                                :else n))))
              "IN" (recur (parse-in-rhs p left s negate))
              ("LIKE" "GLOB" "MATCH" "REGEXP")
              (let [r (parse-expr-prec p 5)
                    esc (when (accept-kw p "ESCAPE") (parse-expr-prec p 5))]
                (recur (node {:op :like :o (keyword (str/lower-case u)) :ostr (:v t) :e left :pat r :esc esc :not negate}
                             s (prev-end p))))
              "BETWEEN"
              (let [lo (loop [lo (parse-expr-prec p 3)]
                         ;; an OR inside the lower bound is shifted by SQLite's parser; its right side may take ANDs
                         (if (at-kw? p "OR")
                           (do (advance! p)
                               (let [r (parse-expr-prec p 2)]
                                 (recur (node {:op :bin :o :or :l lo :r r} (:ps lo) (:pe r)))))
                           lo))
                    _ (expect-kw p "AND")
                    hi (parse-expr-prec p 5)]
                (recur (node {:op :between :e left :lo lo :hi hi :not negate} s (:pe hi))))))
          :else
          (let [k (cond (= :op (:t t)) (:v t)
                        (= :id (:t t)) (:u t))
                b (get binops k)]
            (if (and b (>= (long (first b)) minp))
              (let [[prec o] b]
                (advance! p)
                (let [r (parse-expr-prec p (inc (long prec)))]
                  (recur (fold-and (node {:op :bin :o o :l left :r r} s (:pe r))))))
              left)))))))

(defn parse-expr [^P p] (parse-expr-prec p 1))

;; ------------------------------------------------------------------ SELECT

(defn- parse-alias
  ([^P p] (parse-alias p true))
  ([^P p table?]
   (if (accept-kw p "AS")
     (parse-nm p)
     (let [t (tok p)]
       (when (or (and (ident-tok? t) (not (join-kw (:u t))) (not (and table? (#{"WINDOW" "INDEXED" "OVER" "FILTER"} (:u t)))))
                 (= :str (:t t)))
         (advance! p) (:v t))))))

(defn- parse-result-col [^P p]
  (let [t (tok p) s (:s t)]
    (cond
      (at-op? p "*") (do (advance! p) {:star true})
      (and (or (ident-tok? t) (= :str (:t t))) (= "." (:v (tok-at p 1))) (= "*" (:v (tok-at p 2))))
      (do (advance! p) (advance! p) (advance! p) {:star true :table (:v t)})
      :else
      (let [e (parse-expr p)
            alias (parse-alias p false)
            ;; SQLite's column name span runs to the start of the next token, so comments after the
            ;; expression belong to it; trailing white space is trimmed
            npe (when-not alias
                  (let [^String sql (.-sql p) ns (:s (tok p))]
                    (when (and ns (> (long ns) (long (:pe e))))
                      (loop [k (long ns)]
                        (if (and (> k (long (:pe e))) (#{\space \tab \newline \return \formfeed} (.charAt sql (dec k))))
                          (recur (dec k))
                          (when (> k (long (:pe e))) k))))))]
        (cond-> {:e e :alias alias} npe (assoc :name-pe npe))))))

(declare parse-from)

(defn- parse-table-ref [^P p]
  (cond
    (at-op? p "(")
    (do (advance! p)
        (if (or (at-kw? p "SELECT") (at-kw? p "VALUES") (at-kw? p "WITH"))
          (let [sel (parse-select p)]
            (expect-op p ")")
            {:subq sel :alias (parse-alias p)})
          (let [f (parse-from p)]
            (expect-op p ")")
            {:nested f :alias (parse-alias p)})))
    :else
    (let [n1 (parse-nm p)
          [sch nm] (if (accept-op p ".") [n1 (parse-nm p)] [nil n1])]
      (if (at-op? p "(")
        (do (advance! p)
            (let [args (if (accept-op p ")") [] (let [a (loop [acc [(parse-expr p)]] (if (accept-op p ",") (recur (conj acc (parse-expr p))) acc))] (expect-op p ")") a))]
              {:tvf nm :args args :alias (parse-alias p)}))
        (let [alias (parse-alias p)
              ib (cond (at-kw? p "INDEXED") (do (advance! p) (expect-kw p "BY") (parse-nm p))
                       (and (at-kw? p "NOT") (kw? (tok-at p 1) "INDEXED")) (do (advance! p) (advance! p) :none)
                       (at-kw? p "NOT") (do (advance! p) (fail! p))
                       :else nil)]
          {:table nm :schema sch :alias alias :indexed-by ib})))))

(defn- parse-join-op
  "Returns nil if no join follows, else a join type keyword."
  [^P p]
  (cond
    (accept-op p ",") :cross-comma
    (at-kw? p "JOIN") (do (advance! p) :inner)
    (and (= :id (:t (tok p))) (join-kw (:u (tok p))))
    (let [t0 (advance! p)
          more (loop [acc []]
                 (let [t (tok p)]
                   (if (and (< (count acc) 2) (not (kw? t "JOIN"))
                            (or (ident-tok? t) (= :str (:t t))))
                     (do (advance! p) (recur (conj acc t)))
                     acc)))
          _ (expect-kw p "JOIN")
          all (cons t0 more)
          codes {"NATURAL" #{:nat} "LEFT" #{:left :outer} "OUTER" #{:outer} "RIGHT" #{:right :outer}
                 "FULL" #{:left :right :outer} "INNER" #{:inner} "CROSS" #{:inner :cross}}
          bits (reduce (fn [acc t] (let [c (and (= :id (:t t)) (codes (:u t)))]
                                     (if c (into acc c) (reduced (conj acc :error)))))
                       #{} all)
          _ (when (or (and (bits :inner) (bits :outer)) (bits :error)
                      (and (bits :outer) (not (bits :left)) (not (bits :right))))
              (throw (lx/sql-error (str "unknown join type: " (str/join " " (map #(tok-text p %) all))))))
          kws (map :u all)]
      (let [s (set kws)]
        (cond
          (s "NATURAL") (cond (s "LEFT") :natural-left (s "RIGHT") :natural-right (s "FULL") :natural-full :else :natural)
          (s "LEFT") :left
          (s "RIGHT") :right
          (s "FULL") :full
          (s "CROSS") :cross
          :else :inner)))
    :else nil))

(defn- valid-after-from? [^P p]
  (let [t (tok p)]
    (or (= :eof (:t t))
        (and (= :op (:t t)) (#{";" ")" ","} (:v t)))
        (and (= :id (:t t))
             (contains? #{"WHERE" "GROUP" "HAVING" "WINDOW" "ORDER" "LIMIT" "UNION" "INTERSECT" "EXCEPT" "JOIN"
                          "LEFT" "RIGHT" "FULL" "INNER" "CROSS" "NATURAL" "RETURNING" "OUTER"} (:u t))))))

(defn parse-from [^P p]
  (loop [items (let [first-ref (parse-table-ref p)]
                 (cond
                   (at-kw? p "ON")
                   (do (advance! p) (parse-expr p)
                       (when-not (valid-after-from? p) (fail! p))
                       (throw (lx/sql-error "a JOIN clause is required before ON")))
                   (at-kw? p "USING")
                   (do (advance! p) (expect-op p "(")
                       (loop [] (parse-nm p) (when (accept-op p ",") (recur)))
                       (expect-op p ")")
                       (when-not (valid-after-from? p) (fail! p))
                       (throw (lx/sql-error "a JOIN clause is required before USING")))
                   ;; SQLite's parser flattens an unaliased parenthesized join in the first position
                   (and (:nested first-ref) (nil? (:alias first-ref))) (vec (:nested first-ref))
                   :else [first-ref]))]
    (let [save (pos p)
          jt (parse-join-op p)]
      (if (nil? jt)
        ;; a leading parenthesized join without alias is simply spliced into the FROM list (as SQLite's parser does)
        (let [f (first items)]
          (if (and (:nested f) (nil? (:alias f)) (nil? (:on f)) (nil? (:using f)) (nil? (:join f)))
            (into (vec (:nested f)) (rest items))
            items))
        (let [ref (parse-table-ref p)
              ref (cond
                    (accept-kw p "ON") (assoc ref :on (parse-expr p))
                    (accept-kw p "USING")
                    (do (expect-op p "(")
                        (let [cols (loop [acc [(parse-nm p)]] (if (accept-op p ",") (recur (conj acc (parse-nm p))) acc))]
                          (expect-op p ")")
                          (assoc ref :using cols)))
                    :else ref)]
          (recur (conj items (assoc ref :join jt))))))))

(defn- parse-order-by [^P p]
  (loop [acc []]
    (let [e (parse-expr p)
          desc (cond (accept-kw p "DESC") true (accept-kw p "ASC") false :else false)
          nulls (when (accept-kw p "NULLS")
                  (cond (accept-kw p "FIRST") :first (accept-kw p "LAST") :last :else (fail! p)))
          acc (conj acc {:e e :desc desc :nulls nulls})]
      (if (accept-op p ",") (recur acc) acc))))

(defn- parse-values [^P p]
  ;; after VALUES
  (loop [rows []]
    (expect-op p "(")
    (let [row (loop [acc [(parse-expr p)]] (if (accept-op p ",") (recur (conj acc (parse-expr p))) acc))]
      (expect-op p ")")
      (let [rows (conj rows row)]
        (if (accept-op p ",") (recur rows) rows)))))

(defn- parse-select-core [^P p]
  (cond
    (accept-kw p "VALUES")
    {:type :values :rows (parse-values p)}
    :else
    (do
      (expect-kw p "SELECT")
      (let [distinct (cond (accept-kw p "DISTINCT") true (accept-kw p "ALL") false :else false)
            cols (loop [acc [(parse-result-col p)]] (if (accept-op p ",") (recur (conj acc (parse-result-col p))) acc))
            from (when (accept-kw p "FROM") (parse-from p))
            where (when (accept-kw p "WHERE") (parse-expr p))
            group (when (at-kw? p "GROUP")
                    (advance! p) (expect-kw p "BY")
                    (loop [acc [(parse-expr p)]] (if (accept-op p ",") (recur (conj acc (parse-expr p))) acc)))
            having (when (accept-kw p "HAVING") (parse-expr p))]
        (let [windows (when (accept-kw p "WINDOW")
                        (loop [acc []]
                          (let [n (parse-nm p)
                                _ (expect-kw p "AS")
                                _ (expect-op p "(")
                                w (parse-window-def p)
                                _ (expect-op p ")")
                                acc (conj acc [n w])]
                            (if (accept-op p ",") (recur acc) acc))))]
          (cond-> {:type :core :distinct distinct :cols cols :from from :where where :group group :having having}
            windows (assoc :windows windows)))))))

(defn parse-with
  "After WITH: [RECURSIVE] cte, ... Returns {:recursive :ctes [{:name :cols :select}]}."
  [^P p]
  (let [rec (boolean (accept-kw p "RECURSIVE"))
        ctes (loop [acc []]
               (let [name (parse-nm p)
                     cols (when (accept-op p "(")
                            (let [c (loop [acc [(parse-nm p)]] (if (accept-op p ",") (recur (conj acc (parse-nm p))) acc))]
                              (expect-op p ")") c))
                     _ (expect-kw p "AS")
                     _ (cond (accept-kw p "MATERIALIZED") nil
                             (at-kw? p "NOT") (do (advance! p) (expect-kw p "MATERIALIZED")))
                     _ (expect-op p "(")
                     sel (parse-select p)
                     _ (expect-op p ")")
                     acc (conj acc {:name name :cols cols :select sel})]
                 (if (accept-op p ",") (recur acc) acc)))]
    {:recursive rec :ctes ctes}))

(defn parse-select [^P p]
  (let [with (when (at-kw? p "WITH") (advance! p) (parse-with p))
        first-core (parse-select-core p)
        body (loop [left first-core]
               (let [op (cond (at-kw? p "UNION") (do (advance! p) (if (accept-kw p "ALL") :union-all :union))
                              (at-kw? p "INTERSECT") (do (advance! p) :intersect)
                              (at-kw? p "EXCEPT") (do (advance! p) :except)
                              :else nil)]
                 (if op
                   (recur {:type :compound :op op :left left :right (parse-select-core p)})
                   left)))
        last-core (if (= :compound (:type body)) (:right body) body)
        _ (when (and (= :values (:type last-core)) (or (at-kw? p "ORDER") (at-kw? p "LIMIT"))) (fail! p))
        order (when (at-kw? p "ORDER") (advance! p) (expect-kw p "BY") (parse-order-by p))
        [limit offset] (when (accept-kw p "LIMIT")
                         (let [a (parse-expr p)]
                           (cond (accept-kw p "OFFSET") [a (parse-expr p)]
                                 (accept-op p ",") [(parse-expr p) a]
                                 :else [a nil])))
        _ (when (and (or order limit) (or (at-kw? p "UNION") (at-kw? p "INTERSECT") (at-kw? p "EXCEPT")))
            (let [op (cond (at-kw? p "INTERSECT") "INTERSECT" (at-kw? p "EXCEPT") "EXCEPT"
                           (kw? (tok-at p 1) "ALL") "UNION ALL" :else "UNION")]
              (throw (lx/sql-error (str (if order "ORDER BY" "LIMIT") " clause should come after " op " not before")))))]
    (cond-> {:body body :order order :limit limit :offset offset}
      with (assoc :with with))))

;; ------------------------------------------------------------------ DDL

(defn- parse-conflict-clause [^P p]
  (when (at-kw? p "ON")
    (advance! p) (expect-kw p "CONFLICT")
    (let [t (advance! p)]
      (case (:u t)
        "ROLLBACK" :rollback "ABORT" :abort "FAIL" :fail "IGNORE" :ignore "REPLACE" :replace
        (syntax-error p t)))))

(defn- parse-signed-literal [^P p]
  (let [s (:s (tok p))]
    (cond
      (or (at-op? p "-") (at-op? p "+"))
      (let [o (:v (advance! p))
            t (tok p)]
        (if (or (#{:int :float :hex :str :blob} (:t t)) (and (= :id (:t t)) (#{"NULL" "CURRENT_TIME" "CURRENT_DATE" "CURRENT_TIMESTAMP"} (:u t))))
          (let [e (parse-primary p)] (node {:op :unary :o (if (= o "-") :neg :pos) :e e} s (:pe e)))
          (fail! p)))
      (let [t (tok p)]
        (or (#{:int :float :hex :str :blob} (:t t)) (and (= :id (:t t)) (#{"NULL" "CURRENT_TIME" "CURRENT_DATE" "CURRENT_TIMESTAMP"} (:u t)))))
      (parse-primary p)
      :else (fail! p))))

(defn- parse-fk-tail
  "After REFERENCES: parent table, optional column list, actions. Returns a map."
  [^P p]
  (let [tbl (parse-nm p)
        to (when (accept-op p "(")
             (let [c (loop [acc [(parse-nm p)]] (if (accept-op p ",") (recur (conj acc (parse-nm p))) acc))]
               (expect-op p ")") c))
        acts (loop [m {}]
               (cond
                 (and (at-kw? p "ON") (#{"DELETE" "UPDATE"} (:u (tok-at p 1))))
                 (let [_ (advance! p)
                       which (if (= "DELETE" (:u (advance! p))) :on-delete :on-update)
                       act (cond (accept-kw p "SET") (if (accept-kw p "NULL") :set-null
                                                         (do (expect-kw p "DEFAULT") :set-default))
                                 (accept-kw p "CASCADE") :cascade
                                 (accept-kw p "RESTRICT") :restrict
                                 (accept-kw p "NO") (do (expect-kw p "ACTION") :no-action)
                                 :else (fail! p))]
                   (recur (assoc m which act)))
                 (accept-kw p "MATCH") (recur (assoc m :match (parse-nm p)))
                 :else m))
        deferred (when (or (at-kw? p "DEFERRABLE") (and (at-kw? p "NOT") (kw? (tok-at p 1) "DEFERRABLE")))
                   (let [neg (accept-kw p "NOT")]
                     (advance! p)
                     (let [init (when (accept-kw p "INITIALLY")
                                  (let [t (advance! p)] (:u t)))]
                       (and (not neg) (= init "DEFERRED")))))]
    (merge {:table tbl :to to :deferred (boolean deferred)} acts)))

(defn- parse-column-def [^P p]
  (let [cs (:s (tok p))
        name (parse-nm p)
        type (parse-type-name p)
        cons (loop [acc []]
               (let [cname (when (accept-kw p "CONSTRAINT")
                             (loop [nm (parse-nm p)]
                               (if (accept-kw p "CONSTRAINT") (recur (parse-nm p)) nm)))
                     t (tok p)]
                 (cond
                   (kw? t "PRIMARY")
                   (do (advance! p) (expect-kw p "KEY")
                       (let [dir (cond (accept-kw p "ASC") :asc (accept-kw p "DESC") :desc :else nil)
                             cc (parse-conflict-clause p)
                             auto (boolean (accept-kw p "AUTOINCREMENT"))]
                         (recur (conj acc {:c :pk :dir dir :conflict cc :auto auto}))))
                   (kw? t "NOT")
                   (do (advance! p) (expect-kw p "NULL")
                       (recur (conj acc {:c :notnull :conflict (parse-conflict-clause p)})))
                   (kw? t "NULL") (do (advance! p) (parse-conflict-clause p) (recur acc))
                   (kw? t "UNIQUE")
                   (do (advance! p) (recur (conj acc {:c :unique :conflict (parse-conflict-clause p)})))
                   (kw? t "CHECK")
                   (do (advance! p) (expect-op p "(")
                       (let [e (parse-expr p)] (expect-op p ")") (recur (conj acc {:c :check :e e :name cname}))))
                   (kw? t "DEFAULT")
                   (do (advance! p)
                       (let [e (cond
                                 (at-op? p "(") (do (advance! p) (let [e (parse-expr p)] (expect-op p ")") e))
                                 (and (:u (tok p)) (#{"CURRENT_TIME" "CURRENT_DATE" "CURRENT_TIMESTAMP"} (:u (tok p)))) (parse-primary p)
                                 (ident-tok? (tok p)) (let [t (advance! p)]
                                                        (if (and (= :id (:t t)) (#{"TRUE" "FALSE"} (:u t)))
                                                          (node {:op :lit :kind :int :text (if (= "TRUE" (:u t)) "1" "0")} (:s t) (:e t))
                                                          (node {:op :lit :v (:v t)} (:s t) (:e t))))
                                 :else (parse-signed-literal p))]
                         (recur (conj acc {:c :default :e e}))))
                   (kw? t "COLLATE")
                   (do (advance! p) (recur (conj acc {:c :collate :coll (parse-nm p)})))
                   (kw? t "REFERENCES")
                   (do (advance! p)
                       (recur (conj acc (assoc (parse-fk-tail p) :c :fk :from [name]))))
                   (or (kw? t "GENERATED") (kw? t "AS"))
                   (do (when (accept-kw p "GENERATED") (expect-kw p "ALWAYS"))
                       (expect-kw p "AS") (expect-op p "(")
                       (let [e (parse-expr p)]
                         (expect-op p ")")
                         (let [stored (cond (accept-kw p "STORED") true (accept-kw p "VIRTUAL") false :else false)]
                           (recur (conj acc {:c :generated :e e :stored stored})))))
                   :else (do (when cname (fail! p)) acc))))]
    {:name name :type type :cons cons :s cs :e (prev-end p)}))

(defn- parse-indexed-cols [^P p]
  (expect-op p "(")
  (let [cols (loop [acc []]
               (let [e (parse-expr p)
                     [e coll] (if (= :collate (:op e)) [(:e e) (:coll e)] [e nil])
                     e (if (and (= :lit (:op e)) (string? (:v e)) (nil? (:kind e))) (assoc e :op :col :name (:v e)) e)
                     desc (cond (accept-kw p "DESC") true (accept-kw p "ASC") false :else false)
                     acc (conj acc {:e e :coll coll :desc desc})]
                 (if (accept-op p ",") (recur acc) acc)))]
    (expect-op p ")")
    cols))

(defn- parse-create-table [^P p temp]
  (let [ine (when (at-kw? p "IF") (advance! p) (expect-kw p "NOT") (expect-kw p "EXISTS") true)
        name-s (volatile! (:s (tok p)))
        entered (volatile! false)
        n1 (parse-nm p)
        [sch name] (if (accept-op p ".") (do (vreset! name-s (:s (tok p))) [n1 (parse-nm p)]) [nil n1])]
    (try
    (when (and temp sch (not= "temp" (str/lower-case sch))) (throw (lx/sql-error "temporary table name must be unqualified")))
    (if (accept-kw p "AS")
      (do (vreset! entered true)
          {:kind :create-table :name name :schema sch :temp (boolean temp) :name-s @name-s :name-raw (name-raw p @name-s) :if-not-exists ine :as (parse-select p)})
      (do
        (expect-op p "(")
        (vreset! entered true)
        (let [[cols tcons]
              (loop [cols [] tcons []]
                (let [t0 (tok p)
                      cname (when (kw? t0 "CONSTRAINT")
                              (advance! p)
                              (loop [nm (parse-nm p)]
                                (if (accept-kw p "CONSTRAINT") (recur (parse-nm p)) nm)))
                      t (tok p)]
                  (cond
                    (and cname (empty? cols) (not (or (kw? t "PRIMARY") (kw? t "UNIQUE") (kw? t "CHECK") (kw? t "FOREIGN"))))
                    (syntax-error p t0)
                    (or (kw? t "PRIMARY") (kw? t "UNIQUE") (kw? t "CHECK") (kw? t "FOREIGN"))
                    (if (empty? cols) (syntax-error p t0)
                        (let [tc (case (:u t)
                                   "PRIMARY" (do (advance! p) (expect-kw p "KEY")
                                                 {:c :pk :cols (parse-indexed-cols p) :conflict (parse-conflict-clause p)})
                                   "UNIQUE" (do (advance! p) {:c :unique :cols (parse-indexed-cols p) :conflict (parse-conflict-clause p)})
                                   "CHECK" (do (advance! p) (expect-op p "(") (let [e (parse-expr p)] (expect-op p ")") {:c :check :e e :name cname}))
                                   "FOREIGN" (do (advance! p) (expect-kw p "KEY")
                                                 (let [from (mapv (fn [ic] (:name (:e ic))) (parse-indexed-cols p))]
                                                   (expect-kw p "REFERENCES")
                                                   (assoc (parse-fk-tail p) :c :fk :from from))))
                              tcons (conj tcons tc)]
                          (cond (accept-op p ",") (recur cols tcons)
                                (accept-op p ")") [cols tcons]
                                (let [u (:u (tok p))] (#{"CONSTRAINT" "PRIMARY" "UNIQUE" "CHECK" "FOREIGN"} u)) (recur cols tcons)
                                :else (fail! p))))
                    (seq tcons) (fail! p)
                    :else
                    (let [cd (parse-column-def p)
                          cols (conj cols cd)]
                      (cond (accept-op p ",") (recur cols tcons)
                            (accept-op p ")") [cols tcons]
                            :else (fail! p))))))
              opts (loop [opts #{} need false]
                     (cond
                       (accept-kw p "STRICT") (let [o (conj opts :strict)] (if (accept-op p ",") (recur o true) o))
                       (at-kw? p "WITHOUT") (do (advance! p)
                                                (let [t (advance! p)]
                                                  (when-not (= "ROWID" (:u t))
                                                    (if (or (ident-tok? t) (= :str (:t t)))
                                                      (throw (lx/sql-error (str "unknown table option: " (tok-text p t))))
                                                      (syntax-error p t))))
                                                (let [o (conj opts :without-rowid)] (if (accept-op p ",") (recur o true) o)))
                       need (fail! p)
                       (and (empty? opts) (accept-op p ",")) (recur opts true)
                       (and (or (ident-tok? (tok p)) (= :str (:t (tok p)))) (not (reserved (:u (tok p)))))
                       (throw (lx/sql-error (str "unknown table option: " (tok-text p (tok p)))))
                       :else opts))]
          {:kind :create-table :name name :schema sch :temp (boolean temp) :name-s @name-s :name-raw (name-raw p @name-s) :if-not-exists ine :cols cols :tcons tcons
           :strict (contains? opts :strict) :without-rowid (contains? opts :without-rowid)})))
    (catch clojure.lang.ExceptionInfo ex
      (if (and (:sql-error (ex-data ex)) (not ine) @entered)
        (throw (ex-info (ex-message ex) (assoc (ex-data ex) :pending-create {:name name :schema sch :raw (name-raw p @name-s)})))
        (throw ex))))))

;; ------------------------------------------------------------------ DML

(defn- parse-returning [^P p & [kind]]
  (when (and *in-trigger* (at-kw? p "RETURNING") (not= kind :insert))
    (fail! p))
  (when (accept-kw p "RETURNING")
    (loop [acc [(parse-result-col p)]] (if (accept-op p ",") (recur (conj acc (parse-result-col p))) acc))))

(defn- parse-upsert [^P p]
  (loop [acc []]
    (if (and (at-kw? p "ON") (kw? (tok-at p 1) "CONFLICT"))
      (do (advance! p) (advance! p)
          (let [target (when (at-op? p "(")
                         (let [cols (parse-indexed-cols p)
                               w (when (accept-kw p "WHERE") (parse-expr p))]
                           {:cols cols :where w}))
                _ (expect-kw p "DO")
                action (cond
                         (accept-kw p "NOTHING") {:do :nothing}
                         (accept-kw p "UPDATE")
                         (do (expect-kw p "SET")
                             (let [sets (loop [acc []]
                                          (let [cols (if (at-op? p "(")
                                                       (do (advance! p)
                                                           (let [c (loop [a [(parse-nm p)]] (if (accept-op p ",") (recur (conj a (parse-nm p))) a))]
                                                             (expect-op p ")") c))
                                                       [(parse-nm p)])
                                                _ (expect-op p "=")
                                                e (parse-expr p)
                                                acc (conj acc [cols e])]
                                            (if (accept-op p ",") (recur acc) acc)))
                                   w (when (accept-kw p "WHERE") (parse-expr p))]
                               {:do :update :sets sets :where w}))
                         :else (fail! p))]
            (if target
              (recur (conj acc (merge {:target target} action)))
              (conj acc (merge {:target target} action)))))
      acc)))

(defn- parse-insert [^P p or-action]
  (expect-kw p "INTO")
  (let [n1 (parse-nm p)
        [sch tname] (if (accept-op p ".") [n1 (parse-nm p)] [nil n1])
        _ (when (and *trigger-cmd* sch (not *trigger-temp*)) (throw (lx/sql-error "qualified table names are not allowed on INSERT, UPDATE, and DELETE statements within triggers")))
        alias (when (accept-kw p "AS") (parse-nm p))
        cols (when (at-op? p "(")
               (advance! p)
               (let [c (loop [acc [(parse-nm p)]] (if (accept-op p ",") (recur (conj acc (parse-nm p))) acc))]
                 (expect-op p ")") c))
        src (cond
              (and (at-kw? p "DEFAULT") (not *trigger-cmd*)) (do (advance! p) (expect-kw p "VALUES") {:default true})
              (at-kw? p "VALUES")
              (let [save (pos p)
                    _ (advance! p)
                    rows (parse-values p)]
                (if (or (at-kw? p "UNION") (at-kw? p "INTERSECT") (at-kw? p "EXCEPT") (at-kw? p "ORDER") (at-kw? p "LIMIT"))
                  (do (set-pos! p save) {:select (parse-select p)})
                  {:rows rows}))
              (or (at-kw? p "SELECT") (at-kw? p "WITH")) {:select (parse-select p)}
              :else (fail! p))
        upsert (parse-upsert p)
        ret (parse-returning p :insert)]
    (merge {:kind :insert :or or-action :table tname :schema sch :alias alias :cols cols :upsert upsert :returning ret} src)))

(defn- parse-qualified-table [^P p]
  (let [n1 (parse-nm p)
        [sch tname] (if (accept-op p ".") [n1 (parse-nm p)] [nil n1])
        _ (when (and *trigger-cmd* sch (not *trigger-temp*)) (throw (lx/sql-error "qualified table names are not allowed on INSERT, UPDATE, and DELETE statements within triggers")))
        alias (when (accept-kw p "AS") (parse-nm p))
        _ (when (and *trigger-cmd* (at-kw? p "INDEXED")) (throw (lx/sql-error "the INDEXED BY clause is not allowed on UPDATE or DELETE statements within triggers")))
        _ (when (and *trigger-cmd* (at-kw? p "NOT") (kw? (tok-at p 1) "INDEXED")) (throw (lx/sql-error "the NOT INDEXED clause is not allowed on UPDATE or DELETE statements within triggers")))
        ib (cond (at-kw? p "INDEXED") (do (advance! p) (expect-kw p "BY") (parse-nm p))
                 (and (at-kw? p "NOT") (kw? (tok-at p 1) "INDEXED")) (do (advance! p) (advance! p) :none)
                 :else nil)]
    {:table tname :schema sch :alias alias :indexed-by ib}))

(defn- parse-update [^P p or-action]
  (let [qt (parse-qualified-table p)
        _ (expect-kw p "SET")
        sets (loop [acc []]
               (let [cols (if (at-op? p "(")
                            (do (advance! p)
                                (let [c (loop [a [(parse-nm p)]] (if (accept-op p ",") (recur (conj a (parse-nm p))) a))]
                                  (expect-op p ")") c))
                            [(parse-nm p)])
                     _ (expect-op p "=")
                     e (parse-expr p)
                     acc (conj acc [cols e])]
                 (if (accept-op p ",") (recur acc) acc)))
        from (when (accept-kw p "FROM") (parse-from p))
        where (when (accept-kw p "WHERE") (parse-expr p))
        _ (when (and *trigger-cmd* (at-kw? p "RETURNING")) (fail! p))
        ret (parse-returning p)
        _ (when (and *trigger-cmd* (or (at-kw? p "ORDER") (at-kw? p "LIMIT"))) (fail! p))
        _ (when (at-kw? p "ORDER") (throw (lx/sql-error "syntax error near \"ORDER BY\"")))
        _ (when (at-kw? p "LIMIT") (throw (lx/sql-error "syntax error near \"LIMIT\"")))]
    (merge {:kind :update :or or-action :sets sets :from from :where where :returning ret} qt)))

(defn- parse-delete [^P p]
  (expect-kw p "FROM")
  (let [qt (parse-qualified-table p)
        where (when (accept-kw p "WHERE") (parse-expr p))
        _ (when (and *trigger-cmd* (at-kw? p "RETURNING")) (fail! p))
        ret (parse-returning p)
        _ (when (and *trigger-cmd* (or (at-kw? p "ORDER") (at-kw? p "LIMIT"))) (fail! p))
        _ (when (at-kw? p "ORDER") (throw (lx/sql-error "syntax error near \"ORDER BY\"")))
        _ (when (at-kw? p "LIMIT") (throw (lx/sql-error "syntax error near \"LIMIT\"")))]
    (merge {:kind :delete :where where :returning ret} qt)))

;; ------------------------------------------------------------------ statements

(defn- parse-statement* [^P p]
  (check-illegal p)
  (let [t (tok p)]
    (if (not= :id (:t t))
      (fail! p)
      (case (:u t)
        ("SELECT" "VALUES") {:kind :select :select (parse-select p)}
        "WITH" (do (advance! p)
                   (let [w (parse-with p)
                         t2 (tok p)]
                     (if (and (= :id (:t t2)) (#{"SELECT" "VALUES"} (:u t2)))
                       {:kind :select :select (assoc (parse-select p) :with w)}
                       (if (and (= :id (:t t2)) (#{"INSERT" "REPLACE" "UPDATE" "DELETE"} (:u t2)) (not *trigger-cmd*))
                         (assoc (parse-statement* p) :with w)
                         (fail! p)))))
        "INSERT" (do (advance! p)
                     (let [or-action (when (accept-kw p "OR")
                                       (let [t (advance! p)]
                                         (case (:u t)
                                           "ROLLBACK" :rollback "ABORT" :abort "FAIL" :fail "IGNORE" :ignore "REPLACE" :replace
                                           (syntax-error p t))))]
                       (parse-insert p or-action)))
        "REPLACE" (do (advance! p) (parse-insert p :replace))
        "UPDATE" (do (advance! p)
                     (let [or-action (when (accept-kw p "OR")
                                       (let [t (advance! p)]
                                         (case (:u t)
                                           "ROLLBACK" :rollback "ABORT" :abort "FAIL" :fail "IGNORE" :ignore "REPLACE" :replace
                                           (syntax-error p t))))]
                       (parse-update p or-action)))
        "DELETE" (do (advance! p) (parse-delete p))
        "CREATE"
        (do (advance! p)
            (let [temp (or (accept-kw p "TEMP") (accept-kw p "TEMPORARY"))
                  unique (accept-kw p "UNIQUE")]
              (cond
                (and (not unique) (accept-kw p "TABLE")) (parse-create-table p temp)
                (accept-kw p "INDEX")
                (let [ine (when (at-kw? p "IF") (advance! p) (expect-kw p "NOT") (expect-kw p "EXISTS") true)
                      name-s (volatile! (:s (tok p)))
                      n1 (parse-nm p)
                      [sch name] (if (accept-op p ".") (do (vreset! name-s (:s (tok p))) [n1 (parse-nm p)]) [nil n1])
                      _ (expect-kw p "ON")
                      tname (parse-nm p)
                      cols (parse-indexed-cols p)
                      where (when (accept-kw p "WHERE") (parse-expr p))]
                  {:kind :create-index :unique (boolean unique) :if-not-exists ine :name name :schema sch :name-s @name-s :table tname :cols cols :where where})
                (and (not unique) (accept-kw p "VIEW"))
                (let [ine (when (at-kw? p "IF") (advance! p) (expect-kw p "NOT") (expect-kw p "EXISTS") true)
                      name-s (volatile! (:s (tok p)))
                      n1 (parse-nm p)
                      [sch name] (if (accept-op p ".") (do (vreset! name-s (:s (tok p))) [n1 (parse-nm p)]) [nil n1])
                      _ (when (and temp sch (not= "temp" (str/lower-case sch))) (throw (lx/sql-error "temporary view name must be unqualified")))
                      cols (when (at-op? p "(") (advance! p)
                                 (let [c (loop [acc [(parse-nm p)]] (if (accept-op p ",") (recur (conj acc (parse-nm p))) acc))]
                                   (expect-op p ")") c))
                      _ (expect-kw p "AS")
                      sel (parse-select p)]
                  {:kind :create-view :name name :schema sch :temp (boolean temp) :name-s @name-s :name-raw (name-raw p @name-s) :if-not-exists ine :cols cols :select sel})
                (and (not unique) (accept-kw p "TRIGGER"))
                (let [ine (when (at-kw? p "IF") (advance! p) (expect-kw p "NOT") (expect-kw p "EXISTS") true)
                      name-s (volatile! (:s (tok p)))
                      n1 (parse-nm p)
                      [sch name] (if (accept-op p ".") (do (vreset! name-s (:s (tok p))) [n1 (parse-nm p)]) [nil n1])
                      _ (when (and temp sch (not= "temp" (str/lower-case sch))) (throw (lx/sql-error "temporary trigger may not have qualified name")))
                      timing (cond (accept-kw p "BEFORE") :before
                                   (accept-kw p "AFTER") :after
                                   (at-kw? p "INSTEAD") (do (advance! p) (expect-kw p "OF") :instead)
                                   :else :before)
                      [event cols] (cond (accept-kw p "DELETE") [:delete nil]
                                         (accept-kw p "INSERT") [:insert nil]
                                         (accept-kw p "UPDATE")
                                         (if (accept-kw p "OF")
                                           [:update (loop [acc [(parse-nm p)]] (if (accept-op p ",") (recur (conj acc (parse-nm p))) acc))]
                                           [:update nil])
                                         :else (fail! p))
                      _ (expect-kw p "ON")
                      t1 (parse-nm p)
                      [tsch tname] (if (accept-op p ".") [t1 (parse-nm p)] [nil t1])
                      _ (when (accept-kw p "FOR") (expect-kw p "EACH") (expect-kw p "ROW"))
                      wh (when (accept-kw p "WHEN") (binding [*in-trigger* true] (parse-expr p)))
                      _ (expect-kw p "BEGIN")
                      body (binding [*in-trigger* true *trigger-cmd* true *trigger-temp* (boolean (or temp (some-> sch str/lower-case (= "temp"))))]
                             (loop [acc []]
                               (cond
                                 (and (seq acc) (accept-kw p "END")) acc
                                 :else (let [t (tok p)
                                             st (case (:u t)
                                                  ("SELECT" "VALUES" "INSERT" "REPLACE" "UPDATE" "DELETE" "WITH") (parse-statement* p)
                                                  (fail! p))
                                             _ (when (and (#{:insert :update :delete} (:kind st)) (:schema st) (not temp) (not= "temp" (some-> sch str/lower-case)))
                                                 (throw (lx/sql-error "qualified table names are not allowed on INSERT, UPDATE, and DELETE statements within triggers")))
                                             _ (when (and (#{:update :delete} (:kind st)) (:indexed-by st))
                                                 (throw (lx/sql-error (if (= :none (:indexed-by st))
                                                                        "the NOT INDEXED clause is not allowed on UPDATE or DELETE statements within triggers"
                                                                        "the INDEXED BY clause is not allowed on UPDATE or DELETE statements within triggers"))))
                                             _ (when (some #(and (map? %) (:var %)) (tree-seq coll? seq st))
                                                 (throw (lx/sql-error "trigger cannot use variables")))]
                                         (expect-op p ";")
                                         (recur (conj acc st))))))]
                  {:kind :create-trigger :name name :schema sch :temp (boolean temp) :name-s @name-s :name-raw (name-raw p @name-s) :if-not-exists ine
                   :timing timing :event event :cols cols :table tname :table-schema tsch :when wh :body body})
                :else (fail! p))))
        "DROP"
        (do (advance! p)
            (let [t (advance! p)
                  what (case (:u t) "TABLE" :table "INDEX" :index "VIEW" :view "TRIGGER" :trigger (syntax-error p t))
                  ie (when (at-kw? p "IF") (advance! p) (expect-kw p "EXISTS") true)
                  n1 (parse-nm p)
                  [sch name] (if (accept-op p ".") [n1 (parse-nm p)] [nil n1])]
              {:kind :drop :what what :if-exists ie :name name :schema sch}))
        "REINDEX"
        (do (advance! p)
            (let [name (when (or (ident-tok? (tok p)) (= :str (:t (tok p)))) (parse-nm p))
                  name (if (and name (accept-op p ".")) (parse-nm p) name)]
              {:kind :reindex :name name}))
        "ALTER"
        (do (advance! p) (expect-kw p "TABLE")
            (let [n1 (parse-nm p)
                  [sch tname] (if (accept-op p ".") [n1 (parse-nm p)] [nil n1])]
              (merge
               {:kind :alter :schema sch :table tname}
               (cond
                 (accept-kw p "RENAME")
                 (cond
                   (accept-kw p "TO") {:action :rename :to (parse-nm p)}
                   :else (do (accept-kw p "COLUMN")
                             (let [rt (tok p) old (parse-nm p)] (expect-kw p "TO") {:action :rename-col :old old :old-raw (tok-text p rt) :to (parse-nm p)})))
                 (accept-kw p "ADD")
                 (if (or (at-kw? p "CONSTRAINT") (at-kw? p "CHECK"))
                   (let [cs (:s (tok p))
                         cname (when (accept-kw p "CONSTRAINT") (parse-nm p))
                         _ (expect-kw p "CHECK") _ (expect-op p "(")
                         e (parse-expr p)
                         _ (expect-op p ")")]
                     {:action :add-constraint :cname cname :text-s cs :text-e (prev-end p) :e e})
                   (do (accept-kw p "COLUMN") {:action :add :col (parse-column-def p)}))
                 (accept-kw p "DROP")
                 (if (accept-kw p "CONSTRAINT")
                   {:action :drop-constraint :cname (parse-nm p)}
                   (do (accept-kw p "COLUMN") {:action :drop-col :col (parse-nm p)}))
                 (accept-kw p "ALTER")
                 (do (accept-kw p "COLUMN")
                     (let [c (parse-nm p)]
                       (cond (accept-kw p "SET") (do (expect-kw p "NOT") (expect-kw p "NULL") {:action :set-notnull :col c})
                             (accept-kw p "DROP") (do (expect-kw p "NOT") (expect-kw p "NULL") {:action :drop-notnull :col c})
                             :else (fail! p))))
                 :else (fail! p)))))
        ("BEGIN") (do (advance! p)
                      (let [mode (or (when (accept-kw p "DEFERRED") :deferred) (when (accept-kw p "IMMEDIATE") :immediate)
                                     (when (accept-kw p "EXCLUSIVE") :exclusive))]
                        (when (accept-kw p "TRANSACTION") (when (ident-tok? (tok p)) (parse-nm p)))
                        {:kind :begin :mode mode}))
        ("COMMIT" "END") (do (advance! p) (when (accept-kw p "TRANSACTION") (when (ident-tok? (tok p)) (parse-nm p))) {:kind :commit})
        "ROLLBACK" (do (advance! p)
                       (when (accept-kw p "TRANSACTION") (when (and (ident-tok? (tok p)) (not (at-kw? p "TO"))) (parse-nm p)))
                       (if (accept-kw p "TO")
                         (do (accept-kw p "SAVEPOINT") {:kind :rollback :to (parse-nm p)})
                         {:kind :rollback}))
        "SAVEPOINT" (do (advance! p) {:kind :savepoint :name (parse-nm p)})
        "ATTACH" (do (advance! p) (accept-kw p "DATABASE")
                     (let [f (parse-expr p)]
                       (expect-kw p "AS")
                       (let [n (parse-expr p)]
                         (when (accept-kw p "KEY") (parse-expr p))
                         {:kind :attach :file f :as n})))
        "DETACH" (do (advance! p) (accept-kw p "DATABASE") {:kind :detach :name (parse-expr p)})
        "RELEASE" (do (advance! p) (accept-kw p "SAVEPOINT") {:kind :release :name (parse-nm p)})
        "ANALYZE" (do (advance! p)
                      (let [n1 (when (or (ident-tok? (tok p)) (= :str (:t (tok p)))) (parse-nm p))
                            [sch n] (if (and n1 (accept-op p ".")) [n1 (parse-nm p)] [nil n1])]
                        {:kind :analyze :schema sch :name n}))
        "VACUUM" (do (advance! p)
                     (let [sch (when (and (or (ident-tok? (tok p)) (= :str (:t (tok p)))) (not (at-kw? p "INTO"))) (parse-nm p))
                           into (when (accept-kw p "INTO") (parse-expr p))]
                       {:kind :vacuum :schema sch :into into}))
        "PRAGMA" (do (advance! p)
                     (let [n1 (parse-nm p)
                           [sch n] (if (accept-op p ".") [n1 (parse-nm p)] [nil n1])
                           pval (fn []
                                  (let [sign (cond (accept-op p "-") "-" (accept-op p "+") "" :else "")
                                        t (advance! p)]
                                    (when (or (= :eof (:t t)) (= :op (:t t)) (= :blob (:t t))
                                              (and (= :id (:t t)) (#{"NULL"} (:u t)))
                                              (and (= :id (:t t)) (reserved (:u t)) (not (join-kw (:u t)))
                                                   (not (#{"ON" "DELETE" "DEFAULT"} (:u t)))))
                                      (syntax-error p t))
                                    (str sign (:v t))))
                           v (cond (accept-op p "=") (pval)
                                   (accept-op p "(") (let [x (pval)] (expect-op p ")") x)
                                   :else nil)]
                       {:kind :pragma :schema sch :name n :value v}))
        "EXPLAIN" (fail! p)
        (fail! p)))))

;; SQLite's tokenizer (analyzeWindowKeyword etc.): WINDOW is a keyword only before "id AS", OVER only after ")"
;; and before "(" or an id, FILTER only after ")" and before "("; elsewhere they are plain identifiers.
(defn- fix-window-keywords [toks]
  (let [toks (vec toks) n (count toks)
        op? (fn [t v] (and t (= :op (:t t)) (= v (:v t))))]
    (vec (map-indexed
          (fn [i t]
            (if (and (= :id (:t t)) (#{"OVER" "FILTER" "WINDOW"} (:u t)))
              (let [prev (when (pos? i) (nth toks (dec i)))
                    nx (when (< (inc i) n) (nth toks (inc i)))
                    nx2 (when (< (+ i 2) n) (nth toks (+ i 2)))
                    kw? (case (:u t)
                          "OVER" (and (op? prev ")") (or (op? nx "(") (and nx (ident-tok? nx))))
                          "FILTER" (and (op? prev ")") (op? nx "("))
                          "WINDOW" (and nx (ident-tok? nx) nx2 (= :id (:t nx2)) (= "AS" (:u nx2))))]
                (if kw? t (assoc t :u (str "\u0000" (:u t)))))
              t))
          toks))))

(defn parse-statements
  "Parses statements one at a time from sql starting at token index; returns a lazy-ish iterator fn.
  Usage: (next-statement state) -> [stmt state'] | nil at end. Throws ex-info with :sql-error on syntax error."
  [^String sql]
  (P. sql (fix-window-keywords (lx/tokenize sql)) (int-array 1)))

(defn- stack-overflow?
  "Approximates Lemon's parser stack (about 2494 entries) from the nesting of parentheses, calls and CASE."
  [toks start]
  (let [n (count toks)]
    (loop [i start frames [] cur 0 mx 0 prev nil]
      (if (or (>= i n) (> mx 2493))
        (> mx 2493)
        (let [t (nth toks i)]
          (cond
            (= :eof (:t t)) false
            (and (= :op (:t t)) (= ";" (:v t)) (empty? frames)) false
            (and (= :op (:t t)) (= "(" (:v t)))
            (let [c (cond (and prev (or (= :qid (:t prev))
                                        (and (= :id (:t prev)) (not (reserved (:u prev)))
                                             (not (contains? #{"SELECT" "VALUES" "IN" "FROM" "JOIN" "AS" "WITH" "VIEW" "TABLE" "SET"} (:u prev))))))
                          3
                          (and prev (or (and (= :op (:t prev)) (not= ")" (:v prev)) (not= "(" (:v prev)) (not= "," (:v prev)))
                                        (and (= :id (:t prev)) (contains? #{"AND" "OR" "NOT" "WHEN" "THEN" "ELSE" "IS" "IN" "BETWEEN" "LIKE"} (:u prev)))))
                          3
                          (kw? (nth toks (inc i)) "SELECT") 6
                          :else 1)
                  cur (+ cur c)]
              (recur (inc i) (conj frames c) cur (max mx cur) t))
            (and (= :op (:t t)) (= ")" (:v t)) (seq frames) (not= :case (peek frames)))
            (recur (inc i) (pop frames) (- cur (long (peek frames))) mx t)
            (= "CASE" (:u t))
            (recur (inc i) (conj frames 3) (+ cur 3) (max mx (+ cur 3)) t)
            (and (= "END" (:u t)) (seq frames) (= 3 (peek frames)))
            (recur (inc i) (pop frames) (- cur 3) mx t)
            :else (recur (inc i) frames cur mx t)))))))

(defn- expr-height [x]
  (cond
    (map? x) (let [h (reduce (fn [m v] (max m (long (expr-height v)))) 0 (vals x))]
               (cond (or (#{:subq :exists} (:op x)) (and (:op x) (:select x))) (+ h 23)
                     (:op x) (inc h)
                     :else h))
    (sequential? x) (reduce (fn [m v] (max m (long (expr-height v)))) 0 x)
    :else 0))

(defn next-statement
  "Returns the next statement AST (with :sql-text) or nil if no more. Skips empty statements."
  [^P p]
  (loop []
    (let [t (tok p)]
      (cond
        (= :eof (:t t)) nil
        (and (= :op (:t t)) (= ";" (:v t))) (do (advance! p) (recur))
        :else
        (let [st-s (:s t)
              _ (when (> (count (.-toks p)) 800)
                  (when (stack-overflow? (.-toks p) (pos p)) (throw (lx/sql-error "Recursion limit"))))
              st (parse-statement* p)
              _ (when (and (> (count (.-toks p)) 100) (map? st) (> (long (expr-height st)) 1000))
                  (throw (lx/sql-error "Expression tree is too large (maximum depth 1000)")))
              t (tok p)
              st (if (map? st) (assoc st :st-s st-s :st-e (prev-end p) :st-next (or (:s t) (count (.-sql p)))) st)]
          (cond
            (= :eof (:t t)) st
            (and (= :op (:t t)) (= ";" (:v t))) (do (advance! p) st)
            :else (syntax-error p t)))))))

(defn sql-of [^P p] (.-sql p))

(ns sqlclj.parser
  "Recursive-descent SQL parser producing a map-based AST. Every expression node carries :s/:e
  (source offsets) so that result column names can use the text as written."
  (:require [clojure.string :as str]
            [sqlclj.lexer :as lx]))

(set! *warn-on-reflection* true)

(def reserved
  #{"ADD" "ALL" "ALTER" "AND" "AS" "AUTOINCREMENT" "BETWEEN" "CASE" "CHECK" "COLLATE" "COMMIT"
    "CONSTRAINT" "CREATE" "CROSS" "DEFAULT" "DEFERRABLE" "DELETE" "DISTINCT" "DROP" "ELSE" "ESCAPE"
    "EXCEPT" "EXISTS" "FOREIGN" "FROM" "FULL" "GROUP" "HAVING" "IN" "INDEX" "INDEXED" "INNER"
    "INSERT" "INTERSECT" "INTO" "IS" "ISNULL" "JOIN" "LEFT" "LIMIT" "NATURAL" "NOT" "NOTNULL" "NULL"
    "ON" "OR" "ORDER" "OUTER" "PRIMARY" "REFERENCES" "RIGHT" "SELECT" "SET" "TABLE" "THEN" "TO"
    "TRANSACTION" "UNION" "UNIQUE" "UPDATE" "USING" "VALUES" "WHEN" "WHERE" "RETURNING" "NOTHING"
    "WINDOW" "FILTER" "OVER"})

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

(declare parse-expr parse-select parse-expr-prec)

(defn- node [m s e] (assoc m :ps s :pe e))

(defn- prev-end [^P p] (:e (nth (.-toks p) (dec (pos p)))))

(defn- parse-type-name
  "Parses a type name (sequence of identifiers, optional (n[,m])). Returns string or nil."
  [^P p]
  (let [start (pos p)
        names (loop [acc []]
                (let [t (tok p)]
                  (if (and (or (= :id (:t t)) (= :qid (:t t)))
                           (or (= :qid (:t t)) (not (reserved (:u t)))))
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
          args (loop [acc [(parse-expr p)]]
                 (if (accept-op p ",") (recur (conj acc (parse-expr p))) acc))
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
      :var (do (advance! p) (node {:op :lit :v nil} s (:e t)))
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
          (if (at-op? p ".")
            (do (advance! p)
                (let [n2 (parse-nm p)]
                  (if (at-op? p ".")
                    (do (advance! p) (let [n3 (parse-nm p)] (node {:op :col :schema (:v t) :table n2 :name n3} s (prev-end p))))
                    (node {:op :col :table (:v t) :name n2} s (prev-end p)))))
            (node {:op :col :name (:v t) :dq (:dq t)} s (:e t))))
      :id
      (let [u (:u t)]
        (cond
          (= u "NULL") (do (advance! p) (node {:op :lit :v nil} s (:e t)))
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
          (= u "RAISE") (fail! p)
          (reserved u) (fail! p)
          :else
          (do (advance! p)
              (cond
                (at-op? p "(")
                (do (advance! p)
                    (let [fa (parse-function-args p)]
                      (when (at-kw? p "FILTER") (fail! p))
                      (when (at-kw? p "OVER") (fail! p))
                      (node (merge {:op :fn :name (str/lower-case (:v t))} fa) s (prev-end p))))
                (at-op? p ".")
                (do (advance! p)
                    (let [n2 (parse-nm p)]
                      (if (at-op? p ".")
                        (do (advance! p) (let [n3 (parse-nm p)] (node {:op :col :schema (:v t) :table n2 :name n3} s (prev-end p))))
                        (node {:op :col :table (:v t) :name n2} s (prev-end p)))))
                :else (node {:op :col :name (:v t)} s (:e t))))))
      (fail! p))))

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
                              (if (and (= :lit (:op r)) (nil? (:kind r)) (nil? (:v r)) (not dist))
                                (fold-null-test n left (not isnot))
                                n))))
              "IN" (recur (parse-in-rhs p left s negate))
              ("LIKE" "GLOB" "MATCH" "REGEXP")
              (let [r (parse-expr-prec p 5)
                    esc (when (accept-kw p "ESCAPE") (parse-expr-prec p 5))]
                (recur (node {:op :like :o (keyword (str/lower-case u)) :e left :pat r :esc esc :not negate}
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

(defn- parse-alias [^P p]
  (if (accept-kw p "AS")
    (parse-nm p)
    (let [t (tok p)]
      (when (or (and (ident-tok? t) (not (join-kw (:u t))))
                (= :str (:t t)))
        (advance! p) (:v t)))))

(defn- parse-result-col [^P p]
  (let [t (tok p) s (:s t)]
    (cond
      (at-op? p "*") (do (advance! p) {:star true})
      (and (or (ident-tok? t) (= :str (:t t))) (= "." (:v (tok-at p 1))) (= "*" (:v (tok-at p 2))))
      (do (advance! p) (advance! p) (advance! p) {:star true :table (:v t)})
      :else
      (let [e (parse-expr p)
            alias (parse-alias p)]
        {:e e :alias alias}))))

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
                       :else nil)]
          {:table nm :schema sch :alias alias :indexed-by ib})))))

(defn- parse-join-op
  "Returns nil if no join follows, else a join type keyword."
  [^P p]
  (cond
    (accept-op p ",") :cross-comma
    (at-kw? p "JOIN") (do (advance! p) :inner)
    (and (= :id (:t (tok p))) (join-kw (:u (tok p))))
    (let [kws (loop [acc []]
                (if (and (= :id (:t (tok p))) (join-kw (:u (tok p))))
                  (recur (conj acc (:u (advance! p))))
                  acc))]
      (expect-kw p "JOIN")
      (let [s (set kws)]
        (cond
          (s "NATURAL") (cond (s "LEFT") :natural-left (s "RIGHT") :natural-right (s "FULL") :natural-full :else :natural)
          (s "LEFT") :left
          (s "RIGHT") :right
          (s "FULL") :full
          (s "CROSS") :cross
          :else :inner)))
    :else nil))

(defn parse-from [^P p]
  (loop [items [(parse-table-ref p)]]
    (let [save (pos p)
          jt (parse-join-op p)]
      (if (nil? jt)
        items
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
        (when (at-kw? p "WINDOW") (fail! p))
        {:type :core :distinct distinct :cols cols :from from :where where :group group :having having}))))

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
        order (when (at-kw? p "ORDER") (advance! p) (expect-kw p "BY") (parse-order-by p))
        [limit offset] (when (accept-kw p "LIMIT")
                         (let [a (parse-expr p)]
                           (cond (accept-kw p "OFFSET") [a (parse-expr p)]
                                 (accept-op p ",") [(parse-expr p) a]
                                 :else [a nil])))]
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
        (if (#{:int :float :hex} (:t t))
          (let [e (parse-primary p)] (node {:op :unary :o (if (= o "-") :neg :pos) :e e} s (:pe e)))
          (fail! p)))
      :else (parse-primary p))))

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
               (let [cname (when (accept-kw p "CONSTRAINT") (parse-nm p))
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
                                 (ident-tok? (tok p)) (let [t (advance! p)] (node {:op :lit :v (:v t)} (:s t) (:e t)))
                                 :else (parse-signed-literal p))]
                         (recur (conj acc {:c :default :e e}))))
                   (kw? t "COLLATE")
                   (do (advance! p) (recur (conj acc {:c :collate :coll (parse-nm p)})))
                   (kw? t "REFERENCES")
                   (do (advance! p)
                       (recur (conj acc (assoc (parse-fk-tail p) :c :fk :from [name]))))
                   (kw? t "GENERATED") (fail! p)
                   (kw? t "AS") (fail! p)
                   :else (do (when cname (fail! p)) acc))))]
    {:name name :type type :cons cons :s cs :e (prev-end p)}))

(defn- parse-indexed-cols [^P p]
  (expect-op p "(")
  (let [cols (loop [acc []]
               (let [e (parse-expr p)
                     [e coll] (if (= :collate (:op e)) [(:e e) (:coll e)] [e nil])
                     desc (cond (accept-kw p "DESC") true (accept-kw p "ASC") false :else false)
                     acc (conj acc {:e e :coll coll :desc desc})]
                 (if (accept-op p ",") (recur acc) acc)))]
    (expect-op p ")")
    cols))

(defn- parse-create-table [^P p temp]
  (let [ine (when (at-kw? p "IF") (advance! p) (expect-kw p "NOT") (expect-kw p "EXISTS") true)
        name-s (volatile! (:s (tok p)))
        n1 (parse-nm p)
        [sch name] (if (accept-op p ".") (do (vreset! name-s (:s (tok p))) [n1 (parse-nm p)]) [nil n1])]
    (if (accept-kw p "AS")
      {:kind :create-table :name name :schema sch :temp (boolean temp) :name-s @name-s :if-not-exists ine :as (parse-select p)}
      (do
        (expect-op p "(")
        (let [[cols tcons]
              (loop [cols [] tcons []]
                (let [t (tok p)
                      cname (when (kw? t "CONSTRAINT") (advance! p) (parse-nm p))
                      t (tok p)]
                  (cond
                    (or (kw? t "PRIMARY") (kw? t "UNIQUE") (kw? t "CHECK") (kw? t "FOREIGN"))
                    (if (empty? cols) (fail! p)
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
                                :else (fail! p))))
                    (seq tcons) (fail! p)
                    :else
                    (let [cd (parse-column-def p)
                          cols (conj cols cd)]
                      (cond (accept-op p ",") (recur cols tcons)
                            (accept-op p ")") [cols tcons]
                            :else (fail! p))))))
              opts (loop [opts #{}]
                     (cond
                       (accept-kw p "STRICT") (let [o (conj opts :strict)] (if (accept-op p ",") (recur o) o))
                       (at-kw? p "WITHOUT") (do (advance! p)
                                                (let [t (advance! p)]
                                                  (when-not (= "ROWID" (:u t)) (syntax-error p t)))
                                                (let [o (conj opts :without-rowid)] (if (accept-op p ",") (recur o) o)))
                       :else opts))]
          {:kind :create-table :name name :schema sch :temp (boolean temp) :name-s @name-s :if-not-exists ine :cols cols :tcons tcons
           :strict (contains? opts :strict) :without-rowid (contains? opts :without-rowid)})))))

;; ------------------------------------------------------------------ DML

(defn- parse-returning [^P p]
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
            (recur (conj acc (merge {:target target} action)))))
      acc)))

(defn- parse-insert [^P p or-action]
  (expect-kw p "INTO")
  (let [n1 (parse-nm p)
        [sch tname] (if (accept-op p ".") [n1 (parse-nm p)] [nil n1])
        alias (when (accept-kw p "AS") (parse-nm p))
        cols (when (at-op? p "(")
               (advance! p)
               (let [c (loop [acc [(parse-nm p)]] (if (accept-op p ",") (recur (conj acc (parse-nm p))) acc))]
                 (expect-op p ")") c))
        src (cond
              (at-kw? p "DEFAULT") (do (advance! p) (expect-kw p "VALUES") {:default true})
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
        ret (parse-returning p)]
    (merge {:kind :insert :or or-action :table tname :alias alias :cols cols :upsert upsert :returning ret} src)))

(defn- parse-qualified-table [^P p]
  (let [n1 (parse-nm p)
        [sch tname] (if (accept-op p ".") [n1 (parse-nm p)] [nil n1])
        alias (when (accept-kw p "AS") (parse-nm p))
        alias (or alias (let [t (tok p)] (when (and (ident-tok? t) (not (#{"SET" "WHERE" "INDEXED" "NOT" "RETURNING" "ORDER" "LIMIT"} (:u t)))) (advance! p) (:v t))))
        ib (cond (at-kw? p "INDEXED") (do (advance! p) (expect-kw p "BY") (parse-nm p))
                 (and (at-kw? p "NOT") (kw? (tok-at p 1) "INDEXED")) (do (advance! p) (advance! p) :none)
                 :else nil)]
    {:table tname :alias alias :indexed-by ib}))

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
        ret (parse-returning p)]
    (merge {:kind :update :or or-action :sets sets :from from :where where :returning ret} qt)))

(defn- parse-delete [^P p]
  (expect-kw p "FROM")
  (let [qt (parse-qualified-table p)
        where (when (accept-kw p "WHERE") (parse-expr p))
        ret (parse-returning p)]
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
                       (if (and (= :id (:t t2)) (#{"INSERT" "REPLACE" "UPDATE" "DELETE"} (:u t2)))
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
                      cols (when (at-op? p "(") (advance! p)
                                 (let [c (loop [acc [(parse-nm p)]] (if (accept-op p ",") (recur (conj acc (parse-nm p))) acc))]
                                   (expect-op p ")") c))
                      _ (expect-kw p "AS")
                      sel (parse-select p)]
                  {:kind :create-view :name name :schema sch :temp (boolean temp) :name-s @name-s :if-not-exists ine :cols cols :select sel})
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
                             (let [old (parse-nm p)] (expect-kw p "TO") {:action :rename-col :old old :to (parse-nm p)})))
                 (accept-kw p "ADD") (do (accept-kw p "COLUMN") {:action :add :col (parse-column-def p)})
                 (accept-kw p "DROP") (do (accept-kw p "COLUMN") {:action :drop-col :col (parse-nm p)})
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
        "RELEASE" (do (advance! p) (accept-kw p "SAVEPOINT") {:kind :release :name (parse-nm p)})
        "ANALYZE" (do (advance! p) (when (ident-tok? (tok p)) (parse-nm p)) {:kind :noop})
        "VACUUM" (do (advance! p) {:kind :noop})
        "PRAGMA" (do (advance! p)
                     (let [n1 (parse-nm p)
                           [sch n] (if (accept-op p ".") [n1 (parse-nm p)] [nil n1])
                           pval (fn []
                                  (let [sign (cond (accept-op p "-") "-" (accept-op p "+") "" :else "")
                                        t (advance! p)]
                                    (when (= :eof (:t t)) (syntax-error p t))
                                    (str sign (:v t))))
                           v (cond (accept-op p "=") (pval)
                                   (accept-op p "(") (let [x (pval)] (expect-op p ")") x)
                                   :else nil)]
                       {:kind :pragma :schema sch :name n :value v}))
        "EXPLAIN" (fail! p)
        (fail! p)))))

(defn parse-statements
  "Parses statements one at a time from sql starting at token index; returns a lazy-ish iterator fn.
  Usage: (next-statement state) -> [stmt state'] | nil at end. Throws ex-info with :sql-error on syntax error."
  [^String sql]
  (P. sql (lx/tokenize sql) (int-array 1)))

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
              st (parse-statement* p)
              t (tok p)
              st (if (map? st) (assoc st :st-s st-s :st-e (prev-end p) :st-next (or (:s t) (count (.-sql p)))) st)]
          (cond
            (= :eof (:t t)) st
            (and (= :op (:t t)) (= ";" (:v t))) (do (advance! p) st)
            :else (syntax-error p t)))))))

(defn sql-of [^P p] (.-sql p))

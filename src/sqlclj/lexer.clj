(ns sqlclj.lexer
  "SQL tokenizer following sqlite3GetToken."
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn sql-error [msg] (ex-info msg {:sql-error msg}))

(defn- id-start? [c]
  (let [c (int c)]
    (or (and (>= c 97) (<= c 122)) (and (>= c 65) (<= c 90)) (= c 95) (>= c 128))))
(defn- id-char? [c]
  (let [c (int c)]
    (or (id-start? c) (and (>= c 48) (<= c 57)) (= c 36))))
(defn- digit? [c] (let [c (int c)] (and (>= c 48) (<= c 57))))
(defn- hex? [c] (let [c (int c)] (or (digit? c) (and (>= c 97) (<= c 102)) (and (>= c 65) (<= c 70)))))
(defn- space? [c] (let [c (int c)] (or (= c 32) (= c 9) (= c 10) (= c 12) (= c 13))))

(defn tokenize
  "Returns a vector of tokens {:t type :v value :s start :e end}. Types: :id :qid :str :blob :int :float :op :illegal.
  Ends with an :eof token."
  [^String s]
  (let [n (.length s)
        out (transient [])]
    (loop [i (Long/valueOf 0)]
      (if (>= (long i) n)
        (persistent! (conj! out {:t :eof :s n :e n :v ""}))
        (let [c (.charAt s i)]
          (cond
            (space? c) (recur (inc i))
            (and (= c \-) (< (inc i) n) (= (.charAt s (inc i)) \-))
            (recur (loop [j i] (if (and (< j n) (not= (.charAt s j) \newline)) (recur (inc j)) j)))
            (and (= c \/) (< (inc i) n) (= (.charAt s (inc i)) \*))
            (recur (let [k (.indexOf s "*/" (+ i 2))] (if (neg? k) n (+ k 2))))
            ;; blob literal
            (and (or (= c \x) (= c \X)) (< (inc i) n) (= (.charAt s (inc i)) \'))
            (let [j (loop [j (+ i 2)] (if (and (< j n) (hex? (.charAt s j))) (recur (inc j)) j))
                  ok (and (< j n) (= (.charAt s j) \') (even? (- j i 2)))]
              (if ok
                (do (conj! out {:t :blob :v (subs s (+ i 2) j) :s i :e (inc j)}) (recur (inc j)))
                (let [k (loop [k j] (if (and (< k n) (not= (.charAt s k) \')) (recur (inc k)) k))
                      k (if (< k n) (inc k) k)]
                  (conj! out {:t :illegal :s i :e k}) (recur k))))
            (id-start? c)
            (let [j (loop [j (inc i)] (if (and (< j n) (id-char? (.charAt s j))) (recur (inc j)) j))]
              (conj! out {:t :id :v (subs s i j) :u (str/upper-case (subs s i j)) :s i :e j})
              (recur j))
            (or (digit? c) (and (= c \.) (< (inc i) n) (digit? (.charAt s (inc i)))))
            (if (and (= c \0) (< (inc i) n) (or (= (.charAt s (inc i)) \x) (= (.charAt s (inc i)) \X))
                     (< (+ i 2) n) (hex? (.charAt s (+ i 2))))
              (let [j (loop [j (+ i 2)] (if (and (< j n) (hex? (.charAt s j))) (recur (inc j)) j))]
                (if (and (< j n) (id-char? (.charAt s j)))
                  (let [k (loop [k j] (if (and (< k n) (id-char? (.charAt s k))) (recur (inc k)) k))]
                    (conj! out {:t :illegal :s i :e k}) (recur k))
                  (do (conj! out {:t :hex :v (subs s i j) :s i :e j}) (recur j))))
              (let [j (loop [j i] (if (and (< j n) (digit? (.charAt s j))) (recur (inc j)) j))
                    [j fl] (if (and (< j n) (= (.charAt s j) \.))
                             [(loop [j (inc j)] (if (and (< j n) (digit? (.charAt s j))) (recur (inc j)) j)) true]
                             [j false])
                    [j fl] (if (and (< j n) (or (= (.charAt s j) \e) (= (.charAt s j) \E))
                                    (or (and (< (inc j) n) (digit? (.charAt s (inc j))))
                                        (and (< (+ j 2) n) (or (= (.charAt s (inc j)) \+) (= (.charAt s (inc j)) \-))
                                             (digit? (.charAt s (+ j 2))))))
                             [(loop [j (+ j 2)] (if (and (< j n) (digit? (.charAt s j))) (recur (inc j)) j)) true]
                             [j fl])]
                (if (and (< j n) (id-char? (.charAt s j)))
                  (let [k (loop [k j] (if (and (< k n) (id-char? (.charAt s k))) (recur (inc k)) k))]
                    (conj! out {:t :illegal :s i :e k}) (recur k))
                  (do (conj! out {:t (if fl :float :int) :v (subs s i j) :s i :e j}) (recur j)))))
            (= c \')
            (let [sb (StringBuilder.)
                  j (loop [j (inc i)]
                      (cond (>= j n) -1
                            (= (.charAt s j) \')
                            (if (and (< (inc j) n) (= (.charAt s (inc j)) \'))
                              (do (.append sb \') (recur (+ j 2)))
                              (inc j))
                            :else (do (.append sb (.charAt s j)) (recur (inc j)))))]
              (if (neg? j)
                (do (conj! out {:t :illegal :s i :e n}) (recur n))
                (do (conj! out {:t :str :v (.toString sb) :s i :e j}) (recur j))))
            (or (= c \") (= c \`) (= c \[))
            (let [close (case c \" \" \` \` \[ \])
                  sb (StringBuilder.)
                  j (loop [j (inc i)]
                      (cond (>= j n) -1
                            (= (.charAt s j) close)
                            (if (and (not= close \]) (< (inc j) n) (= (.charAt s (inc j)) close))
                              (do (.append sb close) (recur (+ j 2)))
                              (inc j))
                            :else (do (.append sb (.charAt s j)) (recur (inc j)))))]
              (if (neg? j)
                (do (conj! out {:t :illegal :s i :e n}) (recur n))
                (do (conj! out {:t :qid :v (.toString sb) :dq (= c \") :s i :e j}) (recur j))))
            :else
            (let [two (when (< (inc i) n) (subs s i (+ i 2)))
                  op (cond (#{"<=" ">=" "==" "!=" "<>" "<<" ">>" "||" "->"} two) two
                           (#{\( \) \; \, \+ \- \* \/ \% \= \< \> \& \| \~ \.} c) (str c)
                           :else nil)]
              (cond
                (= op "->") (let [len (if (and (< (+ i 2) n) (= (.charAt s (+ i 2)) \>)) 3 2)]
                              (conj! out {:t :op :v (subs s i (+ i len)) :s i :e (+ i len)}) (recur (+ i len)))
                op (do (conj! out {:t :op :v op :s i :e (+ i (count op))}) (recur (+ i (count op))))
                (= c \?) (let [j (loop [j (inc i)] (if (and (< j n) (digit? (.charAt s j))) (recur (inc j)) j))]
                           (conj! out {:t :var :v (subs s i j) :s i :e j}) (recur j))
                (and (or (= c \:) (= c \@) (= c \$))
                     (let [j (loop [j (inc i)] (if (and (< j n) (or (id-char? (.charAt s j)) (and (= c \$) (= (.charAt s j) \:) (< (inc j) n) (= (.charAt s (inc j)) \:)))) (recur (inc j)) j))]
                       (> j (inc i))))
                (let [j (loop [j (inc i)] (if (and (< j n) (or (id-char? (.charAt s j)) (and (= c \$) (= (.charAt s j) \:) (< (inc j) n) (= (.charAt s (inc j)) \:)))) (recur (inc j)) j))]
                  (conj! out {:t :var :v (subs s i j) :s i :e j}) (recur j))
                :else (do (conj! out {:t :illegal :s i :e (inc i)}) (recur (inc i)))))))))))

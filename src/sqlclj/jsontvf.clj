(ns sqlclj.jsontvf
  "json_each() and json_tree() table-valued functions."
  (:require [clojure.string :as str]
            [sqlclj.engine :as e]
            [sqlclj.values :as v]
            [sqlclj.json :as j]))

(defn- hdr-len [^long payload]
  (cond (<= payload 11) 1 (<= payload 0xff) 2 (<= payload 0xffff) 3 :else 5))

(defn- node-size
  "size in bytes of the minimal JSONB encoding of node"
  ^long [node]
  (let [t (j/ntype node) p (j/npay node)]
    (case (long t)
      (0 1 2) 1
      (3 4 5 6 7 8 9 10) (let [n (alength ^bytes (v/str->utf8 p))] (+ (hdr-len n) n))
      (11 12) (let [n (reduce + 0 (map node-size p))] (+ (hdr-len n) n)))))

(defn- label-path [^String label]
  (if (and (pos? (count label))
           (every? (fn [c] (let [c (int c)] (or (and (>= c 48) (<= c 57)) (and (>= c 65) (<= c 90)) (and (>= c 97) (<= c 122)) (= c 95) (>= c 128)))) label)
           (not (Character/isDigit (.charAt label 0))))
    (str "." label)
    (str ".\"" label "\"")))

(defn- scalar-row-value [node]
  (j/node->sql node))

(defn- row [key node id parent fullkey path]
  (let [t (j/ntype node)
        val (j/node->sql node)
        container? (>= t 11)]
    [key val (j/node-type-name node) (if container? nil (if (j/json-subtype? val) nil val)) id parent fullkey path]))

(defn- children
  "[[key child-node id-offset value-offset] ...] relative to the container start"
  [node]
  (let [t (j/ntype node) p (j/npay node)
        hl (hdr-len (reduce + 0 (map node-size p)))]
    (if (= t 11)
      (loop [cs (seq p) off hl i 0 out []]
        (if cs
          (recur (next cs) (+ off (node-size (first cs))) (inc i) (conj out [i (first cs) off off]))
          out))
      (loop [cs (seq p) off hl out []]
        (if cs
          (let [k (first cs) vn (second cs)
                voff (+ off (node-size k))]
            (recur (nnext cs) (+ voff (node-size vn)) (conj out [(j/key-str k) vn off voff])))
          out)))))

(defn- walk
  "rows for node (value start `start`, id `id`)."
  [node start id parent fullkey path key tree? top?]
  (let [t (j/ntype node)
        self (row key node id parent fullkey path)]
    (cond
      (< t 11) [(if (and top? (not tree?)) (assoc self 0 nil 7 fullkey) self)]
      :else
      (let [kids (children node)
            sub (mapcat (fn [[k child idoff voff]]
                          (let [cfull (str fullkey (if (= t 11) (str "[" k "]") (label-path k)))]
                            (if tree?
                              (walk child (+ start voff) (+ start idoff) id cfull fullkey k true false)
                              [(row k child (+ start idoff) nil cfull fullkey)])))
                        kids)]
        (if tree? (cons self sub) sub)))))

(defn- each-rows [tree? args]
  (let [[x p] args]
    (when (and (not (nil? x)) (or (< (count args) 2) (not (nil? p))))
      (let [root (j/arg->node x)
            path (if (< (count args) 2) "$" (v/text-of p))]
        (when root
          (if (< (count args) 2)
            (walk root 0 0 nil "$" "$" nil tree? true)
            (do
              (when-not (= (.charAt ^String (if (empty? path) " " path) 0) \$)
                (j/jerr (str "bad JSON path: '" (.replace ^String path "'" "''") "'")))
              (when-let [[node start key id] (j/locate root path)]
                (walk node start id nil path (j/parent-path path) key tree? true)))))))))

(defn- resolver [lname db]
  (when (#{"json_each" "json_tree" "jsonb_each" "jsonb_tree"} lname)
    (let [tree? (str/ends-with? lname "tree")]
      {:cols ["key" "value" "type" "atom" "id" "parent" "fullkey" "path"]
       :hidden ["json" "root"]
       :rows (fn [db args] (each-rows tree? args))})))

(swap! e/tvf-resolvers (fn [rs] (if (some #(= % #'resolver) rs) rs (conj rs #'resolver))))

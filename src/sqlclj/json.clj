(ns sqlclj.json
  "SQLite's JSON functions (json.c): text/JSONB parsing into a tree, path lookup and edits, rendering."
  (:require [clojure.string :as str]
            [sqlclj.values :as v]
            [sqlclj.num :as num]
            [sqlclj.printf :as pf])
  (:import [sqlclj.values Blob]))

;; ---------------------------------------------------------------- node model
;; node = [type payload]; type: 0 null 1 true 2 false 3 int 4 int5 5 float 6 float5 7 text 8 textj 9 text5 10 textraw
;; 11 array (payload: vector of nodes) 12 object (payload: vector k0 v0 k1 v1 ...)
(def ^:const T-NULL 0) (def ^:const T-TRUE 1) (def ^:const T-FALSE 2) (def ^:const T-INT 3) (def ^:const T-INT5 4)
(def ^:const T-FLOAT 5) (def ^:const T-FLOAT5 6) (def ^:const T-TEXT 7) (def ^:const T-TEXTJ 8) (def ^:const T-TEXT5 9)
(def ^:const T-TEXTRAW 10) (def ^:const T-ARRAY 11) (def ^:const T-OBJECT 12)

(defn ntype ^long [node] (long (nth node 0)))
(defn npay [node] (nth node 1))
(defn text-type? [^long t] (and (>= t 7) (<= t 10)))

(def malformed (ex-info "malformed JSON" {:sql-error "malformed JSON"}))
(defn jerr [msg] (throw (ex-info msg {:sql-error msg})))

;; ---------------------------------------------------------------- subtype registry (identity based)
(def ^:private subtypes (java.util.IdentityHashMap.))
(defn mark-json
  "returns a fresh String instance carrying the JSON subtype"
  ^String [^String s]
  (let [c (String. s)]
    (when (> (.size subtypes) 100000) (.clear subtypes))
    (.put subtypes c 74)
    c))
(defn json-subtype? [x] (and (string? x) (.containsKey subtypes x)))
(defn subtype-of [x] (if (and (string? x) (.containsKey subtypes x)) 74 0))
(defn reset-subtypes! [] (.clear subtypes))

;; ---------------------------------------------------------------- character classes
(defn- digit? [c] (let [c (int c)] (and (>= c 48) (<= c 57))))
(defn- xdigit? [c] (let [c (int c)] (or (and (>= c 48) (<= c 57)) (and (>= c 65) (<= c 70)) (and (>= c 97) (<= c 102)))))
(defn- alnum? [c] (let [c (int c)] (or (digit? c) (and (>= c 65) (<= c 90)) (and (>= c 97) (<= c 122)))))
(defn- id1? [c] (let [c (int c)] (or (and (>= c 65) (<= c 90)) (and (>= c 97) (<= c 122)) (= c 95) (= c 36) (>= c 128))))
(defn- id2? [c] (or (id1? c) (digit? c)))

(defn- chat ^long [^String z i] (if (< i (.length z)) (long (.charAt z i)) 0))

(defn- ws5-len
  "json5Whitespace: length of whitespace/comment at z[i] (0 if none)."
  ^long [^String z i]
  (let [c (chat z i)]
    (cond
      (and (>= c 9) (<= c 13)) 1
      (= c 32) 1
      (= c 47) (let [d (chat z (inc i))]
                 (cond
                   (= d 42) ;; /* ... */
                   (let [n (.length z)]
                     (loop [j (+ i 3)]
                       (cond (>= j n) 0
                             (and (= (.charAt z j) \/) (= (.charAt z (dec j)) \*)) (- (inc j) i)
                             :else (recur (inc j)))))
                   (= d 47)
                   (let [n (.length z)]
                     (loop [j (+ i 2)]
                       (if (or (>= j n) (= (.charAt z j) \newline) (= (.charAt z j) \return)
                               (= (.charAt z j) (char 0x2028)) (= (.charAt z j) (char 0x2029)))
                         (- j i) (recur (inc j)))))
                   :else 0))
      (or (= c 0xa0) (= c 0x1680) (and (>= c 0x2000) (<= c 0x200a)) (= c 0x2028) (= c 0x2029) (= c 0x202f)
          (= c 0x205f) (= c 0x3000) (= c 0xfeff)) 1
      :else 0)))

(defn- is4hex? [^String z i] (and (xdigit? (chat z i)) (xdigit? (chat z (+ i 1))) (xdigit? (chat z (+ i 2))) (xdigit? (chat z (+ i 3)))))
(defn- is2hex? [^String z i] (and (xdigit? (chat z i)) (xdigit? (chat z (+ i 1)))))

(defn- ci-prefix? [^String z i ^String w]
  (.regionMatches z true (int i) w 0 (.length w)))

;; ---------------------------------------------------------------- text parser
;; returns node-vector-with-end [node end] on success, or negative code: 0 EOF, -1 error, -2 '}', -3 ']', -4 ',', -5 ':'
;; state: volatile map {:iErr n :nonstd bool :depth n}
(def ^:private ^:const MAX-DEPTH 1000)

(defn- seterr! [st i] (vswap! st assoc :iErr i))
(defn- nonstd! [st] (vswap! st assoc :nonstd true))

(declare parse-val parse-object parse-array)

(defn- parse-string [^String z i st]
  (let [delim (.charAt z i)]
    (when (= delim \') (nonstd! st))
    (loop [j (inc i) opcode T-TEXT]
      (let [c (chat z j)]
        (cond
          (= c (long delim)) [[opcode (subs z (inc i) j)] (inc j)]
          (and (= c 0) (>= j (.length z))) (do (seterr! st j) -1)
          (= c 92)
          (let [j (inc j) c (chat z j)]
            (cond
              (or (= c 34) (= c 92) (= c 47) (= c 98) (= c 102) (= c 110) (= c 114) (= c 116)
                  (and (= c 117) (is4hex? z (inc j))))
              (recur (inc j) (if (= opcode T-TEXT) T-TEXTJ opcode))
              (or (= c 39) (= c 48) (= c 118) (= c 10)
                  (and (= c 0x2028)) (and (= c 0x2029))
                  (and (= c 120) (is2hex? z (inc j))))
              (do (nonstd! st) (recur (inc j) T-TEXT5))
              (= c 13)
              (let [j (if (= (chat z (inc j)) 10) (inc j) j)]
                (nonstd! st) (recur (inc j) T-TEXT5))
              :else (do (seterr! st j) -1)))
          (<= c 0x1f)
          (if (= c 0)
            (do (seterr! st j) -1)
            (do (nonstd! st) (recur (inc j) T-TEXT5)))
          (= c 34) (recur (inc j) T-TEXT5) ; a double quote inside a single-quoted string
          :else (recur (inc j) opcode))))))

(defn- parse-number [^String z i st]
  (let [c (chat z i)
        fin (fn [t j]
              ;; payload from i (a leading + is dropped)
              (let [s (subs z i j)
                    s (if (= (.charAt s 0) \+) (subs s 1) s)]
                [[t s] j]))
        scan-hex (fn [j0] (loop [j j0] (if (xdigit? (chat z j)) (recur (inc j)) j)))]
    (cond
      (and (= c 48) (or (= (chat z (inc i)) 120) (= (chat z (inc i)) 88)) (xdigit? (chat z (+ i 2))))
      (do (nonstd! st) (fin T-INT5 (scan-hex (+ i 3))))
      (and (= c 48) (digit? (chat z (inc i)))) (do (seterr! st (inc i)) -1)
      :else
      (let [early
            (when (<= c 48) ; '+' '-' '.' or '0'
              (when-not (= c 48)
                (cond
                  (not (digit? (chat z (inc i))))
                  (cond
                    (and (or (= (chat z (inc i)) 73) (= (chat z (inc i)) 105)) (ci-prefix? z (inc i) "inf"))
                    (do (nonstd! st)
                        [[T-FLOAT (if (= c 45) "-9e999" "9e999")]
                         (+ i (if (ci-prefix? z (+ i 4) "inity") 9 4))])
                    (= (chat z (inc i)) 46)
                    ;; "-.5" "+.5"
                    (if (digit? (chat z (+ i 2))) :float-dot (do (seterr! st i) -1))
                    :else (do (seterr! st i) -1))
                  (= (chat z (inc i)) 48)
                  (cond (digit? (chat z (+ i 2))) (do (seterr! st (inc i)) -1)
                        (and (or (= (chat z (+ i 2)) 120) (= (chat z (+ i 2)) 88)) (xdigit? (chat z (+ i 3))))
                        (do (nonstd! st) (fin T-INT5 (scan-hex (+ i 4))))
                        :else nil)
                  :else nil)))]
        (cond
          (and early (not= early :float-dot)) early
          :else
          (let [start-dot (or (= early :float-dot) (= c 46))]
            (if (and (= c 46) (not (digit? (chat z (inc i)))))
              (do (seterr! st i) -1)
              (loop [j (if (= early :float-dot) (+ i 2) (inc i)) t (Long/valueOf (long (if start-dot T-FLOAT5 T-INT))) seen-e (Boolean/valueOf false) seen-dp (Boolean/valueOf (boolean start-dot))]
                (let [d (chat z j)]
                  (cond
                    (digit? d) (recur (inc j) t seen-e seen-dp)
                    (= d 46)
                    (if (or seen-dp seen-e)
                      (do (seterr! st j) -1)
                      (recur (inc j) (if (= t T-INT) T-FLOAT t) seen-e true))
                    (or (= d 101) (= d 69))
                    (let [prev (chat z (dec j))]
                      (cond
                        (and (< prev 48) (not (and (= prev 46) (>= (- j 2) i) (digit? (chat z (- j 2))))))
                        (do (seterr! st j) -1)
                        seen-e (do (seterr! st j) -1)
                        :else
                        (let [t2 (if (< prev 48) T-FLOAT5 (if (= t T-INT) T-FLOAT t))
                              j2 (let [nx (chat z (inc j))] (if (or (= nx 43) (= nx 45)) (inc j) j))
                              nx (chat z (inc j2))]
                          (when (< prev 48) (nonstd! st))
                          (if (or (< nx 48) (> nx 57))
                            (do (seterr! st j2) -1)
                            (recur (inc j2) t2 true true)))))
                    :else
                    (let [prev (chat z (dec j))]
                      (if (< prev 48)
                        (if (and (= prev 46) (>= (- j 2) i) (digit? (chat z (- j 2))))
                          (do (nonstd! st) (fin T-FLOAT5 j))
                          (do (seterr! st j) -1))
                        (do (when (and (= t T-FLOAT5)) (nonstd! st))
                            (fin t j))))))))))))))

(defn- parse-val [^String z i st]
  (let [n (.length z)
        c (chat z i)]
    (cond
      (>= i n) 0
      (= c 123) (parse-object z i st)
      (= c 91) (parse-array z i st)
      (or (= c 34) (= c 39)) (parse-string z i st)
      (= c 116) (if (and (.startsWith z "true" (int i)) (not (alnum? (chat z (+ i 4))))) [[T-TRUE nil] (+ i 4)] (do (seterr! st i) -1))
      (= c 102) (if (and (.startsWith z "false" (int i)) (not (alnum? (chat z (+ i 5))))) [[T-FALSE nil] (+ i 5)] (do (seterr! st i) -1))
      (or (= c 110) (= c 78))
      (cond (and (= c 110) (.startsWith z "null" (int i)) (not (alnum? (chat z (+ i 4))))) [[T-NULL nil] (+ i 4)]
            (and (ci-prefix? z i "nan") (not (alnum? (chat z (+ i 3))))) (do (nonstd! st) [[T-NULL nil] (+ i 3)])
            :else (do (seterr! st i) -1))
      (or (= c 105) (= c 73))
      (if (and (ci-prefix? z i "infinity") (not (alnum? (chat z (+ i 8)))))
        (do (nonstd! st) [[T-FLOAT "9e999"] (+ i 8)])
        (do (seterr! st i) -1))
      (= c 125) (do (seterr! st i) -2)
      (= c 93) (do (seterr! st i) -3)
      (= c 44) (do (seterr! st i) -4)
      (= c 58) (do (seterr! st i) -5)
      (or (= c 45) (= c 43) (= c 46) (digit? c)) (parse-number z i st)
      :else
      (let [w (ws5-len z i)]
        (if (pos? w)
          (do (when-not (or (= c 32) (= c 9) (= c 10) (= c 13)) (nonstd! st))
              (recur z (+ i w) st))
          (do (seterr! st i) -1))))))

(defn- skip-ws [^String z j]
  (loop [j (long j)]
    (let [w (ws5-len z j)] (if (pos? w) (recur (+ j w)) j))))

(defn- parse-array [^String z i st]
  (when (> (:depth (vswap! st update :depth inc)) MAX-DEPTH) (seterr! st i))
  (if (> (:depth @st) MAX-DEPTH)
    -1
    (loop [j (inc i) items []]
      (let [x (parse-val z j st)]
        (if (number? x)
          (if (= x -3)
            (do (when (seq items) (nonstd! st))
                (vswap! st update :depth dec)
                [[T-ARRAY items] (inc (long (:iErr @st)))])
            (do (when (not= x -1) (seterr! st (skip-ws z j))) -1))
          (let [[node j2] x
                x2 (parse-val z j2 st)]
            (cond
              (and (number? x2) (= x2 -4)) (recur (inc (long (:iErr @st))) (conj items node))
              (and (number? x2) (= x2 -3))
              (do (vswap! st update :depth dec)
                  [[T-ARRAY (conj items node)] (inc (long (:iErr @st)))])
              :else (do (when (not= x2 -1) (seterr! st (skip-ws z j2))) -1))))))))

(defn- parse-object [^String z i st]
  (when (> (:depth (vswap! st update :depth inc)) MAX-DEPTH) (seterr! st i))
  (if (> (:depth @st) MAX-DEPTH)
    -1
    (loop [j (inc i) items []]
      (let [x (parse-val z j st)
            ;; the label
            lab (if (number? x)
                  (if (= x -2)
                    :close
                    (let [j (+ j (ws5-len z j))]
                      (if (or (id1? (chat z j))
                              (and (= (chat z j) 92) (= (chat z (inc j)) 117) (is4hex? z (+ j 2))))
                        (let [k (loop [k (inc j)]
                                  (if (or (and (id2? (chat z k)) (zero? (ws5-len z k)))
                                          (and (= (chat z k) 92) (= (chat z (inc k)) 117) (is4hex? z (+ k 2))))
                                    (recur (inc k)) k))]
                          (nonstd! st)
                          [[T-TEXT (subs z j k)] k])
                        (do (when (not= x -1) (seterr! st (skip-ws z j))) -1))))
                  x)]
        (cond
          (= lab :close)
          (do (when (seq items) (nonstd! st))
              (vswap! st update :depth dec)
              [[T-OBJECT items] (inc (long (:iErr @st)))])
          (number? lab) -1
          :else
          (let [[knode jk] lab]
            (if-not (text-type? (ntype knode))
              (do (seterr! st j) -1)
              (let [x1 (parse-val z jk st)]
                (if-not (and (number? x1) (= x1 -5))
                  (do (when (not= x1 -1) (seterr! st (skip-ws z jk))) -1)
                  (let [jv (inc (long (:iErr @st)))
                        xv (parse-val z jv st)]
                    (if (number? xv)
                      (do (when (not= xv -1) (seterr! st (skip-ws z jv))) -1)
                      (let [[vnode je] xv
                            x2 (parse-val z je st)
                            items (conj items knode vnode)]
                        (cond
                          (and (number? x2) (= x2 -4)) (recur (inc (long (:iErr @st))) items)
                          (and (number? x2) (= x2 -2))
                          (do (vswap! st update :depth dec)
                              [[T-OBJECT items] (inc (long (:iErr @st)))])
                          :else (do (when (not= x2 -1) (seterr! st (skip-ws z je))) -1))))))))))))))

(defn parse-text
  "Parses JSON text. Returns {:node node :nonstd bool} or {:err iErr}."
  [^String s0]
  (let [k (.indexOf s0 (int 0))
        z (if (neg? k) s0 (subs s0 0 k))
        st (volatile! {:iErr 0 :nonstd false :depth 0})
        r (parse-val z 0 st)]
    (if (or (number? r) (nil? r))
      {:err (:iErr @st)}
      (let [[node end] r
            n (.length z)
            e (loop [e (long end)] (if (and (< e n) (let [c (.charAt z e)] (or (= c \space) (= c \tab) (= c \newline) (= c \return)))) (recur (inc e)) e))
            nonstd (:nonstd @st)
            [e nonstd] (if (< e n)
                         (let [w (ws5-len z e)] [(+ e w) (or nonstd (pos? w))])
                         [e nonstd])]
        (if (< e n)
          {:err 0 :trailing true}
          {:node node :nonstd nonstd})))))

;; ---------------------------------------------------------------- rendering to JSON text
(defn- control-escape [^StringBuilder sb ^long c]
  (case c
    8 (.append sb "\\b") 9 (.append sb "\\t") 10 (.append sb "\\n") 12 (.append sb "\\f") 13 (.append sb "\\r")
    (.append sb (format "\\u%04x" c))))

(defn- render-text5 [^StringBuilder sb ^String z]
  (.append sb \")
  (let [n (.length z)]
    (loop [i 0]
      (when (< i n)
        (let [c (.charAt z i)]
          (cond
            (= c \") (do (.append sb "\\\"") (recur (inc i)))
            (< (int c) 32) (do (control-escape sb (int c)) (recur (inc i)))
            (= c \\)
            (let [d (if (< (inc i) n) (.charAt z (inc i)) (char 0))]
              (case d
                \' (do (.append sb \') (recur (+ i 2)))
                \v (do (.append sb "\\u000b") (recur (+ i 2)))
                \x (do (.append sb "\\u00") (.append sb (subs z (+ i 2) (min n (+ i 4)))) (recur (+ i 4)))
                \0 (do (.append sb "\\u0000") (recur (+ i 2)))
                \return (recur (if (and (< (+ i 2) n) (= (.charAt z (+ i 2)) \newline)) (+ i 3) (+ i 2)))
                \newline (recur (+ i 2))
                (if (or (= d (char 0x2028)) (= d (char 0x2029)))
                  (recur (+ i 2))
                  (do (.append sb c) (.append sb d) (recur (+ i 2))))))
            :else (do (.append sb c) (recur (inc i))))))))
  (.append sb \"))

(defn- render-textraw [^StringBuilder sb ^String z]
  (.append sb \")
  (dotimes [i (.length z)]
    (let [c (.charAt z i)]
      (cond (= c \") (.append sb "\\\"")
            (= c \\) (.append sb "\\\\")
            (< (int c) 32) (control-escape sb (int c))
            :else (.append sb c))))
  (.append sb \"))

(defn- render-int5 [^StringBuilder sb ^String p]
  (let [neg (= (.charAt p 0) \-)
        body (if (or neg (= (.charAt p 0) \+)) (subs p 1) p)
        hex (subs body 2)
        hex (str/replace hex #"^0+" "")]
    (cond
      (> (count hex) 16) (.append sb (if neg "-9.0e999" "9.0e999"))
      :else
      (let [u (if (= hex "") 0 (Long/parseUnsignedLong hex 16))]
        (when neg (.append sb \-))
        (.append sb (Long/toUnsignedString u))))))

(defn- render-float5 [^StringBuilder sb ^String p]
  (let [n (.length p)
        [p n] (cond (= (.charAt p 0) \-) (do (.append sb \-) [(subs p 1) (dec n)])
                    (= (.charAt p 0) \+) [(subs p 1) (dec n)]
                    :else [p n])]
    (when (= (.charAt p 0) \.) (.append sb \0))
    (dotimes [k n]
      (let [c (.charAt p k)]
        (.append sb c)
        (when (and (= c \.) (or (= (inc k) n) (not (digit? (.charAt p (inc k))))))
          (.append sb \0))))))

(defn render [^StringBuilder sb node]
  (let [t (ntype node) p (npay node)]
    (case t
      0 (.append sb "null")
      1 (.append sb "true")
      2 (.append sb "false")
      3 (.append sb ^String p)
      4 (render-int5 sb p)
      5 (.append sb ^String p)
      6 (render-float5 sb p)
      7 (do (.append sb \") (.append sb ^String p) (.append sb \"))
      8 (do (.append sb \") (.append sb ^String p) (.append sb \"))
      9 (render-text5 sb p)
      10 (render-textraw sb p)
      11 (do (.append sb \[)
             (loop [i 0 cs (seq p)]
               (when cs
                 (when (pos? i) (.append sb \,))
                 (render sb (first cs))
                 (recur (inc i) (next cs))))
             (.append sb \]))
      12 (do (.append sb \{)
             (loop [i 0 cs (seq p)]
               (when cs
                 (when (pos? i) (.append sb \,))
                 (render sb (first cs))
                 (.append sb \:)
                 (render sb (second cs))
                 (recur (inc i) (nnext cs))))
             (.append sb \})))))

(defn node->text ^String [node]
  (let [sb (StringBuilder.)] (render sb node) (.toString sb)))

;; ---------------------------------------------------------------- JSONB encode / decode
(defn- put-header [^java.io.ByteArrayOutputStream bo ^long t ^long sz]
  (cond
    (<= sz 11) (.write bo (int (bit-or t (bit-shift-left sz 4))))
    (<= sz 0xff) (do (.write bo (int (bit-or t 0xc0))) (.write bo (int sz)))
    (<= sz 0xffff) (do (.write bo (int (bit-or t 0xd0))) (.write bo (int (bit-shift-right sz 8))) (.write bo (int (bit-and sz 0xff))))
    :else (do (.write bo (int (bit-or t 0xe0)))
              (dotimes [k 4] (.write bo (int (bit-and (bit-shift-right sz (- 24 (* 8 k))) 0xff)))))))

(defn encode-node [node]
  (let [t (ntype node) p (npay node)]
    (case t
      (0 1 2) (byte-array [(unchecked-byte t)])
      (3 4 5 6 7 8 9 10)
      (let [b (v/str->utf8 p) bo (java.io.ByteArrayOutputStream.)]
        (put-header bo t (alength b)) (.write bo b 0 (alength b)) (.toByteArray bo))
      (11 12)
      (let [body (java.io.ByteArrayOutputStream.)]
        (doseq [c p] (let [b (encode-node c)] (.write body b 0 (alength b))))
        (let [bo (java.io.ByteArrayOutputStream.)]
          (put-header bo t (.size body))
          (.write bo (.toByteArray body) 0 (.size body))
          (.toByteArray bo))))))

(defn- hdr
  "returns [type payload-size header-len] at offset i of b (length n), or nil when invalid."
  [^bytes b i ^long n]
  (when (< i n)
    (let [h (bit-and (aget b i) 0xff)
          t (bit-and h 0x0f)
          s (bit-shift-right h 4)]
      (cond
        (<= s 11) [t s 1]
        :else
        (let [nb (case (long s) 12 1 13 2 14 4 15 8)]
          (when (<= (+ i 1 nb) n)
            (let [sz (loop [k 0 acc 0] (if (< k nb) (recur (inc k) (+ (* acc 256) (bit-and (aget b (+ i 1 k)) 0xff))) acc))]
              [t sz (+ 1 nb)])))))))

(defn blob-header-ok?
  "jsonArgIsJsonb's check: first byte type <= 12 and the declared size matches the blob length."
  [^bytes b]
  (let [n (alength b)]
    (when (and (pos? n) (<= (bit-and (aget b 0) 0x0f) 12))
      (let [[t sz hl] (hdr b 0 n)]
        (and t (= (+ hl sz) n))))))

(defn- valid-int-text? [^String s]
  (boolean (re-matches #"-?(0|[1-9][0-9]*)" s)))
(defn- valid-float-text? [^String s]
  (boolean (re-matches #"-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?" s)))

(defn decode-node
  "Decodes JSONB at offset i; returns [node next-offset] or nil if invalid."
  [^bytes b i ^long limit]
  (when-let [[t sz hl] (hdr b i limit)]
    (let [t (long t) sz (long sz) hl (long hl)
          end (+ i hl sz)]
      (when (<= end limit)
        (let [pay (fn [] (v/utf8->str (java.util.Arrays/copyOfRange b (int (+ i hl)) (int end))))]
          (case t
            (0 1 2) (when (zero? sz) [[t nil] end])
            3 (let [s (pay)] (when (valid-int-text? s) [[t s] end]))
            4 (let [s (pay)] (when (re-matches #"[-+]?0[xX][0-9a-fA-F]+" s) [[t s] end]))
            5 (let [s (pay)] (when (valid-float-text? s) [[t s] end]))
            6 (let [s (pay)] (when (and (pos? sz) (re-matches #"[-+]?([0-9]*\.?[0-9]*)([eE][+-]?[0-9]+)?" s)) [[t s] end]))
            (7 8 9 10) [[t (pay)] end]
            11 (loop [off (+ i hl) items []]
                 (if (= off end)
                   [[t items] end]
                   (when-let [[c nx] (decode-node b off end)]
                     (recur nx (conj items c)))))
            12 (loop [off (+ i hl) items []]
                 (if (= off end)
                   [[t items] end]
                   (when-let [[k nx] (decode-node b off end)]
                     (when (text-type? (ntype k))
                       (when-let [[val nx2] (decode-node b nx end)]
                         (recur nx2 (conj items k val)))))))
            nil))))))

(defn decode-blob [^bytes b]
  (let [r (decode-node b 0 (alength b))]
    (when (and r (= (long (second r)) (alength b))) (first r))))

;; ---------------------------------------------------------------- SQL value <-> node
(defn- unescape-text
  "Unescapes the payload of a TEXTJ/TEXT5 node into a SQL string."
  [^String z]
  (let [n (.length z) bo (java.io.ByteArrayOutputStream.)]
    (loop [i 0]
      (if (>= i n)
        nil
        (let [c (.charAt z i)]
          (if (not= c \\)
            (let [cc (Character/charCount (.codePointAt z i))
                  b (v/str->utf8 (subs z i (+ i cc)))]
              (.write bo b 0 (alength b))
              (recur (+ i cc)))
            (let [d (if (< (inc i) n) (.charAt z (inc i)) (char 0))]
              (case d
                \b (do (.write bo 8) (recur (+ i 2)))
                \f (do (.write bo 12) (recur (+ i 2)))
                \n (do (.write bo 10) (recur (+ i 2)))
                \r (do (.write bo 13) (recur (+ i 2)))
                \t (do (.write bo 9) (recur (+ i 2)))
                \v (do (.write bo 11) (recur (+ i 2)))
                \0 (do (.write bo 0) (recur (+ i 2)))
                \' (do (.write bo 39) (recur (+ i 2)))
                \" (do (.write bo 34) (recur (+ i 2)))
                \\ (do (.write bo 92) (recur (+ i 2)))
                \/ (do (.write bo 47) (recur (+ i 2)))
                \x (let [h (Integer/parseInt (subs z (+ i 2) (+ i 4)) 16)]
                     (let [b (v/str->utf8 (String. (Character/toChars h)))] (.write bo b 0 (alength b)))
                     (recur (+ i 4)))
                \u (let [h (Integer/parseInt (subs z (+ i 2) (+ i 6)) 16)]
                     (if (and (>= h 0xD800) (<= h 0xDBFF) (< (+ i 11) (inc n))
                              (= (chat z (+ i 6)) 92) (= (chat z (+ i 7)) 117) (is4hex? z (+ i 8))
                              (let [lo (Integer/parseInt (subs z (+ i 8) (+ i 12)) 16)] (and (>= lo 0xDC00) (<= lo 0xDFFF))))
                       (let [lo (Integer/parseInt (subs z (+ i 8) (+ i 12)) 16)
                             cp (+ 0x10000 (bit-shift-left (- h 0xD800) 10) (- lo 0xDC00))
                             b (v/str->utf8 (String. (Character/toChars cp)))]
                         (.write bo b 0 (alength b)) (recur (+ i 12)))
                       (do
                         ;; encode h as (possibly invalid) UTF-8 directly
                         (cond (< h 0x80) (.write bo h)
                               (< h 0x800) (do (.write bo (bit-or 0xC0 (bit-shift-right h 6))) (.write bo (bit-or 0x80 (bit-and h 0x3F))))
                               :else (do (.write bo (bit-or 0xE0 (bit-shift-right h 12)))
                                         (.write bo (bit-or 0x80 (bit-and (bit-shift-right h 6) 0x3F)))
                                         (.write bo (bit-or 0x80 (bit-and h 0x3F)))))
                         (recur (+ i 6)))))
                \return (recur (if (and (< (+ i 2) n) (= (.charAt z (+ i 2)) \newline)) (+ i 3) (+ i 2)))
                \newline (recur (+ i 2))
                (if (or (= d (char 0x2028)) (= d (char 0x2029)))
                  (recur (+ i 2))
                  (recur (+ i 2)))))))))
    (v/utf8->str (.toByteArray bo))))

(defn- parse-int64 [^String s]
  ;; returns Long or nil if not an in-range integer
  (try (Long/parseLong (if (.startsWith s "+") (subs s 1) s)) (catch NumberFormatException _ nil)))

(defn node->sql
  "jsonReturnFromBlob for scalars / text for containers (with JSON subtype)."
  [node]
  (let [t (ntype node) p (npay node)]
    (case t
      0 nil
      1 1
      2 0
      3 (or (parse-int64 p) (first (num/atof p)))
      4 (let [neg (= (.charAt ^String p 0) \-)
                body (if (or neg (= (.charAt ^String p 0) \+)) (subs p 1) p)
                hex (str/replace (subs body 2) #"^0+" "")]
            (if (> (count hex) 16)
              (if neg Double/NEGATIVE_INFINITY Double/POSITIVE_INFINITY)
              (let [u (if (= hex "") 0 (Long/parseUnsignedLong hex 16))]
                (if (neg? u)
                  (double (if neg (- (Double/parseDouble (Long/toUnsignedString u))) (Double/parseDouble (Long/toUnsignedString u))))
                  (if neg (- u) u)))))
      (5 6) (let [s (node->text node)] (first (num/atof s)))
      (7 10) p
      (8 9) (unescape-text p)
      (11 12) (mark-json (node->text node)))))

(defn- sql-text-node [^String s]
  (if (some (fn [c] (or (< (int c) 32) (= c \") (= c \\))) s)
    (let [sb (StringBuilder.)]
      (dotimes [i (.length s)]
        (let [c (.charAt s i)]
          (cond (= c \") (.append sb "\\\"")
                (= c \\) (.append sb "\\\\")
                (< (int c) 32) (control-escape sb (int c))
                :else (.append sb c))))
      [T-TEXTJ (.toString sb)])
    [T-TEXT s]))

(defn real->json-text [^double r]
  (cond (Double/isNaN r) nil
        (Double/isInfinite r) (if (pos? r) "9.0e999" "-9.0e999")
        :else (num/real->text r)))

(declare parse-json-arg)

(defn value->node
  "jsonAppendSqlValue: node for a SQL value used as a value (json_array arg, json_set value...)."
  [x]
  (cond
    (nil? x) [T-NULL nil]
    (instance? Long x) [T-INT (Long/toString (long x))]
    (instance? Double x) (let [r (double x)]
                           (cond (Double/isNaN r) [T-NULL nil]
                                 (Double/isInfinite r) [T-FLOAT (if (pos? r) "9.0e+999" "-9.0e+999")]
                                 :else [T-FLOAT (num/real->text r)]))
    (v/blob? x)
    (or (let [b (v/blob-bytes x)] (when (blob-header-ok? b) (decode-blob b)))
        (jerr "JSON cannot hold BLOB values"))
    (json-subtype? x) (let [r (parse-text x)] (if (:node r) (:node r) (jerr "malformed JSON")))
    :else (sql-text-node x)))

;; ---------------------------------------------------------------- argument parsing
(defn arg->node
  "Parses a SQL argument as JSON. nil for SQL NULL; throws 'malformed JSON'."
  [x]
  (cond
    (nil? x) nil
    (v/blob? x)
    (let [b (v/blob-bytes x)]
      (or (when (blob-header-ok? b) (decode-blob b))
          (let [r (parse-text (v/text-of x))] (or (:node r) (throw malformed)))))
    :else (let [r (parse-text (v/text-of x))] (or (:node r) (throw malformed)))))

(defn- bad-path [^String path]
  (jerr (str "bad JSON path: '" (.replace path "'" "''") "'")))

;; ---------------------------------------------------------------- path lookup / edit
(defn key-str [knode]
  (let [t (ntype knode)]
    (if (or (= t T-TEXT) (= t T-TEXTRAW)) (npay knode) (unescape-text (npay knode)))))

(declare edit)

(defn- create-sub [path pos mode val]
  (let [c (chat path pos)]
    (cond
      (>= pos (.length ^String path)) val
      (= c 46) (edit [T-OBJECT []] path pos mode val)
      (= c 91) (edit [T-ARRAY []] path pos mode val)
      :else (bad-path path))))

(defn- parse-label [^String path pos]
  ;; pos is just after the '.'; returns [label raw? next-pos]
  (if (= (chat path pos) 34)
    (let [n (.length path)
          i (loop [i (inc pos)]
              (cond (>= i n) nil
                    (= (.charAt path i) \") i
                    (and (= (.charAt path i) \\) (< (inc i) n)) (recur (+ i 2))
                    :else (recur (inc i))))]
      (if (nil? i)
        (bad-path path)
        (let [lab (subs path (inc pos) i)]
          [lab (not (.contains lab "\\")) (inc i)])))
    (let [n (.length path)
          i (loop [i pos] (if (and (< i n) (not= (.charAt path i) \.) (not= (.charAt path i) \[)) (recur (inc i)) i))]
      (when (= i pos) (bad-path path))
      [(subs path pos i) true i])))

(defn- parse-index
  "returns [k next-pos] or [nil nil] (not found) for the array step at pos ('['); throws on syntax errors"
  [^String path pos cnt]
  (let [[i k] (loop [i (inc pos) k 0]
                (if (digit? (chat path i)) (recur (inc i) (bit-and (+ (* k 10) (- (chat path i) 48)) 0xffffffff)) [i k]))]
    (if (and (>= (- i pos) 2) (= (chat path i) 93))
      [k (inc i)]
      (if (= (chat path (inc pos)) 35)
        (let [[i nn] (if (and (= (chat path (+ pos 2)) 45) (digit? (chat path (+ pos 3))))
                       (loop [i (+ pos 3) nn 0]
                         (if (digit? (chat path i)) (recur (inc i) (bit-and (+ (* nn 10) (- (chat path i) 48)) 0xffffffff)) [i nn]))
                       [(+ pos 2) 0])]
          (cond (not= (chat path i) 93) (bad-path path)
                (> nn cnt) [nil nil]
                :else [(- cnt nn) (inc i)]))
        (bad-path path)))))

(defn edit
  "Walks node along path from pos. mode :get :set :insert :replace :delete :ainsert.
  Returns ::nf (not found / nothing to do), ::del, or the new node (the found node for :get)."
  [node ^String path pos mode val]
  (let [c (chat path pos)
        n (.length path)]
    (cond
      (>= pos n)
      (case mode :get node (:set :replace) val :insert node :delete ::del :ainsert node)
      (= c 46)
      (let [[lab raw nx] (parse-label path (inc pos))]
        (when (and (= mode :ainsert) (>= nx n))
          (jerr (str "not an array element: '" (.replace path "'" "''") "'")))
        (if (not= (ntype node) T-OBJECT)
          ::nf
          (let [kids (npay node)
                lab-s (if raw lab (unescape-text lab))
                idx (loop [i 0] (cond (>= i (count kids)) nil
                                      (= (key-str (nth kids i)) lab-s) i
                                      :else (recur (+ i 2))))]
            (if idx
              (let [r (edit (nth kids (inc idx)) path nx mode val)]
                (cond (= r ::nf) ::nf
                      (= r ::del) [T-OBJECT (vec (concat (subvec kids 0 idx) (subvec kids (+ idx 2))))]
                      (= mode :get) r
                      :else [T-OBJECT (assoc kids (inc idx) r)]))
              (if (#{:set :insert} mode)
                (let [sub (create-sub path nx mode val)]
                  (if (= sub ::nf) ::nf
                      [T-OBJECT (conj kids [(if raw T-TEXTRAW T-TEXTJ) lab] sub)]))
                ::nf)))))
      (= c 91)
      (if (not= (ntype node) T-ARRAY)
        ::nf
        (let [kids (npay node)
              cnt (count kids)
              [k nx] (parse-index path pos cnt)]
          (cond
            (nil? k) ::nf
            (< k cnt)
            (if (and (= mode :ainsert) (>= nx n))
              [T-ARRAY (vec (concat (subvec kids 0 k) [val] (subvec kids k)))]
              (let [r (edit (nth kids k) path nx mode val)]
                (cond (= r ::nf) ::nf
                      (= r ::del) [T-ARRAY (vec (concat (subvec kids 0 k) (subvec kids (inc k))))]
                      (= mode :get) r
                      :else [T-ARRAY (assoc kids k r)])))
            (and (= k cnt) (#{:set :insert :ainsert} mode))
            (let [sub (create-sub path nx mode val)]
              (if (= sub ::nf) ::nf [T-ARRAY (conj kids sub)]))
            :else ::nf)))
      :else (bad-path path))))

(defn- path-arg [x] (when-not (nil? x) (v/text-of x)))

(defn lookup [root ^String path]
  (if-not (= (chat path 0) 36)
    (bad-path path)
    (let [r (edit root path 1 :get nil)] (if (= r ::nf) nil r))))

;; ---------------------------------------------------------------- results
(defn- json-result [node] (mark-json (node->text node)))
(defn- jsonb-result [node] (Blob. (encode-node node)))

(defn node-type-name [node]
  (case (long (ntype node))
    0 "null" 1 "true" 2 "false" (3 4) "integer" (5 6) "real" (7 8 9 10) "text" 11 "array" 12 "object"))

;; ---------------------------------------------------------------- functions
(defn fn-json [blob?]
  (fn [[x]]
    (when-let [node (arg->node x)]
      (if blob? (jsonb-result node) (json-result node)))))

(defn fn-array [blob?]
  (fn [args]
    (let [node [T-ARRAY (mapv value->node args)]]
      (if blob? (jsonb-result node) (json-result node)))))

(defn fn-object [name blob?]
  (fn [args]
    (when (odd? (count args)) (jerr (str name "() requires an even number of arguments")))
    (let [kids (loop [as args out []]
                 (if (empty? as) out
                     (let [k (first as)]
                       (when-not (string? k) (jerr (str name "() labels must be TEXT")))
                       (recur (nnext as) (conj out (sql-text-node k) (value->node (second as)))))))
          node [T-OBJECT kids]]
      (if blob? (jsonb-result node) (json-result node)))))

(defn fn-extract [name blob? arrow]
  (fn [args]
    (let [[x & paths] args
          root (arg->node x)]
      (when (and root (seq paths))
        (if (= 1 (count paths))
          (let [p (path-arg (first paths))]
            (when p
              (when-let [r (lookup root p)]
                (if (and blob? (#{T-ARRAY T-OBJECT} (ntype r)))
                  (jsonb-result r)
                  (node->sql r)))))
          (let [vals (loop [pl paths out []]
                       (if (empty? pl) out
                           (let [p (path-arg (first pl))]
                             (if (nil? p)
                               nil
                               (recur (rest pl) (conj out (or (lookup root p) [T-NULL nil])))))))]
            (when vals
              (let [node [T-ARRAY vals]] (if blob? (jsonb-result node) (json-result node))))))))))

(defn fn-edit [name mode blob?]
  (fn [args]
    (when (even? (count args))
      (jerr (str name "() needs an odd number of arguments")))
    (let [[x & more] args
          root (arg->node x)]
      (when root
        (loop [root root rs more]
          (if (empty? rs)
            (if blob? (jsonb-result root) (json-result root))
            (let [p (path-arg (first rs))
                  val (value->node (second rs))]
              (if (nil? p)
                (recur root (nnext rs))
                (do
                  (when-not (= (chat p 0) 36) (bad-path p))
                  (let [r (edit root p 1 mode val)]
                    (recur (if (or (= r ::nf) (= r ::del)) root r) (nnext rs))))))))))))

(defn fn-remove [name blob?]
  (fn [args]
    (let [[x & paths] args
          root (arg->node x)]
      (when root
        (loop [root root pl paths]
          (if (empty? pl)
            (if blob? (jsonb-result root) (json-result root))
            (let [p (path-arg (first pl))]
              (if (nil? p)
                nil
                (do
                  (when-not (= (chat p 0) 36) (bad-path p))
                  (let [r (edit root p 1 :delete nil)]
                    (cond (= r ::nf) (recur root (rest pl))
                          (= r ::del) nil
                          :else (recur r (rest pl)))))))))))))

(defn fn-type [[x p :as args]]
  (let [root (arg->node x)]
    (when root
      (let [r (if (= 1 (count args)) root (when-not (nil? p) (lookup root (v/text-of p))))]
        (when r (node-type-name r))))))

(defn fn-array-length [args]
  (let [root (arg->node (first args))]
    (when root
      (let [r (if (= 1 (count args)) root (let [p (path-arg (second args))] (when p (lookup root p))))]
        (when r (if (= (ntype r) T-ARRAY) (long (count (npay r))) 0))))))

(defn fn-valid [[x flags-arg :as args]]
  (let [flags (if (= 2 (count args))
                (let [f (v/int-value flags-arg)]
                  (when (or (< f 1) (> f 15)) (jerr "FLAGS parameter to json_valid() must be between 1 and 15"))
                  f)
                1)]
    (cond
      (nil? x) nil
      :else
      (let [bin (when (v/blob? x) (let [b (v/blob-bytes x)] (when (blob-header-ok? b) b)))]
        (cond
          (and bin (pos? (bit-and flags 4))) 1
          (and bin (pos? (bit-and flags 8))) (if (decode-blob bin) 1 0)
          :else
          (let [r (parse-text (v/text-of x))]
            (cond
              (nil? (:node r)) 0
              (pos? (bit-and flags 2)) 1
              (and (pos? (bit-and flags 1)) (not (:nonstd r))) 1
              :else 0)))))))

(defn fn-error-position [[x]]
  (cond
    (nil? x) nil
    :else
    (let [bin (when (v/blob? x) (let [b (v/blob-bytes x)] (when (blob-header-ok? b) b)))]
      (if bin
        (if (decode-blob bin) 0 1)
        (let [s (v/text-of x)
              r (parse-text s)]
          (if (:node r)
            0
            (let [z (let [i (.indexOf ^String s (int 0))] (if (neg? i) s (subs s 0 i)))
                  e (min (long (:err r)) (.length ^String z))]
              (long (inc (.codePointCount ^String z 0 (int e)))))))))))

(defn fn-quote [[x]]
  (json-result (value->node x)))

;; ---------------------------------------------------------------- json_patch
(defn- merge-patch [target patch]
  (if (not= (ntype patch) T-OBJECT)
    patch
    (let [tkids (if (= (ntype target) T-OBJECT) (npay target) [])]
      (loop [kids tkids pl (seq (npay patch))]
        (if (nil? pl)
          [T-OBJECT kids]
          (let [pk (first pl) pv (second pl)
                pks (key-str pk)
                idx (loop [i 0] (cond (>= i (count kids)) nil (= (key-str (nth kids i)) pks) i :else (recur (+ i 2))))]
            (cond
              (= (ntype pv) T-NULL)
              (recur (if idx (vec (concat (subvec kids 0 idx) (subvec kids (+ idx 2)))) kids) (nnext pl))
              idx (recur (assoc kids (inc idx) (merge-patch (nth kids (inc idx)) pv)) (nnext pl))
              :else (recur (conj kids pk (merge-patch [T-OBJECT []] pv)) (nnext pl)))))))))

(defn fn-patch [blob?]
  (fn [[a b]]
    (let [t (arg->node a) p (arg->node b)]
      (when (and t p)
        (let [r (merge-patch t p)] (if blob? (jsonb-result r) (json-result r)))))))

;; ---------------------------------------------------------------- json_pretty
(defn- pretty [^StringBuilder sb node ^String indent level]
  (let [t (ntype node)]
    (case (long t)
      11 (if (empty? (npay node))
           (.append sb "[]")
           (do (.append sb "[\n")
               (loop [cs (seq (npay node)) first? true]
                 (when cs
                   (when-not first? (.append sb ",\n"))
                   (dotimes [_ (inc level)] (.append sb indent))
                   (pretty sb (first cs) indent (inc level))
                   (recur (next cs) false)))
               (.append sb "\n")
               (dotimes [_ level] (.append sb indent))
               (.append sb "]")))
      12 (if (empty? (npay node))
           (.append sb "{}")
           (do (.append sb "{\n")
               (loop [cs (seq (npay node)) first? true]
                 (when cs
                   (when-not first? (.append sb ",\n"))
                   (dotimes [_ (inc level)] (.append sb indent))
                   (render sb (first cs))
                   (.append sb ": ")
                   (pretty sb (second cs) indent (inc level))
                   (recur (nnext cs) false)))
               (.append sb "\n")
               (dotimes [_ level] (.append sb indent))
               (.append sb "}")))
      (render sb node))))

(defn fn-pretty [args]
  (let [root (arg->node (first args))]
    (when root
      (let [indent (if (and (= 2 (count args)) (not (nil? (second args)))) (v/text-of (second args)) "    ")
            sb (StringBuilder.)]
        (pretty sb root indent 0)
        (str sb)))))

(defn fn-subtype [[x]] (long (subtype-of x)))

(defn make-group-acc
  "json_group_array / json_group_object accumulators: (acc x y) steps, (acc) finalizes."
  [name]
  (let [obj? (.endsWith ^String name "object")
        blob? (.startsWith ^String name "jsonb")
        items (java.util.ArrayList.)]
    (fn
      ([] (let [node [(if obj? T-OBJECT T-ARRAY) (vec items)]]
            (if blob? (jsonb-result node) (json-result node))))
      ([x y]
       (if obj?
         (do (when-not (string? x) (when-not (nil? x) nil))
             (.add items (sql-text-node (v/text-of x)))
             (.add items (value->node y)))
         (.add items (value->node x)))))))

(defn- hlen [payload] (cond (<= payload 11) 1 (<= payload 0xff) 2 (<= payload 0xffff) 3 :else 5))
(defn- nsize [node]
  (let [t (ntype node) p (npay node)]
    (if (< t 11)
      (if (<= t 2) 1 (let [n (alength ^bytes (v/str->utf8 p))] (+ (hlen n) n)))
      (let [n (reduce + 0 (map nsize p))] (+ (hlen n) n)))))

(defn locate
  "[node offset key] of the node at path (offsets as in the minimal JSONB), or nil when not found."
  [root ^String path]
  (loop [node root off 0 id 0 key nil pos 1]
    (let [c (chat path pos) n (.length path)]
      (cond
        (>= pos n) [node off key id]
        (= c 46)
        (let [[lab raw nx] (parse-label path (inc pos))]
          (when (= (ntype node) T-OBJECT)
            (let [kids (npay node)
                  lab-s (if raw lab (unescape-text lab))
                  idx (loop [i 0] (cond (>= i (count kids)) nil (= (key-str (nth kids i)) lab-s) i :else (recur (+ i 2))))]
              (when idx
                (let [hl (hlen (reduce + 0 (map nsize kids)))
                      before (reduce + 0 (map nsize (subvec kids 0 (inc idx))))
                      before-id (reduce + 0 (map nsize (subvec kids 0 idx)))]
                  (recur (nth kids (inc idx)) (+ off hl before) (+ off hl before-id) lab-s nx))))))
        (= c 91)
        (when (= (ntype node) T-ARRAY)
          (let [kids (npay node) [k nx] (parse-index path pos (count kids))]
            (when (and k (< k (count kids)))
              (let [hl (hlen (reduce + 0 (map nsize kids)))
                    before (reduce + 0 (map nsize (subvec kids 0 k)))]
                (recur (nth kids k) (+ off hl before) (+ off hl before) k nx)))))
        :else (bad-path path)))))

(defn parent-path
  "the path with its last component removed ('$' stays '$')"
  [^String path]
  (let [n (.length path)]
    (loop [i (dec n)]
      (cond (<= i 0) "$"
            (and (= (.charAt path i) \[) ) (subs path 0 i)
            (and (= (.charAt path i) \.) ) (subs path 0 i)
            :else (recur (dec i))))))

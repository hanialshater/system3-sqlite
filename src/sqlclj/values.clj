(ns sqlclj.values
  "SQLite values: NULL (nil), INTEGER (Long), REAL (Double), TEXT (String; bytes that are not valid UTF-8
  are kept as U+DC80..U+DCFF escapes), BLOB (Blob). Comparison, affinity, conversions, arithmetic."
  (:require [sqlclj.num :as num]
            [sqlclj.lexer :as lx]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(deftype Blob [^bytes b]
  Object
  (equals [this o] (and (instance? Blob o) (java.util.Arrays/equals b ^bytes (.-b ^Blob o))))
  (hashCode [this] (java.util.Arrays/hashCode b))
  (toString [this] (str "X'" (apply str (map #(format "%02X" (bit-and (int %) 0xff)) b)) "'")))

(def subtypes
  "identity registry of String instances carrying the JSON subtype (a subtype is lost when a value is stored)"
  (java.util.IdentityHashMap.))
(defn strip-subtype
  "a fresh String instance if x carries a JSON subtype (so the subtype is dropped), else x"
  [x]
  (if (and (string? x) (pos? (.size subtypes)) (.containsKey subtypes x)) (String. ^String x) x))
(defn strip-row [row]
  (if (and (pos? (.size subtypes)) (some #(and (string? %) (.containsKey subtypes %)) row))
    (mapv strip-subtype row)
    row))

(defn blob? [v] (instance? Blob v))
(defn blob-bytes ^bytes [^Blob v] (.-b v))

(defn err [msg] (throw (lx/sql-error msg)))

;; ------------------------------------------------------------------ UTF-8 with byte escapes

(defn str->utf8 ^bytes [^String s]
  (let [n (.length s)
        out (java.io.ByteArrayOutputStream. (+ n 8))]
    (loop [i 0]
      (when (< i n)
        (let [c (int (.charAt s i))]
          (cond
            (< c 0x80) (do (.write out c) (recur (inc i)))
            (< c 0x800) (do (.write out (bit-or 0xC0 (bit-shift-right c 6)))
                            (.write out (bit-or 0x80 (bit-and c 0x3F))) (recur (inc i)))
            (and (>= c 0xD800) (<= c 0xDBFF) (< (inc i) n)
                 (let [d (int (.charAt s (inc i)))] (and (>= d 0xDC00) (<= d 0xDFFF))))
            (let [d (int (.charAt s (inc i)))
                  cp (+ 0x10000 (bit-shift-left (- c 0xD800) 10) (- d 0xDC00))]
              (.write out (bit-or 0xF0 (bit-shift-right cp 18)))
              (.write out (bit-or 0x80 (bit-and (bit-shift-right cp 12) 0x3F)))
              (.write out (bit-or 0x80 (bit-and (bit-shift-right cp 6) 0x3F)))
              (.write out (bit-or 0x80 (bit-and cp 0x3F)))
              (recur (+ i 2)))
            (and (>= c 0xDC80) (<= c 0xDCFF)) (do (.write out (- c 0xDC00)) (recur (inc i)))
            :else (do (.write out (bit-or 0xE0 (bit-shift-right c 12)))
                      (.write out (bit-or 0x80 (bit-and (bit-shift-right c 6) 0x3F)))
                      (.write out (bit-or 0x80 (bit-and c 0x3F))) (recur (inc i)))))))
    (.toByteArray out)))

(defn utf8->str ^String [^bytes b]
  (let [n (alength b)
        sb (StringBuilder. n)]
    (loop [i 0]
      (when (< i n)
        (let [c (bit-and (aget b i) 0xff)
              cont (fn [k] (and (< (+ i k) n) (= 0x80 (bit-and (aget b (+ i k)) 0xC0))))
              cb (fn [k] (bit-and (aget b (+ i k)) 0x3F))]
          (cond
            (< c 0x80) (do (.append sb (char c)) (recur (inc i)))
            (and (>= c 0xC2) (< c 0xE0) (cont 1))
            (do (.append sb (char (bit-or (bit-shift-left (bit-and c 0x1F) 6) (cb 1)))) (recur (+ i 2)))
            (and (>= c 0xE0) (< c 0xF0) (cont 1) (cont 2)
                 (let [cp (bit-or (bit-shift-left (bit-and c 0x0F) 12) (bit-shift-left (cb 1) 6) (cb 2))]
                   (and (>= cp 0x800) (not (and (>= cp 0xD800) (<= cp 0xDFFF))))))
            (do (.append sb (char (bit-or (bit-shift-left (bit-and c 0x0F) 12) (bit-shift-left (cb 1) 6) (cb 2))))
                (recur (+ i 3)))
            (and (>= c 0xF0) (< c 0xF5) (cont 1) (cont 2) (cont 3)
                 (let [cp (bit-or (bit-shift-left (bit-and c 0x07) 18) (bit-shift-left (cb 1) 12) (bit-shift-left (cb 2) 6) (cb 3))]
                   (and (>= cp 0x10000) (<= cp 0x10FFFF))))
            (let [cp (bit-or (bit-shift-left (bit-and c 0x07) 18) (bit-shift-left (cb 1) 12) (bit-shift-left (cb 2) 6) (cb 3))]
              (.appendCodePoint sb cp) (recur (+ i 4)))
            :else (do (.append sb (char (+ 0xDC00 c))) (recur (inc i)))))))
    (.toString sb)))

;; ------------------------------------------------------------------ conversions to text / numbers

;; database text encoding: :utf8 :utf16le :utf16be (set by the driver from PRAGMA encoding)
(def enc (volatile! :utf8))

(defn- utf16-bytes ^bytes [^String s be?]
  (let [n (.length s) out (java.io.ByteArrayOutputStream. (* 2 n))]
    (dotimes [i n]
      (let [c (int (.charAt s i))
            ;; escaped raw bytes (lone surrogates 0xDC80..0xDCFF) are written as plain bytes' code units
            c (if (and (>= c 0xDC80) (<= c 0xDCFF)) (- c 0xDC00) c)]
        (if be?
          (do (.write out (bit-shift-right c 8)) (.write out (bit-and c 0xff)))
          (do (.write out (bit-and c 0xff)) (.write out (bit-shift-right c 8))))))
    (.toByteArray out)))

(defn- utf16-str ^String [^bytes b be?]
  (let [n (bit-and (alength b) (bit-not 1)) sb (StringBuilder. (quot n 2))]
    (loop [i 0]
      (when (< i n)
        (let [x (bit-and (aget b i) 0xff) y (bit-and (aget b (inc i)) 0xff)]
          (.append sb (char (if be? (bit-or (bit-shift-left x 8) y) (bit-or (bit-shift-left y 8) x))))
          (recur (+ i 2)))))
    (.toString sb)))

(defn text-of
  "sqlite3_value_text: nil for NULL."
  ^String [v]
  (cond (nil? v) nil
        (string? v) v
        (instance? Long v) (Long/toString (long v))
        (instance? Double v) (num/real->text (double v))
        (blob? v) (case @enc :utf8 (utf8->str (blob-bytes v)) :utf16le (utf16-str (blob-bytes v) false) (utf16-str (blob-bytes v) true))
        :else (str v)))

(defn bytes-of
  "sqlite3_value_blob."
  ^bytes [v]
  (cond (nil? v) nil
        (blob? v) (blob-bytes v)
        :else (let [t (text-of v)] (case @enc :utf8 (str->utf8 t) :utf16le (utf16-bytes t false) (utf16-bytes t true)))))

(defn real-same-as-int [^double r ^long i]
  (let [r2 (double i)]
    (or (== r 0.0)
        (and (= (Double/doubleToRawLongBits r) (Double/doubleToRawLongBits r2))
             (>= i -2251799813685248) (< i 2251799813685248)))))

(defn real-to-i64 ^long [^double r]
  (cond (< r -9223372036854774784.0) Long/MIN_VALUE
        (> r 9223372036854774784.0) Long/MAX_VALUE
        :else (long r)))

(defn double->int64
  "doubleToInt64 (saturating)."
  ^long [^double r]
  (cond (Double/isNaN r) 0
        (<= r -9.223372036854775808E18) Long/MIN_VALUE
        (>= r 9.223372036854775807E18) Long/MAX_VALUE
        :else (long r)))

(defn integer-affinity
  "sqlite3VdbeIntegerAffinity on a double: Long if integral and in range, else the double."
  [^double r]
  (let [ix (double->int64 r)]
    (if (and (== r (double ix)) (> ix Long/MIN_VALUE) (< ix Long/MAX_VALUE))
      ix
      r)))

(defn apply-numeric-text
  "applyNumericAffinity on a string: returns Long/Double if it looks numeric, else the string."
  [^String s try-int]
  (let [[r rc] (num/atof s)
        r (double r) rc (long rc)]
    (if (<= rc 0)
      s
      (let [as-int (when (= rc 1)
                     (let [iv (real-to-i64 r)]
                       (if (real-same-as-int r iv)
                         iv
                         (let [[v c] (num/atoi64 s)] (when (= 0 (long c)) v)))))]
        (cond as-int as-int
              try-int (integer-affinity r)
              :else r)))))

(defn real-value
  "sqlite3VdbeRealValue."
  ^double [v]
  (cond (nil? v) 0.0
        (instance? Double v) (double v)
        (instance? Long v) (double (long v))
        :else (double (first (num/atof (text-of v))))))

(defn int-value
  "sqlite3VdbeIntValue."
  ^long [v]
  (cond (nil? v) 0
        (instance? Long v) (long v)
        (instance? Double v) (double->int64 (double v))
        :else (long (first (num/atoi64 (text-of v))))))

(defn bool-value
  "sqlite3VdbeBoolValue for non-NULL v."
  [v]
  (if (instance? Long v) (not= 0 (long v)) (not= 0.0 (real-value v))))

(defn numeric-value
  "computeNumericType: the number used for arithmetic (Long or Double); v non-NULL."
  [v]
  (cond (instance? Long v) v
        (instance? Double v) v
        :else
        (let [s (text-of v)
              [r rc] (num/atof s)
              rc (long rc)]
          (cond
            (<= rc 0) (let [[iv c] (num/atoi64 s)]
                        (if (and (= rc 0) (<= (long c) 1)) iv (double r)))
            (= rc 1) (let [[iv c] (num/atoi64 s)] (if (= 0 (long c)) iv (double r)))
            :else (double r)))))

(defn numeric-type-value
  "sqlite3_value_numeric_type conversion: text that looks numeric becomes a number; others unchanged."
  [v]
  (if (string? v) (apply-numeric-text v false) v))

;; ------------------------------------------------------------------ affinity

(defn type-affinity
  "sqlite3AffinityType: :integer :text :blob :real :numeric."
  [t]
  (if (or (nil? t) (= "" t))
    :blob
    (let [u (str/upper-case t)
          n (count u)]
      (loop [i 0 aff :numeric]
        (if (>= i n)
          aff
          (let [rest (subs u i)]
            (cond
              (str/starts-with? rest "INT") :integer
              (or (str/starts-with? rest "CHAR") (str/starts-with? rest "CLOB") (str/starts-with? rest "TEXT"))
              (recur (inc i) :text)
              (and (str/starts-with? rest "BLOB") (#{:numeric :real} aff)) (recur (inc i) :blob)
              (and (or (str/starts-with? rest "REAL") (str/starts-with? rest "FLOA") (str/starts-with? rest "DOUB"))
                   (= aff :numeric))
              (recur (inc i) :real)
              :else (recur (inc i) aff))))))))

(defn numeric-aff? [a] (#{:numeric :integer :real} a))

(defn apply-affinity
  "Affinity applied when storing a value into a column (or OP_Affinity)."
  [v aff]
  (case aff
    (:integer :numeric)
    (cond (string? v) (apply-numeric-text v true)
          (instance? Double v) (integer-affinity (double v))
          :else v)
    :real
    (cond (string? v) (let [x (apply-numeric-text v true)] (if (instance? Long x) (double (long x)) x))
          (instance? Long v) (double (long v))
          :else v)
    :text
    (cond (instance? Long v) (text-of v)
          (instance? Double v) (text-of v)
          :else v)
    v))

;; ------------------------------------------------------------------ comparison

(defn- type-rank ^long [v]
  (cond (nil? v) 0 (instance? Long v) 1 (instance? Double v) 1 (string? v) 2 :else 3))

(defn int-float-cmp ^long [^long i ^double r]
  (cond (< r -9.223372036854775808E18) 1
        (>= r 9.223372036854775808E18) -1
        :else (let [y (long r)]
                (cond (< i y) -1 (> i y) 1
                      :else (let [s (double i)] (cond (< s r) -1 (> s r) 1 :else 0))))))

(defn num-cmp ^long [a b]
  (if (instance? Long a)
    (if (instance? Long b)
      (Long/compare (long a) (long b))
      (int-float-cmp (long a) (double b)))
    (if (instance? Long b)
      (- (int-float-cmp (long b) (double a)))
      (let [x (double a) y (double b)] (cond (< x y) -1 (> x y) 1 :else 0)))))

(defn- blob-cmp* ^long [^bytes a ^bytes b]
  (let [c (java.util.Arrays/compareUnsigned a b)]
    (cond (neg? c) -1 (pos? c) 1 :else 0)))

(defn binary-cmp ^long [^String a ^String b]
  (let [na (.length a) nb (.length b)]
    (loop [i 0]
      (if (or (>= i na) (>= i nb))
        (Long/compare na nb)
        (let [ca (.charAt a i) cb (.charAt b i)]
          (if (= ca cb)
            (recur (inc i))
            ;; compare as code points (UTF-8 byte order)
            (if (or (and (>= (int ca) 0xD800) (<= (int ca) 0xDFFF)) (and (>= (int cb) 0xD800) (<= (int cb) 0xDFFF)))
              (blob-cmp* (str->utf8 (subs a i)) (str->utf8 (subs b i)))
            (let [pa (.codePointAt a i) pb (.codePointAt b i)
                  ;; escaped raw bytes sort as their byte value relative to multibyte lead bytes
                  ka (if (and (>= pa 0xDC80) (<= pa 0xDCFF)) (+ 0x110000 (- pa 0xDC00)) pa)
                  kb (if (and (>= pb 0xDC80) (<= pb 0xDCFF)) (+ 0x110000 (- pb 0xDC00)) pb)]
              (Long/compare ka kb)))))))))

(defn- lower-ascii ^long [^long c] (if (and (>= c 65) (<= c 90)) (+ c 32) c))

(defn- needs-bytes? [^String s]
  (let [n (.length s)]
    (loop [i 0] (if (>= i n) false
                    (let [c (int (.charAt s i))]
                      (if (or (zero? c) (and (>= c 0xD800) (<= c 0xDFFF))) true (recur (inc i))))))))

(defn- nocase-cmp-bytes
  "nocaseCollatingFunc: sqlite3StrNICmp over the common length (stopping at a NUL), then length difference."
  ^long [^bytes a ^bytes b]
  (let [la (alength a) lb (alength b) n (min la lb)
        lw (fn ^long [x] (let [x (bit-and (long x) 0xff)] (if (and (>= x 65) (<= x 90)) (+ x 32) x)))
        i (loop [i 0] (if (and (< i n) (not (zero? (aget a i))) (= (lw (aget a i)) (lw (aget b i)))) (recur (inc i)) i))
        r (if (= i n) 0 (- (lw (aget a i)) (lw (aget b i))))
        r (if (zero? r) (- la lb) r)]
    (cond (neg? r) -1 (pos? r) 1 :else 0)))

(declare nocase-cmp*)

(defn nocase-cmp ^long [^String a ^String b]
  (if (or (needs-bytes? a) (needs-bytes? b))
    (nocase-cmp-bytes (str->utf8 a) (str->utf8 b))
    (nocase-cmp* a b)))

(defn- nocase-cmp* ^long [^String a ^String b]
  (let [na (.length a) nb (.length b)]
    (loop [i 0]
      (if (or (>= i na) (>= i nb))
        (Long/compare na nb)
        (let [ca (lower-ascii (long (int (.charAt a i)))) cb (lower-ascii (long (int (.charAt b i))))]
          (cond (= ca cb) (recur (inc i))
                (and (< ca 128) (< cb 128)) (Long/compare ca cb)
                :else (binary-cmp (subs a i) (subs b i))))))))

(defn- rtrim-sp ^String [^String s]
  (loop [k (.length s)] (if (and (> k 0) (= (.charAt s (dec k)) \space)) (recur (dec k)) (subs s 0 k))))

(defn text-cmp ^long [coll ^String a ^String b]
  (case coll
    "NOCASE" (nocase-cmp a b)
    "RTRIM" (binary-cmp (rtrim-sp a) (rtrim-sp b))
    (binary-cmp a b)))

(defn blob-cmp ^long [^bytes a ^bytes b]
  (let [c (java.util.Arrays/compareUnsigned a b)]
    (cond (neg? c) -1 (pos? c) 1 :else 0)))

(defn sql-cmp
  "sqlite3MemCompare with a collation name (upper-case or nil)."
  ^long [a b coll]
  (let [ra (type-rank a) rb (type-rank b)]
    (if (not= ra rb)
      (Long/compare ra rb)
      (case ra
        0 0
        1 (num-cmp a b)
        2 (text-cmp coll a b)
        3 (blob-cmp (blob-bytes a) (blob-bytes b))))))

(defn comparison-coerce
  "Applies comparison affinity to operands; returns [a b]."
  [a b aff]
  (cond
    (numeric-aff? aff)
    [(if (string? a) (apply-numeric-text a false) a)
     (if (string? b) (apply-numeric-text b false) b)]
    (= aff :text)
    (if (or (string? a) (string? b))
      [(if (or (instance? Long a) (instance? Double a)) (text-of a) a)
       (if (or (instance? Long b) (instance? Double b)) (text-of b) b)]
      [a b])
    :else [a b]))

(defn compare-affinity
  "sqlite3CompareAffinity for two expression affinities (nil = no affinity)."
  [a1 a2]
  (cond
    (and a1 a2) (if (or (numeric-aff? a1) (numeric-aff? a2)) :numeric :blob)
    a1 a1
    a2 a2
    :else nil))

;; ------------------------------------------------------------------ arithmetic

(defn- fp-result [^double r] (if (Double/isNaN r) nil r))

(defn arith [op a b]
  (if (or (nil? a) (nil? b))
    nil
    (let [x (numeric-value a) y (numeric-value b)]
      (if (and (instance? Long x) (instance? Long y))
        (let [x (long x) y (long y)]
          (case op
            :add (let [r (unchecked-add x y)]
                   (if (and (= (neg? x) (neg? y)) (not= (neg? r) (neg? x))) (fp-result (+ (double x) (double y))) r))
            :sub (let [r (unchecked-subtract x y)]
                   (if (and (not= (neg? x) (neg? y)) (not= (neg? r) (neg? x))) (fp-result (- (double x) (double y))) r))
            :mul (let [hi (Math/multiplyHigh x y) lo (unchecked-multiply x y)]
                   (if (or (and (= hi 0) (>= lo 0)) (and (= hi -1) (< lo 0)))
                     lo
                     (fp-result (* (double x) (double y)))))
            :div (cond (= y 0) nil
                       (and (= y -1) (= x Long/MIN_VALUE)) (fp-result (/ (double x) (double y)))
                       :else (quot x y))
            :rem (cond (= y 0) nil
                       (= y -1) 0
                       :else (rem x y))))
        (case op
          :add (fp-result (+ (real-value a) (real-value b)))
          :sub (fp-result (- (real-value a) (real-value b)))
          :mul (fp-result (* (real-value a) (real-value b)))
          :div (let [rb (real-value b)] (if (== rb 0.0) nil (fp-result (/ (real-value a) rb))))
          :rem (let [ia (int-value b) ib (int-value a)]
                 (cond (= ia 0) nil
                       :else (double (rem ib (if (= ia -1) 1 ia))))))))))

(defn bitop [op a b]
  (if (or (nil? a) (nil? b))
    nil
    (let [x (int-value a) y (int-value b)]
      (case op
        :bitand (bit-and x y)
        :bitor (bit-or x y)
        (:shl :shr)
        (let [[op y] (if (neg? y) [(if (= op :shl) :shr :shl) (if (> y -64) (- y) 64)] [op y])
              y (long y)]
          (cond
            (= y 0) x
            (>= y 64) (if (or (>= x 0) (= op :shl)) 0 -1)
            (= op :shl) (bit-shift-left x y)
            :else (bit-shift-right x y)))))))

(defn- approx-len ^long [x]
  (cond (string? x) (.length ^String x) (blob? x) (alength ^bytes (blob-bytes x)) :else 24))

(defn concat-values [a b]
  (if (or (nil? a) (nil? b))
    nil
    (do (when (> (+ (approx-len a) (approx-len b)) 1000000000) (err "string or blob too big"))
        (str (text-of a) (text-of b)))))

(defn type-name [v]
  (cond (nil? v) "null" (instance? Long v) "integer" (instance? Double v) "real" (string? v) "text" :else "blob"))

;; ------------------------------------------------------------------ casts

(defn cast-value [v aff]
  (if (nil? v)
    nil
    (case aff
      :blob (if (blob? v) v (Blob. (bytes-of v)))
      :text (text-of v)
      :real (cond (instance? Double v) v
                  (instance? Long v) (double (long v))
                  :else (double (first (num/atof (text-of v)))))
      :integer (cond (instance? Long v) v
                     (instance? Double v) (double->int64 (double v))
                     :else (long (first (num/atoi64 (text-of v)))))
      :numeric (cond (instance? Long v) v
                     (instance? Double v) v
                     :else
                     (let [s (text-of v)
                           [r rc] (num/atof s)
                           r (double r) rc (long rc)
                           [iv ic] (num/atoi64 s)]
                       (cond
                         (and (or (= rc 0) (= rc 1)) (<= (long ic) 1)) iv
                         :else (let [ix (real-to-i64 r)]
                                 (if (real-same-as-int r ix) ix r))))))))

;; ------------------------------------------------------------------ output

(defn hex-upper ^String [^bytes b]
  (let [sb (StringBuilder. (* 2 (alength b)))
        digits "0123456789ABCDEF"]
    (doseq [x b]
      (let [x (bit-and (int x) 0xff)]
        (.append sb (.charAt digits (bit-shift-right x 4)))
        (.append sb (.charAt digits (bit-and x 15)))))
    (.toString sb)))

(defn display
  "Value as printed by the driver."
  ^String [v]
  (cond (nil? v) "NULL"
        (blob? v) (str "X'" (hex-upper (blob-bytes v)) "'")
        :else (let [s (text-of v)
                    k (.indexOf ^String s (int 0))]
                ;; the driver prints with fputs, which stops at a NUL byte
                (if (neg? k) s (subs s 0 k)))))

;; ------------------------------------------------------------------ LIKE / GLOB

(defn- cps
  "Characters decoded like sqlite3Utf8Read from the UTF-8 bytes of s, up to the first NUL, 0-terminated."
  ^ints [^String s]
  (let [^bytes b (str->utf8 s)
        n (alength b)
        out (java.util.ArrayList.)]
    (loop [i 0]
      (when (and (< i n) (not (zero? (aget b i))))
        (let [c0 (bit-and (aget b i) 0xff)]
          (if (< c0 0xc0)
            (do (.add out (int c0)) (recur (inc i)))
            (let [t1 (cond (< c0 0xe0) (bit-and c0 0x1f) (< c0 0xf0) (bit-and c0 0x0f) (< c0 0xf8) (bit-and c0 0x07)
                           (< c0 0xfc) (bit-and c0 0x03) (< c0 0xfe) (bit-and c0 0x01) :else 0)
                  [c j] (loop [j (inc i) c (long t1)]
                          (if (and (< j n) (= 0x80 (bit-and (aget b j) 0xc0)))
                            (recur (inc j) (bit-and 0xffffffff (+ (bit-shift-left c 6) (bit-and (aget b j) 0x3f))))
                            [c j]))
                  c (long c)
                  c (if (or (< c 0x80) (= 0xD800 (bit-and c 0xFFFFF800)) (= 0xFFFE (bit-and c 0xFFFFFFFE))) 0xFFFD c)]
              (.add out (int (unchecked-int c)))
              (recur (long j)))))))
    (let [a (int-array (inc (.size out)))]
      (dotimes [k (.size out)] (aset a k (int (.get out k))))
      a)))

(defn- pattern-compare
  "Port of patternCompare. Returns 0 match, 1 no match, 2 no wildcard match."
  [^ints pat pi ^ints s si glob? no-case match-other]
  (let [match-all (if glob? 42 37) match-one (if glob? 63 95) match-other (long match-other)]
    (loop [pi (long pi) si (long si) escaped -1]
      (let [c (aget pat pi)]
        (if (zero? c)
          (if (zero? (aget s si)) 0 1)
          (let [pi (inc pi)]
            (cond
              (= c match-all)
              (let [res (loop [pi pi si si]
                          (let [c (aget pat pi)]
                            (cond
                              (= c match-all) (recur (inc pi) si)
                              (= c match-one) (if (zero? (aget s si)) [:ret 2] (recur (inc pi) (inc si)))
                              :else [pi si])))]
                (if (= :ret (first res))
                  (second res)
                  (let [[pi si] res pi (long pi) si (long si)
                        c (aget pat pi)]
                    (cond
                      (zero? c) 0
                      (and (= c match-other) glob?)
                      (loop [si si]
                        (if (zero? (aget s si))
                          2
                          (let [b (pattern-compare pat pi s si glob? no-case match-other)]
                            (if (not= b 1) b (recur (inc si))))))
                      :else
                      (let [esc? (and (= c match-other) (not glob?))
                            [c pi] (if esc?
                                     [(aget pat (inc pi)) (+ pi 2)]
                                     [c (inc pi)])
                            c (long c) pi (long pi)]
                        (if (and esc? (zero? c))
                          2
                          (loop [si si]
                            (let [c2 (aget s si)]
                              (if (zero? c2)
                                2
                                (let [si (inc si)]
                                  (if (or (= c2 c) (and no-case (< c 128) (< c2 128) (= (lower-ascii c) (lower-ascii c2))))
                                    (let [b (pattern-compare pat pi s si glob? no-case match-other)]
                                      (if (not= b 1) b (recur si)))
                                    (recur si))))))))))))
              (and (= c match-other) (not glob?))
              (let [c (aget pat pi)]
                (if (zero? c)
                  1
                  (let [pi (inc pi) c2 (aget s si)]
                    (if (or (= c c2) (and no-case (< c 128) (< c2 128) (not= c2 0) (= (lower-ascii c) (lower-ascii c2))))
                      (recur pi (inc si) pi)
                      1))))
              (and (= c match-other) glob?)
              (let [c (aget s si)]
                (if (zero? c)
                  1
                  (let [si (inc si)
                        c2 (aget pat pi) pi (inc pi)
                        [invert c2 pi] (if (= c2 94) [true (aget pat pi) (inc pi)] [false c2 pi])
                        [seen c2 pi] (if (= c2 93) [(= c 93) (aget pat pi) (inc pi)] [false c2 pi])
                        [seen c2 pi] (loop [seen seen c2 (long c2) pi (long pi) prior 0]
                                       (if (and (not= c2 0) (not= c2 93))
                                         (if (and (= c2 45) (not= (aget pat pi) 93) (not= (aget pat pi) 0) (> prior 0))
                                           (let [c3 (aget pat pi) pi (inc pi)
                                                 seen (or seen (and (>= c prior) (<= c c3)))]
                                             (recur seen (aget pat pi) (inc pi) 0))
                                           (recur (or seen (= c c2)) (aget pat pi) (inc pi) c2))
                                         [seen c2 pi]))]
                    (if (or (zero? (long c2)) (= seen invert))
                      1
                      (recur (long pi) si escaped)))))
              :else
              (let [c2 (aget s si)]
                (cond
                  (= c c2) (recur pi (inc si) escaped)
                  (and no-case (< c 128) (< c2 128) (= (lower-ascii c) (lower-ascii c2))) (recur pi (inc si) escaped)
                  (and (= c match-one) (not= pi escaped) (not= c2 0)) (recur pi (inc si) escaped)
                  :else 1)))))))))

(defn- retarget-escape
  "SQLite switches off the % or _ wildcard when it is the ESCAPE character: rewrite the pattern
  with a private escape character so that the compare sees ordinary wildcards."
  ^ints [^ints pat esc]
  (let [E 0x10FFFF
        out (java.util.ArrayList.)
        n (alength pat)]
    (loop [i 0]
      (when (and (< i n) (not (zero? (aget pat i))))
        (let [c (aget pat i)]
          (cond (= c esc) (do (.add out E)
                              (if (and (< (inc i) n) (not (zero? (aget pat (inc i)))))
                                (do (.add out (aget pat (inc i))) (recur (+ i 2)))
                                nil))
                (= c E) (do (.add out E) (.add out E) (recur (inc i)))
                :else (do (.add out c) (recur (inc i)))))))
    (let [a (int-array (inc (.size out)))]
      (dotimes [k (.size out)] (aset a k (int (.get out k))))
      a)))

(defn like-match
  ([pattern s esc] (like-match pattern s esc false))
  ([^String pattern ^String s esc cs]
   (let [pat (cps pattern)]
     (if (and esc (or (= (long esc) 37) (= (long esc) 95)))
       (zero? (long (pattern-compare (retarget-escape pat (int esc)) 0 (cps s) 0 false (not cs) 0x10FFFF)))
       (zero? (long (pattern-compare pat 0 (cps s) 0 false (not cs) (if esc (long esc) 0))))))))

(defn glob-match [^String pattern ^String s]
  (zero? (long (pattern-compare (cps pattern) 0 (cps s) 0 true false 91))))

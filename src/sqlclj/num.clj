(ns sqlclj.num
  "Numeric text conversions matching SQLite 3.53: text->double (sqlite3AtoF with the 96-bit power-of-ten
  conversion), sqlite3Atoi64, and the %!.17g rendering of reals used by sqlite3_column_text.")

(set! *warn-on-reflection* true)

;; ------------------------------------------------------------------ power-of-ten tables
;; Normalized (top bit set) 64-bit significands of 10^i (i = 0..26), exact.
(def ^:private ^"[J" a-base
  (long-array (map unchecked-long
                   [0x8000000000000000 0xa000000000000000 0xc800000000000000 0xfa00000000000000 0x9c40000000000000
                    0xc350000000000000 0xf424000000000000 0x9896800000000000 0xbebc200000000000 0xee6b280000000000
                    0x9502f90000000000 0xba43b74000000000 0xe8d4a51000000000 0x9184e72a00000000 0xb5e620f480000000
                    0xe35fa931a0000000 0x8e1bc9bf04000000 0xb1a2bc2ec5000000 0xde0b6b3a76400000 0x8ac7230489e80000
                    0xad78ebc5ac620000 0xd8d726b7177a8000 0x878678326eac9000 0xa968163f0a57b400 0xd3c21bcecceda100
                    0x84595161401484a0 0xa56fa5b99019a5c8])))
;; 10^(27q) for q = -13..12 (index q+13): truncated high 64 bits, and the next 32 bits rounded.
(def ^:private ^"[J" a-scale
  (long-array (map unchecked-long
                   [0x8049a4ac0c5811ae 0xcf42894a5dce35ea 0xa76c582338ed2621 0x873e4f75e2224e68 0xda7f5bf590966848
                    0xb080392cc4349dec 0x8e938662882af53e 0xe65829b3046b0afa 0xba121a4650e4ddeb 0x964e858c91ba2655
                    0xf2d56790ab41c2a2 0xc428d05aa4751e4c 0x9e74d1b791e07e48 0xcccccccccccccccc 0xcecb8f27f4200f3a
                    0xa70c3c40a64e6c51 0x86f0ac99b4e8dafd 0xda01ee641a708de9 0xb01ae745b101e9e4 0x8e41ade9fbebc27d
                    0xe5d3ef282a242e81 0xb9a74a0637ce2ee1 0x95f83d0a1fb69cd9 0xf24a01a73cf2dccf 0xc3b8358109e84f07
                    0x9e19db92b4e31ba9])))
(def ^:private ^"[J" a-scale-lo
  (long-array [0x205b896d 0x52064cad 0xaf2af2b8 0x5a7744a7 0xaf39a475 0xbd8d794e 0x547eb47b 0x0cb4a5a3 0x92f34d62
               0x3a6a07f9 0xfae27299 0xaa97e14c 0x775ea265 0xcccccccc 0x00000000 0x999090b6 0x69a028bb 0xe80e6f48
               0x5ec05dd0 0x14588f14 0x8f1668c9 0x6d953e2c 0x4abdaf10 0xbc633b39 0x0a862f81 0x6c07a2c2]))

(defmacro ^:private umulhi [a b] `(Math/unsignedMultiplyHigh (long ~a) (long ~b)))
(defn- ult [^long a ^long b] (neg? (Long/compareUnsigned a b)))

(defn- power-of-ten
  "Returns [hi lo32]: hi is the normalized 64-bit significand of 10^p, lo32 the next 32 bits."
  [^long p]
  (cond
    (and (>= p 0) (<= p 26)) [(aget a-base p) 0]
    (= p -1) [(unchecked-long 0xcccccccccccccccc) 0xcccccccc]
    :else
    (let [q (quot p 27) r (rem p 27)]
      (if (zero? r)
        [(aget a-scale (+ q 13)) (aget a-scale-lo (+ q 13))]
        (let [[idx r] (if (neg? r) [(+ q 12) (+ r 27)] [(+ q 13) r])
              hi (aget a-scale idx)
              lo32 (aget a-scale-lo idx)
              b (aget a-base r)
              ph (umulhi hi b)
              pl (unchecked-multiply hi b)
              ;; (lo32*b) >> 32 as a 128-bit quantity
              t-hi (umulhi lo32 b)
              t-lo (unchecked-multiply lo32 b)
              sh-lo (bit-or (unsigned-bit-shift-right t-lo 32) (bit-shift-left t-hi 32))
              sh-hi (unsigned-bit-shift-right t-hi 32)
              s-lo (unchecked-add pl sh-lo)
              carry (if (ult s-lo pl) 1 0)
              s-hi (unchecked-add (unchecked-add ph sh-hi) carry)
              rcx (unsigned-bit-shift-right s-lo 32)]
          (if (neg? s-hi)
            [s-hi rcx]
            [(bit-or (bit-shift-left s-hi 1) (bit-and (unsigned-bit-shift-right rcx 31) 1))
             (bit-and (inc (* 2 rcx)) 0xffffffff)]))))))

(defn fp10-convert
  "sqlite3Fp10Convert2: the double nearest m * 10^e10 (m > 0)."
  ^double [^long m ^long e10]
  (let [clz (Long/numberOfLeadingZeros m)
        F (bit-shift-right (* e10 108853) 15)
        r11 (- clz 11 F)]
    (if (and (> r11 1074) (> r11 1129))
      0.0
      (let [r11 (if (> r11 1074) 1074 r11)
            ebp (- -3 (+ F (- r11 clz)))
            [hi lo] (power-of-ten e10)
            hi (long hi) lo (long lo)
            [hi lo] (if (not= lo 0) [(unchecked-inc hi) (bit-and (bit-not lo) 0xffffffff)] [hi lo])
            hi (long hi) lo (long lo)
            m2 (bit-shift-left m clz)
            mask (bit-not (bit-shift-left -1 ebp))
            ph (umulhi hi m2)
            pl (unchecked-multiply hi m2)
            [rsi sticky]
            (if (zero? (bit-and ph mask))
              (let [rcx (unsigned-bit-shift-right pl 32)
                    t (unsigned-bit-shift-right (umulhi (bit-shift-left lo 32) m2) 32)
                    eax (bit-and (- rcx t) 0xffffffff)
                    sticky (if (> eax 1) 1 0)
                    rsi (if (< rcx t) (unchecked-dec ph) ph)]
                [rsi sticky])
              [ph 1])
            rax (bit-or (unsigned-bit-shift-right (long rsi) ebp) (long sticky))
            [rax r11] (if (ult 0x7ffffffffffffd rax)
                        [(bit-or (unsigned-bit-shift-right rax 1) (bit-and rax 1)) (dec r11)]
                        [rax r11])
            rax (long rax) r11 (long r11)]
        (if (< r11 -971)
          Double/POSITIVE_INFINITY
          (let [rdx (+ rax 1 (bit-and (unsigned-bit-shift-right rax 2) 1))
                rax (unsigned-bit-shift-right rdx 2)
                rax (if (bit-test rdx 54)
                      (bit-or (bit-clear rax 52) (bit-shift-left (- 1075 r11) 52))
                      rax)]
            (Double/longBitsToDouble rax)))))))

;; ------------------------------------------------------------------ text -> number

(defn- space? [c] (let [c (int c)] (or (= c 32) (and (>= c 9) (<= c 13)))))
(defn- digit? [c] (let [c (int c)] (and (>= c 48) (<= c 57))))

(def ^:private T (unchecked-long 0x1999999999999997))

(defn atof
  "Text -> double like sqlite3AtoF. Returns [value eType] where eType: >0 whole string is a number
  (1 integer form, 2+ has '.'/exponent), -1 valid numeric prefix (real) followed by junk, 0 not a number."
  [^String z]
  (let [n (.length z)
        i (loop [i 0] (if (and (< i n) (space? (.charAt z i))) (recur (inc i)) i))]
    (if (>= i n)
      [0.0 0]
      (let [neg (= (.charAt z i) \-)
            i (if (or (= (.charAt z i) \-) (= (.charAt z i) \+)) (inc i) i)
            s (long-array 1) e (long-array 1) nd (long-array 1) ovf (boolean-array 1)
            i (loop [i i]
                (if (and (< i n) (digit? (.charAt z i)))
                  (do (aset nd 0 (inc (aget nd 0)))
                      (if (aget ovf 0)
                        (aset e 0 (inc (aget e 0)))
                        (let [v (unchecked-add (unchecked-multiply (aget s 0) 10) (- (int (.charAt z i)) 48))]
                          (aset s 0 v)
                          (when (ult T v) (aset ovf 0 true))))
                      (recur (inc i)))
                  i))
            etype (long-array [1])
            i (if (and (< i n) (= (.charAt z i) \.))
                (do (aset etype 0 2)
                    (loop [i (inc i)]
                      (if (and (< i n) (digit? (.charAt z i)))
                        (do (aset nd 0 (inc (aget nd 0)))
                            (when (not (ult T (aget s 0)))
                              (aset s 0 (unchecked-add (unchecked-multiply (aget s 0) 10) (- (int (.charAt z i)) 48)))
                              (aset e 0 (dec (aget e 0))))
                            (recur (inc i)))
                        i)))
                i)
            ndig (aget nd 0)
            i (if (and (> ndig 0) (< i n) (or (= (.charAt z i) \e) (= (.charAt z i) \E)))
                (let [j (inc i)
                      esign (if (and (< j n) (= (.charAt z j) \-)) -1 1)
                      j (if (and (< j n) (or (= (.charAt z j) \-) (= (.charAt z j) \+))) (inc j) j)]
                  (if (and (< j n) (digit? (.charAt z j)))
                    (let [[j ev] (loop [j j ev 0]
                                   (if (and (< j n) (digit? (.charAt z j)))
                                     (recur (inc j) (if (<= ev 9999) (+ (* ev 10) (- (int (.charAt z j)) 48)) 10000))
                                     [j ev]))]
                      (aset e 0 (+ (aget e 0) (* esign (long ev))))
                      (aset etype 0 (if (= (aget etype 0) 2) 3 2))
                      j)
                    i))
                i)
            i (loop [i i] (if (and (< i n) (space? (.charAt z i))) (recur (inc i)) i))
            sv (aget s 0) ev (aget e 0)
            v (cond (zero? sv) 0.0
                    (< ev -348) 0.0
                    (> ev 347) Double/POSITIVE_INFINITY
                    :else (fp10-convert sv ev))
            v (if neg (- v) v)
            et (aget etype 0)]
        [v (cond (= ndig 0) 0
                 (= i n) et
                 (>= et 2) -1
                 :else 0)]))))

(defn atoi64
  "Port of sqlite3Atoi64. Returns [value rc]: rc 0 ok, 1 extra text, -1 no digits, 2 overflow, 3 exactly 2^63."
  [^String z]
  (let [n (.length z)
        i (loop [i 0] (if (and (< i n) (space? (.charAt z i))) (recur (inc i)) i))
        neg (and (< i n) (= (.charAt z i) \-))
        i (if (and (< i n) (or (= (.charAt z i) \-) (= (.charAt z i) \+))) (inc i) i)
        start i
        i (loop [i i] (if (and (< i n) (= (.charAt z i) \0)) (recur (inc i)) i))
        dstart i
        dend (loop [j i] (if (and (< j n) (digit? (.charAt z j))) (recur (inc j)) j))
        nd (- dend dstart)
        big (if (pos? nd) (java.math.BigInteger. (.substring z dstart dend)) java.math.BigInteger/ZERO)
        maxb (java.math.BigInteger/valueOf Long/MAX_VALUE)
        v (cond (> (.compareTo big maxb) 0) (if neg Long/MIN_VALUE Long/MAX_VALUE)
                neg (- (.longValue big))
                :else (.longValue big))
        rc (cond (and (= nd 0) (= start dstart)) -1
                 (< dend n) (if (loop [j dend] (cond (>= j n) false (space? (.charAt z j)) (recur (inc j)) :else true)) 1 0)
                 :else 0)]
    (if (< nd 19)
      [v rc]
      (let [c (if (> nd 19) 1 (.compareTo big (.add maxb java.math.BigInteger/ONE)))]
        (cond (< c 0) [v rc]
              (> c 0) [(if neg Long/MIN_VALUE Long/MAX_VALUE) 2]
              :else [(if neg Long/MIN_VALUE Long/MAX_VALUE) (if neg rc 3)])))))

;; ------------------------------------------------------------------ number -> text

(defn- digits-17
  "Decimal digits of r > 0 as produced by SQLite's FpDecode for iRound=17 (before rounding).
  Returns [digit-string iDP]."
  [^double r]
  (let [bits (Double/doubleToRawLongBits r)
        be (bit-and (unsigned-bit-shift-right bits 52) 0x7ff)
        frac (bit-and bits 0xfffffffffffff)
        [m e2] (if (zero? be)
                 (let [clz (Long/numberOfLeadingZeros frac)]
                   [(bit-shift-left frac clz) (- -1074 clz)])
                 [(bit-or (bit-shift-left frac 11) Long/MIN_VALUE) (- be 1086)])
        m (long m) e2 (long e2)
        e10 (bit-shift-right (* (+ e2 63) 78913) 18)
        p (- 17 e10)
        [hi _] (power-of-ten p)
        ph (umulhi m hi)
        F (bit-shift-right (* p 108853) 15)
        sh (- -2 (+ F e2))
        t (unsigned-bit-shift-right ph sh)
        nn (unsigned-bit-shift-right (+ t (bit-and (bit-shift-left t 1) 2)) 1)
        E (- e10 17)
        ds (Long/toUnsignedString nn)]
    [ds (+ E (count ds))]))

(defn- strip-zeros ^String [^String s]
  (let [e (loop [k (count s)] (if (and (> k 1) (= (.charAt s (dec k)) \0)) (recur (dec k)) k))]
    (subs s 0 e)))

(defn- round-at
  "Rounds digit string z at k digits (half-up on digit k). Returns [digits iDP]."
  [^String z k idp]
  (let [k (long k)]
    (if (>= k (count z))
      [(strip-zeros z) idp]
      (if (> (int (.charAt z k)) (int \4))
        (let [b (StringBuilder. (subs z 0 k))]
          (loop [j (dec k)]
            (if (< j 0)
              (do (.insert b 0 \1) [(strip-zeros (.toString b)) (inc idp)])
              (let [c (.charAt b j)]
                (if (= c \9)
                  (do (.setCharAt b j \0) (recur (dec j)))
                  (do (.setCharAt b j (char (inc (int c)))) [(strip-zeros (.toString b)) idp]))))))
        [(strip-zeros (subs z 0 k)) idp]))))

(defn- prefix-val ^long [^String z k]
  (loop [i 0 v 0] (if (< i k) (recur (inc i) (+ (* v 10) (- (int (.charAt z i)) 48))) v)))

(defn- conv-check [^long m ^long e ^double r]
  (let [x (cond (< e -348) 0.0 (> e 347) Double/POSITIVE_INFINITY :else (fp10-convert m e))]
    (== x r)))

(defn- decode-17
  "Digits and decimal point position for %!.17g of r > 0."
  [^double r]
  (let [[^String z idp] (digits-17 r)
        nd (count z)
        idp (long idp)]
    (if (<= nd 17)
      [(strip-zeros z) idp]
      (let [d15 (.charAt z 15)
            zero-check
            (fn []
              (let [k (if (not= (.charAt z 12) \0)
                        13
                        (loop [k 12] (if (and (> k 1) (= (.charAt z (dec k)) \0)) (recur (dec k)) k)))
                    k (long k)
                    mv (if (<= k 1) (- (int (.charAt z 0)) 48) (prefix-val z k))]
                (if (conv-check mv (- idp k) r)
                  (round-at z (inc k) idp)
                  (round-at z 17 idp))))]
        (cond
          (= d15 \9)
          (if (= (.charAt z 14) \9)
            ;; nines: try rounding up the prefix before the run of 9s
            (let [k (loop [k 14] (if (and (> k 0) (= (.charAt z (dec k)) \9)) (recur (dec k)) k))
                  k (long k)
                  [mv e] (if (zero? k) [1 idp] [(inc (prefix-val z k)) (- idp k)])]
              (if (conv-check mv e r)
                (round-at z (inc k) idp)
                (round-at z 17 idp)))
            (if (> nd idp) (round-at z 17 idp) (zero-check)))
          (<= nd idp) (zero-check)
          (and (= d15 \0) (= (.charAt z 14) \0) (= (.charAt z 13) \0)) (zero-check)
          :else (round-at z 17 idp))))))

(defn- render-g
  "Renders like SQLite's %!.17g given decoded digits."
  [neg ^String digits idp]
  (let [ex (dec (long idp))
        sb (StringBuilder.)]
    (when neg (.append sb \-))
    (if (or (< ex -4) (> ex 16))
      (do (.append sb (.charAt digits 0))
          (.append sb \.)
          (if (> (count digits) 1) (.append sb (subs digits 1)) (.append sb \0))
          (.append sb \e)
          (.append sb (if (neg? ex) \- \+))
          (let [a (Math/abs ex)]
            (when (< a 10) (.append sb \0))
            (.append sb a)))
      (if (< idp 1)
        (do (.append sb "0.")
            (dotimes [_ (- idp)] (.append sb \0))
            (.append sb digits))
        (let [nd (count digits)]
          (if (<= nd idp)
            (do (.append sb digits)
                (dotimes [_ (- idp nd)] (.append sb \0))
                (.append sb ".0"))
            (do (.append sb (subs digits 0 idp))
                (.append sb \.)
                (.append sb (subs digits idp)))))))
    (.toString sb)))

(defn real->text
  "SQLite's text for a REAL value (sqlite3_column_text)."
  ^String [^double r]
  (cond
    (Double/isNaN r) "NaN"
    (Double/isInfinite r) (if (pos? r) "Inf" "-Inf")
    (== r 0.0) "0.0"
    :else
    (let [neg (< r 0.0)
          [d idp] (decode-17 (Math/abs r))]
      (render-g neg d idp))))

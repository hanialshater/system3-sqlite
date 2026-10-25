(ns sqlclj.printf
  "SQLite's printf()/format() (sqlite3_str_vappendf)."
  (:require [sqlclj.values :as v]
            [sqlclj.num :as num]))

(defn- digit-at [^String z n k]
  (if (and (>= k 0) (< k (long n))) (.charAt z (int k)) \0))

(defn fp-decode
  "FpDecode for |r|: returns {:z digits :n n :idp idp :neg neg :special nil|:inf|:nan}."
  [^double r iround mxround alt2]
  (cond
    (Double/isNaN r) {:special :nan}
    (Double/isInfinite r) {:special :inf :neg (neg? r)}
    :else
    (let [neg (or (< r 0.0) (and (== r 0.0) (neg? (Math/copySign 1.0 r)) false))
          a (Math/abs r)
          [z idp] (if (== a 0.0) ["0" 1] (num/fp-digits a (and alt2 (= iround 17))))
          ^String z z
          idp (long idp)
          n (count z)
          ;; the special-cased 17-digit path already trimmed trailing zeros and rounded
          iround (long iround) mxround (long mxround)
          [z n idp iround]
          (if (<= iround 0)
            (let [ir (- idp iround)]
              (if (and (zero? ir) (>= (int (.charAt z 0)) (int \5)))
                [(str "0" z) (inc n) (inc idp) 1]
                [z n idp ir]))
            [z n idp iround])
          [z n idp]
          (if (and (> iround 0) (or (< iround n) (> n mxround)))
            (let [ir (min iround mxround)]
              (if (>= ir n)
                [z n idp]
                (if (>= (int (.charAt ^String z ir)) (int \5))
                  ;; round up with carry
                  (let [b (StringBuilder. (subs z 0 ir))]
                    (loop [j (dec ir)]
                      (if (< j 0)
                        (do (.insert b 0 \1) [(.toString b) (inc (.length b)) (inc idp)])
                        (let [c (.charAt b j)]
                          (if (= c \9)
                            (do (.setCharAt b j \0) (recur (dec j)))
                            (do (.setCharAt b j (char (inc (int c))))
                                [(.toString b) ir idp]))))))
                  [(subs z 0 ir) ir idp])))
            [z n idp])
          ;; trim trailing zeros
          ^String z z
          e (loop [k (count z)] (if (and (> k 0) (= (.charAt z (dec k)) \0)) (recur (dec k)) k))
          z (subs z 0 e)]
      {:z z :n (count z) :idp idp :neg (< r 0.0)})))

(defn- thousands [^String digits]
  (let [n (count digits) sb (StringBuilder.)]
    (dotimes [i n]
      (when (and (pos? i) (zero? (mod (- n i) 3))) (.append sb \,))
      (.append sb (.charAt digits i)))
    (.toString sb)))

(defn- pad-str
  "Pads s (display length len) to width."
  [^String s ^long len ^long width left?]
  (if (> width len)
    (let [sp (apply str (repeat (- width len) \space))]
      (if left? (str s sp) (str sp s)))
    s))

(defn- utf8-trunc ^String [^String s ^long nbytes]
  (let [b (v/str->utf8 s)]
    (if (<= (alength b) nbytes) s (v/utf8->str (java.util.Arrays/copyOf b (int nbytes))))))

(defn- char-count ^long [^String s] (.codePointCount s 0 (.length s)))

(defn- ordinal-suffix [^long n]
  (let [m (mod (Math/abs n) 100) d (mod (Math/abs n) 10)]
    (if (and (>= m 10) (<= m 20)) "th" (case (int d) 1 "st" 2 "nd" 3 "rd" "th"))))

(defn sql-printf
  "Formats fmt (a String) with args (SQL values). Returns a String, or nil on an invalid conversion."
  [^String fmt args]
  (let [sb (StringBuilder.)
        n (.length fmt)
        args (vec args)
        argi (volatile! 0)
        next-arg (fn [] (let [i @argi] (vswap! argi inc) (when (< i (count args)) (nth args i))))
        ok (volatile! true)
        app (volatile! false)]
    (loop [i 0]
      (when (and @ok (< i n))
        (let [pct (.indexOf fmt (int \%) i)]
          (if (neg? pct)
            (when (< i n) (vreset! app true) (.append sb (subs fmt i)))
            (do
              (when (< i pct) (vreset! app true) (.append sb (subs fmt i pct)))
              (if (>= (inc pct) n)
                (do (vreset! app true) (.append sb \%))
                ;; parse flags
                (let [[i flags]
                      (loop [i (inc pct) flags #{}]
                        (let [c (if (< i n) (.charAt fmt i) (char 0))]
                          (case c
                            \- (recur (inc i) (conj flags :left))
                            \+ (recur (inc i) (conj (disj flags :space) :plus))
                            \space (recur (inc i) (conj (disj flags :plus) :space))
                            \# (recur (inc i) (conj flags :alt))
                            \! (recur (inc i) (conj flags :alt2))
                            \0 (recur (inc i) (conj flags :zero))
                            \, (recur (inc i) (conj flags :comma))
                            [i flags])))
                      c (fn [i] (if (< i n) (.charAt fmt (int i)) (char 0)))
                      ;; width
                      [i width flags]
                      (if (= (c i) \*)
                        (let [w (unchecked-int (v/int-value (next-arg)))]
                          (if (neg? w)
                            [(inc i) (if (>= w -2147483647) (- w) 0) (conj flags :left)]
                            [(inc i) w flags]))
                        (loop [i i w 0]
                          (let [ch (c i)]
                            (if (and (>= (int ch) 48) (<= (int ch) 57))
                              (recur (inc i) (+ (* w 10) (- (int ch) 48)))
                              [i w flags]))))
                      _ (when (> width 100000000) (v/err "string or blob too big"))
                      width (long width)
                      ;; precision
                      [i prec]
                      (if (= (c i) \.)
                        (let [i (inc i)]
                          (if (= (c i) \*)
                            (let [p (unchecked-int (v/int-value (next-arg)))]
                              [(inc i) (if (neg? p) (if (>= p -2147483647) (- p) -1) p)])
                            (loop [i i p 0]
                              (let [ch (c i)]
                                (if (and (>= (int ch) 48) (<= (int ch) 57))
                                  (recur (inc i) (+ (* p 10) (- (int ch) 48)))
                                  [i p])))))
                        [i -1])
                      _ (when (> prec 100000000) (v/err "string or blob too big"))
                      prec (long prec)
                      ;; length modifier
                      i (if (= (c i) \l) (if (= (c (inc i)) \l) (+ i 2) (inc i)) i)
                      cv (c i)
                      left? (contains? flags :left)
                      alt (contains? flags :alt)
                      alt2 (contains? flags :alt2)
                      zpad (contains? flags :zero)
                      prefix-ch (cond (contains? flags :plus) \+ (contains? flags :space) \space)
                      emit (fn [^String s ^long len]
                             (.append sb ^String (pad-str s len width left?)))
                      num-emit (fn [^String sign ^String body]
                                 ;; numeric field with zero padding after the sign
                                 (let [len (+ (count sign) (count body))]
                                   (if (and zpad (not left?) (> width len))
                                     (.append sb (str sign (apply str (repeat (- width len) \0)) body))
                                     (.append sb ^String (pad-str (str sign body) len width left?)))))]
                  (case cv
                    (\d \i \u \x \X \o \p \r)
                    (let [iv (v/int-value (next-arg))
                          radix (case cv (\x \X \p) 16 \o 8 10)
                          signed? (contains? #{\d \i \r} cv)
                          neg (and signed? (neg? iv))
                          digits (cond
                                   signed? (if (= iv Long/MIN_VALUE) "9223372036854775808" (Long/toString (Math/abs iv)))
                                   :else (case radix
                                           16 (Long/toHexString iv)
                                           8 (Long/toOctalString iv)
                                           (Long/toUnsignedString iv)))
                          digits (if (contains? #{\X \p} cv) (.toUpperCase ^String digits) digits)
                          digits (if (= cv \r)
                                   (str digits (let [a (if (= iv Long/MIN_VALUE) 8 (Math/abs iv))]
                                                 (if (or (>= (mod a 10) 4) (= 1 (mod (quot a 10) 10))) "th"
                                                     (case (int (mod a 10)) 1 "st" 2 "nd" 3 "rd" "th"))))
                                   digits)
                          sign (cond neg "-" (and signed? prefix-ch) (str prefix-ch) :else "")
                          prec (if (and zpad (< prec (- width (count sign)))) (- width (count sign)) prec)
                          digits (if (and (>= prec 0) (< (count digits) prec))
                                   (str (apply str (repeat (- prec (count digits)) \0)) digits)
                                   digits)
                          digits (if (and (contains? flags :comma) (contains? #{\d \i \u} cv)) (thousands digits) digits)
                          altp (cond (and alt (= radix 8) (not (zero? iv))) "0"
                                     (and alt (= radix 16) (not (zero? iv))) (if (= cv \X) "0X" "0x")
                                     :else "")]
                      (.append sb ^String (pad-str (str sign altp digits) (+ (count sign) (count altp) (count digits)) width left?)))
                    (\f \e \E \g \G)
                    (let [r (v/real-value (next-arg))
                          xt (case cv \f :float (\e \E) :exp :gen)
                          prec (if (< prec 0) 6 prec)
                          prec (if (and (= xt :gen) (zero? prec)) 1 prec)
                          iround (case xt :float (- prec) :gen prec :exp (inc prec))
                          s (fp-decode r iround (if alt2 26 16) alt2)]
                      (if (:special s)
                        (let [body (if (= (:special s) :nan) "NaN" "Inf")
                              sign (cond (:neg s) "-" prefix-ch (str prefix-ch) :else "")]
                          (emit (str sign body) (+ (count sign) (count body))))
                        (let [^String z (:z s) nd (long (:n s)) idp (long (:idp s))
                              nd (if (= z "") 0 nd)
                              ;; zero value
                              [z nd idp] (if (zero? nd) ["" 0 1] [z nd idp])
                              exp (dec idp)
                              [xt prec rtz]
                              (if (= xt :gen)
                                (let [p (if (> prec 0) (dec prec) prec)
                                      rtz (not alt)]
                                  (if (or (< exp -4) (> exp p))
                                    [:exp p rtz]
                                    [:float (- p exp) rtz]))
                                [xt prec alt2])
                              exp (if (and (zero? nd)) 0 exp)
                              sign (cond (:neg s) "-" prefix-ch (str prefix-ch) :else "")
                              b (StringBuilder.)
                              dg (fn [k] (digit-at z nd k))]
                          (if (= xt :float)
                            (do
                              (if (<= idp 0)
                                (.append b \0)
                                (let [intp (StringBuilder.)]
                                  (dotimes [k idp] (.append intp (dg k)))
                                  (.append b (if (contains? flags :comma) (thousands (.toString intp)) (.toString intp)))))
                              (when (or (> prec 0) alt alt2) (.append b \.))
                              (dotimes [j prec]
                                (.append b (dg (+ j idp)))))
                            (do
                              (.append b (if (zero? nd) \0 (.charAt z 0)))
                              (when (or (> prec 0) alt alt2) (.append b \.))
                              (dotimes [j prec] (.append b (dg (inc j))))))
                          ;; remove trailing zeros
                          (when rtz
                            (let [dot (.indexOf b ".")]
                              (when (>= dot 0)
                                (loop []
                                  (when (= (.charAt b (dec (.length b))) \0)
                                    (.setLength b (dec (.length b))) (recur)))
                                (when (= (.charAt b (dec (.length b))) \.)
                                  (if alt2 (.append b \0) (when-not alt (.setLength b (dec (.length b)))))))))
                          (when (= xt :exp)
                            (.append b (if (or (= cv \E) (= cv \G)) \E \e))
                            (let [e exp]
                              (.append b (if (neg? e) \- \+))
                              (let [a (Math/abs e)]
                                (when (< a 10) (.append b \0))
                                (.append b a))))
                          (num-emit sign (.toString b)))))
                    \c
                    (let [t (or (v/text-of (next-arg)) "")
                          ch (if (empty? t) "\u0000" (subs t 0 (Character/charCount (.codePointAt t 0))))
                          str1 (if (> prec 1) (apply str (repeat prec ch)) ch)]
                      (emit str1 (char-count str1)))
                    (\s \z)
                    (let [a (next-arg)
                          t (or (v/text-of a) "")
                          t (let [k (.indexOf ^String t (int 0))] (if (neg? k) t (subs t 0 k)))
                          t (if (>= prec 0)
                              (if alt2
                                (let [cps (.codePoints t) arr (.toArray cps)
                                      k (min prec (alength arr))]
                                  (String. arr 0 (int k)))
                                (utf8-trunc t prec))
                              t)]
                      (emit t (alength (v/str->utf8 t))))
                    (\q \Q \w)
                    (let [a (next-arg)
                          isq (= cv \Q)
                          t (cond (nil? a) (if isq nil "(NULL)")
                                  :else (let [^String t (v/text-of a) k (.indexOf t (int 0))] (if (neg? k) t (subs t 0 k))))]
                      (if (nil? t)
                        (let [t (if (and (>= prec 0) (< prec 4)) (subs "NULL" 0 prec) "NULL")] (emit t (count t)))
                        (let [t (if (>= prec 0)
                                  (utf8-trunc t prec)
                                  t)
                              qc (if (= cv \w) "\"" "'")
                              body (.replace ^String t ^String qc ^String (str qc qc))
                              body (if isq (str "'" body "'") body)]
                          (emit body (alength (v/str->utf8 body))))))
                    \%
                    (emit "%" 1)
                    \n nil
                    (vreset! ok false))
                  (when @ok (vreset! app true))
                  (recur (inc i)))))))))
    (when @app (str sb))))

(ns sqlclj.datetime
  "SQLite's date and time functions (a port of date.c)."
  (:require [sqlclj.values :as v]
            [sqlclj.num :as num]
            [sqlclj.printf :as pf]))

;; DateTime state is a mutable map held in a volatile
;; keys: :iJD :Y :M :D :h :m :tz :s :validJD :validYMD :validHMS :validTZ :nFloor :rawS :isError :useSubsec :isUtc :isLocal

(defn- new-dt [] (volatile! {:iJD 0 :Y 0 :M 0 :D 0 :h 0 :m 0 :tz 0 :s 0.0 :nFloor 0}))
(defmacro ^:private g [p k] `(get @~p ~k))
(defmacro ^:private s! [p & kvs] `(vswap! ~p assoc ~@kvs))

(defn- space? [c] (let [c (int c)] (or (= c 32) (and (>= c 9) (<= c 13)))))
(defn- digit? [c] (let [c (int c)] (and (>= c 48) (<= c 57))))

(defn- ch ^long [^String z ^long i] (if (< i (.length z)) (long (.charAt z i)) 0))

(def ^:private amx [12 14 24 31 59 14712])

(defn- get-digits
  "format like \"40f-21a-21d\". Returns vector of values or nil."
  [^String z ^long pos ^String fmt]
  (loop [zi pos fi 0 vals []]
    (let [n (- (long (.charAt fmt fi)) 48)
          mn (- (long (.charAt fmt (+ fi 1))) 48)
          mxv (long (nth amx (- (long (.charAt fmt (+ fi 2))) 97)))
          next-c (if (< (+ fi 3) (.length fmt)) (long (.charAt fmt (+ fi 3))) 0)]
      (let [r (loop [k 0 zi zi val 0]
                (if (= k n)
                  [zi val]
                  (if (digit? (ch z zi))
                    (recur (inc k) (inc zi) (+ (* val 10) (- (ch z zi) 48)))
                    nil)))]
        (when r
          (let [[zi2 val] r]
            (when-not (or (< val mn) (> val mxv) (and (not= next-c 0) (not= next-c (ch z zi2))))
              (let [vals (conj vals val)]
                (if (= next-c 0)
                  vals
                  (recur (inc zi2) (+ fi 4) vals))))))))))

(defn- valid-jd? [^long j] (and (>= j 0) (<= j 464269060799999)))

(defn- clear-ymd-hms-tz [p] (s! p :validYMD false :validHMS false :validTZ false))

(defn- datetime-error [p] (vreset! p {:iJD 0 :Y 0 :M 0 :D 0 :h 0 :m 0 :tz 0 :s 0.0 :nFloor 0 :isError true}))

(defn- fdiv [a b] (quot (long a) (long b))) ; C integer division (truncating)

(defn- compute-jd [p]
  (when-not (g p :validJD)
    (let [{:keys [Y M D]} (if (g p :validYMD) @p {:Y 2000 :M 1 :D 1})
          Y (long Y) M (long M) D (long D)]
      (if (or (< Y -4713) (> Y 9999) (g p :rawS))
        (datetime-error p)
        (let [dim (case (long M) (4 6 9 11) 30 2 (if (and (zero? (rem Y 4)) (or (not (zero? (rem Y 100))) (zero? (rem Y 400)))) 29 28) 31)
              _ (when (> D dim) (s! p :nFloor (- D dim)))
              [Y M] (if (<= M 2) [(dec Y) (+ M 12)] [Y M])
              A (Math/floorDiv Y 100)
              B (+ (- 2 A) (Math/floorDiv A 4))
              X1 (fdiv (* 36525 (+ Y 4716)) 100)
              X2 (fdiv (* 306001 (+ M 1)) 10000)
              jd (long (* (- (+ X1 X2 D B) 1524.5) 86400000.0))]
          (s! p :iJD jd :validJD true)
          (when (g p :validHMS)
            (s! p :iJD (+ (long (g p :iJD)) (* (long (g p :h)) 3600000) (* (long (g p :m)) 60000)
                          (long (+ (* (double (g p :s)) 1000.0) 0.5))))
            (when (g p :validTZ)
              (s! p :iJD (- (long (g p :iJD)) (* (long (g p :tz)) 60000)))
              (clear-ymd-hms-tz p))))))))

(defn- parse-timezone
  "returns true on error"
  [p ^String z ^long pos]
  (let [pos (loop [i pos] (if (space? (ch z i)) (recur (inc i)) i))]
    (s! p :tz 0)
    (let [c (ch z pos)]
      (cond
        (or (= c 45) (= c 43))
        (let [sgn (if (= c 45) -1 1)]
          (if-let [[hr mn] (get-digits z (inc pos) "20b:20e")]
            (let [pos2 (+ pos 1 5)]
              (s! p :tz (* sgn (+ mn (* hr 60))))
              (let [pos3 (loop [i pos2] (if (space? (ch z i)) (recur (inc i)) i))]
                (not= 0 (ch z pos3))))
            true))
        (or (= c 90) (= c 122))
        (do (s! p :isLocal false :isUtc true)
            (let [pos3 (loop [i (inc pos)] (if (space? (ch z i)) (recur (inc i)) i))]
              (not= 0 (ch z pos3))))
        :else (not= c 0)))))

(defn- parse-hhmmss
  "returns true on error"
  [p ^String z ^long pos]
  (if-let [[h m] (get-digits z pos "20c:20e")]
    (let [pos (+ pos 5)
          r (if (= (ch z pos) 58)
              (when-let [[sec] (get-digits z (inc pos) "20e")]
                (let [pos (+ pos 3)]
                  (if (and (= (ch z pos) 46) (digit? (ch z (inc pos))))
                    (loop [i (inc pos) ms 0.0 scale 1.0]
                      (if (digit? (ch z i))
                        (recur (inc i) (+ (* ms 10.0) (- (ch z i) 48)) (* scale 10.0))
                        (let [ms (/ ms scale) ms (if (> ms 0.999) 0.999 ms)]
                          [i (double sec) ms])))
                    [pos (double sec) 0.0])))
              [pos 0.0 0.0])]
      (if (nil? r)
        true
        (let [[pos sec ms] r]
          (s! p :validJD false :rawS false :validHMS true :h h :m m :s (+ sec ms))
          (let [e (parse-timezone p z pos)]
            (s! p :validTZ (not= 0 (long (g p :tz))))
            e))))
    true))

(defn- parse-yyyy-mm-dd
  "returns true on error"
  [p ^String z]
  (let [neg (= (ch z 0) 45)
        pos (if neg 1 0)]
    (if-let [[Y M D] (get-digits z pos "40f-21a-21d")]
      (let [pos (loop [i (+ pos 10)] (if (or (space? (ch z i)) (= (ch z i) 84)) (recur (inc i)) i))]
        (let [err? (cond (not (parse-hhmmss p z pos)) false
                         (= (ch z pos) 0) (do (s! p :validHMS false) false)
                         :else true)]
          (if err?
            true
            (do (s! p :validJD false :validYMD true :Y (if neg (- Y) Y) :M M :D D)
                (when (g p :validTZ) (compute-jd p))
                false))))
      true)))

(defn- set-raw-date-number [p ^double r]
  (s! p :s r :rawS true)
  (when (and (>= r 0.0) (< r 5373484.5))
    (s! p :iJD (long (+ (* r 86400000.0) 0.5)) :validJD true)))

(defn- set-to-current [p]
  (let [ms (System/currentTimeMillis)]
    (s! p :iJD (+ (long ms) 210866760000000) :validJD true)
    true))

(defn- ci= [^String a ^String b] (.equalsIgnoreCase a b))

(defn- parse-date-or-time
  "returns true on error"
  [p ^String z]
  (cond
    (not (parse-yyyy-mm-dd p z)) false
    (not (parse-hhmmss p z 0)) false
    (ci= z "now") (do (set-to-current p) false)
    (pos? (long (let [[r rc] (num/atof z)] (if (pos? (long rc)) 1 0))))
    (do (set-raw-date-number p (double (first (num/atof z)))) false)
    (or (ci= z "subsec") (ci= z "subsecond")) (do (s! p :useSubsec true) (set-to-current p) false)
    :else true))

(defn- compute-ymd [p]
  (when-not (g p :validYMD)
    (cond
      (not (g p :validJD)) (s! p :Y 2000 :M 1 :D 1)
      (not (valid-jd? (g p :iJD))) (datetime-error p)
      :else
      (let [Z (long (quot (+ (long (g p :iJD)) 43200000) 86400000))
            alpha (- (long (/ (+ Z 32044.75) 36524.25)) 52)
            A (- (+ Z 1 alpha) (fdiv (+ alpha 100) 4) -25)
            B (+ A 1524)
            C (long (/ (- B 122.1) 365.25))
            D (fdiv (* 36525 (bit-and C 32767)) 100)
            E (long (/ (- B D) 30.6001))
            X1 (long (* 30.6001 E))
            day (- B D X1)
            M (if (< E 14) (- E 1) (- E 13))
            Y (if (> M 2) (- C 4716) (- C 4715))]
        (s! p :D day :M M :Y Y)))
    (s! p :validYMD true)))

(defn- compute-hms [p]
  (when-not (g p :validHMS)
    (compute-jd p)
    (let [day-ms (long (rem (+ (long (g p :iJD)) 43200000) 86400000))
          sec (/ (double (rem day-ms 60000)) 1000.0)
          day-min (quot day-ms 60000)]
      (s! p :s sec :m (rem day-min 60) :h (quot day-min 60) :rawS false :validHMS true))))

(defn- compute-ymd-hms [p] (compute-ymd p) (compute-hms p))

(def ^:private xform
  [["second" 6 4.6427e+14 1000.0]
   ["minute" 6 7.7379e+12 60000.0]
   ["hour" 4 1.2897e+11 3600000.0]
   ["day" 3 5373485.0 86400000.0]
   ["month" 5 176546.0 2592000000.0]
   ["year" 4 14713.0 31536000000.0]])

(defn- parse-modifier
  "returns true on error"
  [p ^String zin idx]
  (let [n (.length zin)]
    (if (or (>= n 30) (= n 0))
      true
      (let [z (.toLowerCase zin java.util.Locale/ROOT)
            c0 (.charAt z 0)]
        (case c0
          \a (if (= z "auto")
               (cond
                 (> idx 1) true
                 (or (not (g p :rawS)) (g p :validJD)) (do (s! p :rawS false) false)
                 (and (>= (double (g p :s)) -210866760000.0) (<= (double (g p :s)) 253402300799.0))
                 (let [r (+ (* (double (g p :s)) 1000.0) 210866760000000.0)]
                   (if (and (>= r 0.0) (< r 464269060800000.0))
                     (do (clear-ymd-hms-tz p) (s! p :iJD (long (+ r 0.5)) :validJD true :rawS false) false)
                     true))
                 :else true)
               true)
          \j (if (= z "julianday")
               (cond (> idx 1) true
                     (and (g p :validJD) (g p :rawS)) (do (s! p :rawS false) false)
                     :else true)
               true)
          \l (if (= z "localtime")
               (do (when-not (g p :isLocal)
                     (compute-jd p) (compute-ymd-hms p) (s! p :validJD false :rawS false :validTZ false :isError false))
                   (s! p :isUtc false :isLocal true)
                   false)
               true)
          \u (cond
               (and (= z "unixepoch") (g p :rawS))
               (let [r (+ (* (double (g p :s)) 1000.0) 210866760000000.0)]
                 (if (and (>= r 0.0) (< r 464269060800000.0))
                   (do (clear-ymd-hms-tz p) (s! p :iJD (long (+ r 0.5)) :validJD true :rawS false) false)
                   true))
               (= z "utc") (do (when-not (g p :isUtc)
                                 (compute-jd p)
                                 (let [j (long (g p :iJD))]
                                   (vreset! p {:iJD j :Y 0 :M 0 :D 0 :h 0 :m 0 :tz 0 :s 0.0 :nFloor 0 :validJD true :isUtc true :isLocal false})))
                               false)
               :else true)
          \w (if (.startsWith z "weekday ")
               (let [[r rc] (num/atof (subs zin 8))
                     r (double r)]
                 (if (and (pos? (long rc)) (>= r 0.0) (< r 7.0) (== r (double (long r))))
                   (let [nn (long r)]
                     (compute-ymd-hms p)
                     (s! p :validTZ false :validJD false)
                     (compute-jd p)
                     (let [Z (rem (quot (+ (long (g p :iJD)) 129600000) 86400000) 7)
                           Z (if (> Z nn) (- Z 7) Z)]
                       (s! p :iJD (+ (long (g p :iJD)) (* (- nn Z) 86400000)))
                       (clear-ymd-hms-tz p)
                       false))
                   true))
               true)
          \s (if-not (.startsWith z "start of ")
               (if (or (= z "subsec") (= z "subsecond")) (do (s! p :useSubsec true) false) true)
               (if (and (not (g p :validJD)) (not (g p :validYMD)) (not (g p :validHMS)))
                 true
                 (let [z (subs z 9)]
                   (compute-ymd p)
                   (s! p :validHMS true :h 0 :m 0 :s 0.0 :rawS false :validTZ false :validJD false)
                   (cond (= z "month") (do (s! p :D 1) false)
                         (= z "year") (do (s! p :M 1 :D 1) false)
                         (= z "day") false
                         :else true))))
          \c (if (= z "ceiling")
               (do (when (or (g p :validJD) (g p :validYMD) (g p :validHMS))
                     (compute-jd p) (clear-ymd-hms-tz p))
                   false)
               true)
          \f (if (= z "floor")
               (do (when (or (g p :validJD) (g p :validYMD) (g p :validHMS))
                     (compute-jd p)
                     (s! p :iJD (- (long (g p :iJD)) (* (long (g p :nFloor)) 86400000)))
                     (clear-ymd-hms-tz p))
                   false)
               true)
          (\+ \- \0 \1 \2 \3 \4 \5 \6 \7 \8 \9)
          (let [nn (loop [k 1]
                     (cond (or (>= k n) (= (.charAt zin k) \:) (space? (.charAt zin k))) k
                           (and (= (.charAt zin k) \-)
                                (or (and (= k 5) (get-digits zin 1 "40f"))
                                    (and (= k 6) (get-digits zin 1 "50f")))) k
                           :else (recur (inc k))))
                [r rc] (num/atof (subs zin 0 nn))
                r (double r)
                time-part
                (fn [zo z2]
                  ;; (+|-)HH:MM:SS.FFF added to the time; zo = offset of z[0], z2 = start of the time text
                  (let [z2 (if (digit? (.charAt zin z2)) z2 (inc z2))
                        tx (new-dt)]
                    (if (parse-hhmmss tx zin z2)
                      true
                      (do (compute-jd tx)
                          (s! tx :iJD (- (long (g tx :iJD)) 43200000))
                          (let [day (quot (long (g tx :iJD)) 86400000)
                                t (- (long (g tx :iJD)) (* day 86400000))
                                t (if (= (.charAt zin zo) \-) (- t) t)]
                            (compute-jd p)
                            (clear-ymd-hms-tz p)
                            (s! p :iJD (+ (long (g p :iJD)) t) :rawS false :nFloor 0)
                            false)))))]
            (cond
              (<= (long rc) 0) true
              (and (< nn n) (= (.charAt zin nn) \-))
              (let [z0 (.charAt zin 0)
                    zo (if (= nn 6) 1 0)]
                (if (or (and (not= z0 \+) (not= z0 \-)) (not (#{5 6} nn)))
                  true
                  (if-let [[Y M D] (get-digits zin 1 (if (= nn 5) "40f-20a-20d" "50f-20a-20d"))]
                    (if (or (>= M 12) (>= D 31))
                      true
                      (do (compute-ymd-hms p)
                          (s! p :validJD false)
                          (let [[Y M D] (if (= z0 \-)
                                          [(- Y) (- M) (- D)]
                                          [Y M D])
                                _ (s! p :Y (+ (long (g p :Y)) Y) :M (+ (long (g p :M)) M))
                                Mv (long (g p :M))
                                x (if (> Mv 0) (quot (dec Mv) 12) (quot (- Mv 12) 12))]
                            (s! p :Y (+ (long (g p :Y)) x) :M (- Mv (* x 12)))
                            (compute-jd p)
                            (s! p :validHMS false :validYMD false)
                            (s! p :iJD (+ (long (g p :iJD)) (* (long D) 86400000)))
                            (let [e (+ zo 11)]
                              (cond
                                (>= e n) false
                                (and (space? (.charAt zin e)) (get-digits zin (inc e) "20c:20e"))
                                (time-part zo (inc e))
                                :else true)))))
                    true)))
              (and (< nn n) (= (.charAt zin nn) \:))
              (time-part 0 (if (digit? (.charAt zin 0)) 0 1))
              :else
              (let [rest (.trim (subs zin nn))
                    rest (loop [i 0] (if (and (< i (count (subs zin nn))) (space? (.charAt (subs zin nn) i))) (recur (inc i)) i))
                    zrest (subs zin (+ nn rest))
                    m (.length zrest)]
                (if (or (> m 10) (< m 3))
                  true
                  (let [m (if (= (Character/toLowerCase (.charAt zrest (dec m))) \s) (dec m) m)
                        zr (.toLowerCase (subs zrest 0 m))]
                    (compute-jd p)
                    (let [rounder (if (< r 0) -0.5 0.5)
                          idx2 (first (keep-indexed (fn [i [nm nl lim _]]
                                                      (when (and (= nl m) (= nm zr) (> r (- (double lim))) (< r (double lim))) i))
                                                    xform))]
                      (s! p :nFloor 0)
                      (if (nil? idx2)
                        (do (clear-ymd-hms-tz p) true)
                        (let [[_ _ _ rx] (nth xform idx2)
                              r (case (long idx2)
                                  4 (do (compute-ymd-hms p)
                                        (let [M (+ (long (g p :M)) (long r))
                                              x (if (> M 0) (quot (dec M) 12) (quot (- M 12) 12))]
                                          (s! p :Y (+ (long (g p :Y)) x) :M (- M (* x 12)) :validJD false)
                                          (- r (double (long r)))))
                                  5 (let [y (long r)]
                                      (compute-ymd-hms p)
                                      (s! p :Y (+ (long (g p :Y)) y) :validJD false)
                                      (- r (double (long r))))
                                  r)]
                          (compute-jd p)
                          (s! p :iJD (+ (long (g p :iJD)) (long (+ (* (double r) (double rx)) rounder))))
                          (clear-ymd-hms-tz p)
                          (s! p :rawS false)
                          false))))))))
          true)))))

(defn- is-date
  "Returns the DateTime volatile or nil on error. args: vector of values."
  [args]
  (let [p (new-dt)
        n (count args)]
    (let [bad
          (if (zero? n)
            (do (set-to-current p) false)
            (let [a (first args)]
              (cond
                (or (instance? Long a) (instance? Double a)) (do (set-raw-date-number p (v/real-value a)) false)
                (nil? a) true
                :else (parse-date-or-time p (v/text-of a)))))]
      (if bad
        nil
        (let [bad (loop [i 1]
                    (if (< i n)
                      (let [a (nth args i)]
                        (if (or (nil? a) (parse-modifier p (v/text-of a) i))
                          true
                          (recur (inc i))))
                      false))]
          (if bad
            nil
            (do
              (compute-jd p)
              (if (or (g p :isError) (not (valid-jd? (g p :iJD))))
                nil
                (do
                  (when (and (= n 1) (g p :validYMD) (> (long (g p :D)) 28))
                    (s! p :validYMD false))
                  p)))))))))

(defn- fmt [^String f & args] (pf/sql-printf f args))

(defn- ystr [^long y] (if (neg? y) (str "-" (fmt "%04d" (- y))) (fmt "%04d" y)))

(defn date-fn [args]
  (when-let [p (is-date args)]
    (compute-ymd p)
    (str (ystr (long (g p :Y))) (fmt "-%02d-%02d" (long (g p :M)) (long (g p :D))))))

(defn time-fn [args]
  (when-let [p (is-date args)]
    (compute-hms p)
    (if (g p :useSubsec)
      (fmt "%02d:%02d:%06.3f" (long (g p :h)) (long (g p :m)) (double (g p :s)))
      (fmt "%02d:%02d:%02d" (long (g p :h)) (long (g p :m)) (long (double (g p :s)))))))

(defn datetime-fn [args]
  (when-let [p (is-date args)]
    (compute-ymd-hms p)
    (if (g p :useSubsec)
      (fmt (str (ystr (long (g p :Y))) "-%02d-%02d %02d:%02d:%06.3f") (long (g p :M)) (long (g p :D)) (long (g p :h)) (long (g p :m)) (double (g p :s)))
      (fmt (str (ystr (long (g p :Y))) "-%02d-%02d %02d:%02d:%02d") (long (g p :M)) (long (g p :D)) (long (g p :h)) (long (g p :m)) (long (double (g p :s)))))))

(defn julianday-fn [args]
  (when-let [p (is-date args)]
    (compute-jd p)
    (/ (double (long (g p :iJD))) 86400000.0)))

(defn unixepoch-fn [args]
  (when-let [p (is-date args)]
    (compute-jd p)
    (if (g p :useSubsec)
      (/ (double (- (long (g p :iJD)) (* 21086676 10000000))) 1000.0)
      (- (quot (long (g p :iJD)) 1000) (* 21086676 10000)))))

(defn- days-after-jan01 [p]
  (let [j (volatile! @p)]
    (vswap! j assoc :validJD false :M 1 :D 1)
    (compute-jd j)
    (long (quot (+ (- (long (g p :iJD)) (long (g j :iJD))) 43200000) 86400000))))
(defn- days-after-monday [p] (rem (quot (+ (long (g p :iJD)) 43200000) 86400000) 7))
(defn- days-after-sunday [p] (rem (quot (+ (long (g p :iJD)) 129600000) 86400000) 7))

(defn strftime-fn [args]
  (when (and (seq args) (not (nil? (first args))))
    (let [^String f (v/text-of (first args))]
      (when-let [p (is-date (vec (rest args)))]
        (compute-jd p) (compute-ymd-hms p)
        (let [sb (StringBuilder.)
              n (.length f)]
          (loop [i 0]
            (if (>= i n)
              (str sb)
              (let [c (.charAt f i)]
                (if (not= c \%)
                  (do (.append sb c) (recur (inc i)))
                  (if (>= (inc i) n)
                    nil ;; trailing % : unknown conversion
                    (let [k (.charAt f (inc i))
                          Y (long (g p :Y)) M (long (g p :M)) D (long (g p :D))
                          h (long (g p :h)) m (long (g p :m)) s (double (g p :s))
                          iJD (long (g p :iJD))
                          out
                          (case k
                            \d (fmt "%02d" D)
                            \e (fmt "%2d" D)
                            \f (fmt "%06.3f" (min s 59.999))
                            \F (fmt "%04d-%02d-%02d" Y M D)
                            \H (fmt "%02d" h)
                            \k (fmt "%2d" h)
                            (\I \l) (let [hh (if (> h 12) (- h 12) h) hh (if (zero? hh) 12 hh)]
                                      (fmt (if (= k \I) "%02d" "%2d") hh))
                            \j (fmt "%03d" (inc (days-after-jan01 p)))
                            \J (fmt "%.16g" (/ (double iJD) 86400000.0))
                            \m (fmt "%02d" M)
                            \M (fmt "%02d" m)
                            \p (if (>= h 12) "PM" "AM")
                            \P (if (>= h 12) "pm" "am")
                            \R (fmt "%02d:%02d" h m)
                            \s (if (g p :useSubsec)
                                 (fmt "%.3f" (/ (double (- iJD (* 21086676 10000000))) 1000.0))
                                 (fmt "%lld" (- (quot iJD 1000) (* 21086676 10000))))
                            \S (fmt "%02d" (long s))
                            \T (fmt "%02d:%02d:%02d" h m (long s))
                            (\u \w) (let [d (days-after-sunday p)] (str (if (and (zero? d) (= k \u)) 7 d)))
                            \U (fmt "%02d" (quot (+ (- (days-after-jan01 p) (days-after-sunday p)) 7) 7))
                            (\V \g \G)
                            (let [y (volatile! @p)]
                              ;; ISO-8601 week number: shift to the Thursday of this week
                              (vswap! y assoc :validJD false)
                              (vswap! y assoc :iJD (+ iJD (* (- 3 (days-after-monday p)) 86400000)))
                              (vswap! y assoc :validJD true :validYMD false)
                              (compute-ymd y)
                              (let [y2 (volatile! @y)]
                                (vswap! y2 assoc :validJD false :M 1 :D 1)
                                (compute-jd y2)
                                (let [nday (long (quot (+ (- (long (g y :iJD)) (long (g y2 :iJD))) 43200000) 86400000))]
                                  (case k
                                    \V (fmt "%02d" (inc (quot nday 7)))
                                    \g (fmt "%02d" (rem (long (g y :Y)) 100))
                                    \G (fmt "%04d" (long (g y :Y)))))))
                            \W (fmt "%02d" (quot (+ (- (days-after-jan01 p) (days-after-monday p)) 7) 7))
                            \Y (if (neg? Y) (fmt "-%03d" (- Y)) (fmt "%04d" Y))
                            \% "%"
                            nil)]
                      (if (nil? out)
                        nil
                        (do (.append sb ^String out) (recur (+ i 2)))))))))))))))

(defn timediff-fn [args]
  (when (= 2 (count args))
    (let [d1 (is-date [(first args)]) d2 (is-date [(second args)])]
      (when (and d1 d2)
        (compute-ymd-hms d1) (compute-ymd-hms d2)
        (let [recompute (fn [d] (s! d :validJD false) (compute-jd d))
              jd (fn [d] (long (g d :iJD)))]
          (if (>= (jd d1) (jd d2))
            (let [Y (- (long (g d1 :Y)) (long (g d2 :Y)))
                  _ (when-not (zero? Y) (s! d2 :Y (g d1 :Y)) (recompute d2))
                  M (- (long (g d1 :M)) (long (g d2 :M)))
                  [Y M] (if (neg? M) [(dec Y) (+ M 12)] [Y M])
                  _ (when-not (zero? M) (s! d2 :M (g d1 :M)) (recompute d2))
                  [Y M] (loop [Y Y M M]
                          (if (< (jd d1) (jd d2))
                            (let [M (dec M) [Y M] (if (neg? M) [(dec Y) 11] [Y M])]
                              (s! d2 :M (dec (long (g d2 :M))))
                              (when (< (long (g d2 :M)) 1) (s! d2 :M 12 :Y (dec (long (g d2 :Y)))))
                              (recompute d2)
                              (recur Y M))
                            [Y M]))
                  r (volatile! {:iJD (+ (- (jd d1) (jd d2)) 148699540800000) :validJD true :Y 0 :M 0 :D 0 :h 0 :m 0 :tz 0 :s 0.0})]
              (compute-ymd-hms r)
              (fmt (str "+%04d-%02d-%02d %02d:%02d:%06.3f") Y M (dec (long (g r :D))) (long (g r :h)) (long (g r :m)) (double (g r :s))))
            (let [Y (- (long (g d2 :Y)) (long (g d1 :Y)))
                  _ (when-not (zero? Y) (s! d2 :Y (g d1 :Y)) (recompute d2))
                  M (- (long (g d2 :M)) (long (g d1 :M)))
                  [Y M] (if (neg? M) [(dec Y) (+ M 12)] [Y M])
                  _ (when-not (zero? M) (s! d2 :M (g d1 :M)) (recompute d2))
                  [Y M] (loop [Y Y M M]
                          (if (< (jd d2) (jd d1))
                            (let [M (dec M) [Y M] (if (neg? M) [(dec Y) 11] [Y M])]
                              (s! d2 :M (inc (long (g d2 :M))))
                              (when (> (long (g d2 :M)) 12) (s! d2 :M 1 :Y (inc (long (g d2 :Y)))))
                              (recompute d2)
                              (recur Y M))
                            [Y M]))
                  r (volatile! {:iJD (+ (- (jd d2) (jd d1)) 148699540800000) :validJD true :Y 0 :M 0 :D 0 :h 0 :m 0 :tz 0 :s 0.0})]
              (compute-ymd-hms r)
              (fmt (str "-%04d-%02d-%02d %02d:%02d:%06.3f") Y M (dec (long (g r :D))) (long (g r :h)) (long (g r :m)) (double (g r :s))))))))))

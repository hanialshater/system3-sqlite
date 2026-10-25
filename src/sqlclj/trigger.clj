(ns sqlclj.trigger
  "Triggers: the trigger records, which triggers match a row event, and running a trigger program.
  NEW.x / OLD.x references in a trigger program are replaced by the row's values (literal nodes) each time
  the program runs, then the program's statements are executed as ordinary statements."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [sqlclj.engine :as e :refer [err lc]]
            [sqlclj.schema]))

(def exec-fn
  "Set by sqlclj.exec: (fn [db sql st out]) -> db, executes one statement."
  (atom nil))

(def ^:dynamic *active* #{})
(def ^:dynamic *depth* 0)
(def ^:dynamic *budget* nil)

(defn make-trigger
  "Trigger record from a parsed CREATE TRIGGER statement and the text its spans refer to."
  [st src]
  (select-keys (assoc st :src src :table-name (:table st)) [:name :timing :event :cols :when :body :src :temp :table-name :table-schema :schema :db]))

(defn matching
  "Triggers of table tkey for the event, newest first. cols: lower-case names of the columns an UPDATE sets."
  [db tkey timing event cols]
  (when (seq (:triggers db))
    (let [ttemp (boolean (:temp (or (get-in db [:tables tkey]) (get-in db [:views tkey]))))
          ts (->> (vals (:triggers db))
                  (filter (fn [t] (and (= (:table t) tkey) (= (:timing t) timing) (= (:event t) event)
                                       (or (not= event :update) (nil? (:cols t))
                                           (some (fn [c] (some #(= (lc c) %) cols)) (:cols t)))))))
          foreign? (fn [t] (and (:temp t) (not ttemp)))]
      ;; SQLite lists the TEMP triggers of a non-temp table first, oldest first; then the table's own, newest first
      (concat (sort-by :mseq (filter foreign? ts))
              (sort-by :mseq > (remove foreign? ts))))))

(defn any-for? [db tkey event]
  (boolean (some (fn [t] (and (= (:table t) tkey) (= (:event t) event))) (vals (:triggers db)))))

(defn- col-value [tbl row nm side]
  (let [l (lc nm)
        i (.indexOf ^java.util.List (mapv (comp lc :name) (:cols tbl)) l)]
    (cond (>= i 0) (nth row i)
          (and (not (:view tbl)) (#{"rowid" "oid" "_rowid_"} l)) (peek row)
          :else (err (str "no such column: " side "." nm)))))

(defn- col-meta [tbl nm]
  (let [l (lc nm)
        i (.indexOf ^java.util.List (mapv (comp lc :name) (:cols tbl)) l)]
    (when-not (:view tbl)
      (cond (>= i 0) (let [c (nth (:cols tbl) i)]
                       {:tmeta true :taff (when (= i (:ipk tbl)) :integer) :tcoll (or (:coll c) "BINARY")})
            (#{"rowid" "oid" "_rowid_"} l) {:tmeta true :taff :integer :tcoll nil}))))

(defn- substitute [ast tbl old new]
  (walk/postwalk
   (fn [x]
     (if (and (map? x) (= :col (:op x)) (:table x))
       (let [t (lc (:table x))]
         (cond (and (= t "new") new) (merge (assoc (select-keys x [:ps :pe]) :op :lit :v (col-value tbl new (:name x) "new")) (col-meta tbl (:name x)))
               (and (= t "old") old) (merge (assoc (select-keys x [:ps :pe]) :op :lit :v (col-value tbl old (:name x) "old")) (col-meta tbl (:name x)))
               :else x))
       x))
   ast))

(defn- run-program
  [db trig tbl old new]
  (let [src (:src trig)
        changes (:changes db)
        last-rowid (:last-rowid db)
        ctx (assoc (e/->Ctx db src nil (atom 0) nil nil nil) :refs (atom #{}))
        ok (if-let [w (:when trig)]
             (let [c (binding [e/*in-trigger* true] (e/cx ctx (substitute w tbl old new)))
                   x ((e/condf c) (object-array (+ 2 @(:slots ctx))))]
               (and (not (nil? x)) (sqlclj.values/bool-value x)))
             true)]
    (if-not ok
      db
      (binding [e/*in-trigger* true
                sqlclj.schema/*home-db* (when-not (:temp trig) (:db trig))
                *active* (conj *active* (lc (:name trig)))
                *depth* (inc *depth*)]
        (when (> *depth* 1000) (err "too many levels of trigger recursion"))
        (when (and *budget* (> (swap! *budget* inc) 3000)) (err "too many levels of trigger recursion"))
        (let [sb (StringBuilder.)
              db2 (reduce (fn [db st] (@exec-fn db src (substitute st tbl old new) sb)) db (:body trig))]
          (assoc db2 :changes changes :last-rowid last-rowid))))))

(def ^:dynamic *validating* nil)

(def ^:private compile-error-re
  #"^(no such (table|column|function|index|view|collation sequence)|table .* has (no column|\d+ columns but)|\d+ values for \d+ columns|wrong number of arguments|misuse of aggregate|ambiguous column|too many levels|trigger cannot use)")

(defn validate-for!
  "SQLite compiles the programs of the triggers of a statement when it prepares it, so mistakes in them (a dropped
  table, a wrong column count) fail the statement before it touches a row. Here the programs run once against
  a row of NULLs on the immutable db and the result is dropped; only compile-time style errors count."
  [db tkey tbl event setcols]
  (when (and (seq (:triggers db)) (or (zero? *depth*) *validating*))
    (let [trigs (concat (matching db tkey :before event setcols) (matching db tkey :after event setcols) (matching db tkey :instead event setcols))
          blank (vec (repeat (inc (count (:cols tbl))) nil))]
      (doseq [t trigs :when (not (contains? *validating* (lc (:name t))))]
        (binding [*validating* (conj (or *validating* #{}) (lc (:name t)))
                  *budget* (atom 0)]
          (try (run-program db (dissoc t :when) tbl (when (#{:update :delete} event) blank) (when (#{:insert :update} event) blank))
               (catch clojure.lang.ExceptionInfo ex
                 (let [m (:sql-error (ex-data ex))]
                   (when (and m (re-find compile-error-re m)) (throw ex))))))))))

(defn fire
  "Runs the triggers trigs for one row event. old/new: row vectors (rowid last) or nil. Returns the new db.
  A RAISE(IGNORE) propagates as an exception with :raise-ignore in its ex-data."
  [db trigs tbl old new]
  (binding [*budget* (or *budget* (atom 0))]
    (reduce (fn [db trig]
              (if (and (contains? *active* (lc (:name trig)))
                       (not= 1 (get-in db [:pragmas "recursive_triggers"])))
                db
                (run-program db trig tbl old new)))
            db trigs)))

(defn ignore? [^Throwable ex] (boolean (:raise-ignore (ex-data ex))))

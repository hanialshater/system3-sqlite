(ns sqlclj.trigger
  "Triggers: the trigger records, which triggers match a row event, and running a trigger program.
  NEW.x / OLD.x references in a trigger program are replaced by the row's values (literal nodes) each time
  the program runs, then the program's statements are executed as ordinary statements."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [sqlclj.engine :as e :refer [err lc]]))

(def exec-fn
  "Set by sqlclj.exec: (fn [db sql st out]) -> db, executes one statement."
  (atom nil))

(def ^:dynamic *active* #{})
(def ^:dynamic *depth* 0)
(def ^:dynamic *budget* nil)

(defn make-trigger
  "Trigger record from a parsed CREATE TRIGGER statement and the text its spans refer to."
  [st src]
  (select-keys (assoc st :src src :table-name (:table st)) [:name :timing :event :cols :when :body :src :temp :table-name :table-schema :schema]))

(defn matching
  "Triggers of table tkey for the event, newest first. cols: lower-case names of the columns an UPDATE sets."
  [db tkey timing event cols]
  (when (seq (:triggers db))
    (->> (vals (:triggers db))
         (filter (fn [t] (and (= (:table t) tkey) (= (:timing t) timing) (= (:event t) event)
                              (or (not= event :update) (nil? (:cols t))
                                  (some (fn [c] (some #(= (lc c) %) cols)) (:cols t))))))
         (sort-by :mseq >))))

(defn any-for? [db tkey event]
  (boolean (some (fn [t] (and (= (:table t) tkey) (= (:event t) event))) (vals (:triggers db)))))

(defn- col-value [tbl row nm side]
  (let [l (lc nm)
        i (.indexOf ^java.util.List (mapv (comp lc :name) (:cols tbl)) l)]
    (cond (>= i 0) (nth row i)
          (and (not (:view tbl)) (#{"rowid" "oid" "_rowid_"} l)) (peek row)
          :else (err (str "no such column: " side "." nm)))))

(defn- col-def [tbl nm]
  (let [l (lc nm)] (first (filter #(= l (lc (:name %))) (:cols tbl)))))

(defn- lit-of [x tbl row nm side]
  ;; NEW.x / OLD.x keep the column's affinity and collating sequence
  (let [cd (col-def tbl nm)]
    (cond-> (assoc (select-keys x [:ps :pe]) :op :lit :v (col-value tbl row nm side))
      cd (assoc :aff (:aff cd) :coll (or (:coll cd) "BINARY")))))

(defn- substitute [ast tbl old new]
  (walk/postwalk
   (fn [x]
     (if (and (map? x) (= :col (:op x)) (:table x))
       (let [t (lc (:table x))]
         (cond (and (= t "new") new) (lit-of x tbl new (:name x) "new")
               (and (= t "old") old) (lit-of x tbl old (:name x) "old")
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
             (let [c (e/cx ctx (substitute w tbl old new))
                   x ((e/condf c) (object-array (+ 2 @(:slots ctx))))]
               (and (not (nil? x)) (sqlclj.values/bool-value x)))
             true)]
    (if-not ok
      db
      (binding [e/*in-trigger* true
                *active* (conj *active* (lc (:name trig)))
                *depth* (inc *depth*)]
        (when (> *depth* 1000) (err "too many levels of trigger recursion"))
        (when (and *budget* (> (swap! *budget* inc) 3000)) (err "too many levels of trigger recursion"))
        (let [sb (StringBuilder.)
              db2 (reduce (fn [db st] (@exec-fn db src (substitute st tbl old new) sb)) db (:body trig))]
          (assoc db2 :changes changes :last-rowid last-rowid))))))

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

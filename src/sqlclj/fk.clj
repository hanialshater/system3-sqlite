(ns sqlclj.fk
  "Foreign key enforcement. Parent-side actions (CASCADE / SET NULL / SET DEFAULT / RESTRICT) run per row as
  synthesized DELETE / UPDATE statements; the NO ACTION / child-side checks run at the end of a statement over the
  rows it touched (deferred constraints are checked at COMMIT by a full scan)."
  (:require [clojure.string :as str]
            [sqlclj.engine :as e :refer [err lc]]
            [sqlclj.values :as v]
            [sqlclj.trigger :as trg]))

(def ^:dynamic *child* nil)   ; atom: [[tkey old new] ...] rows inserted/updated in tables that have foreign keys
(def ^:dynamic *parent* nil)  ; atom: [[ptbl old new] ...] rows deleted / updated in tables that others reference

(defn enabled? [db] (= 1 (get-in db [:pragmas "foreign_keys"])))

(defn- col-index [tbl n]
  (let [i (.indexOf ^java.util.List (mapv (comp lc :name) (:cols tbl)) (lc n))]
    (when (>= i 0) i)))

(defn parent-key
  "Storage key of the parent table of fk: it lives in the child's own database."
  [db child fk]
  (let [d (:db child) l (lc (:table fk))]
    (if d
      (let [k (str d "." l)] (if (or (get-in db [:tables k]) (not= d "main")) k l))
      l)))

(defn parent-cols
  "Column indexes of the parent key an fk refers to; errors with 'foreign key mismatch' when it is not a key."
  [db child fk]
  (let [ptbl (get-in db [:tables (parent-key db child fk)])
        mismatch #(err (str "foreign key mismatch - \"" (:name child) "\" referencing \"" (:table fk) "\""))]
    (when-not ptbl (err (str "no such table: " (if (and (:db child) (not= "main" (:db child))) (:db child) "main") "." (:table fk))))
    (if-let [to (:to fk)]
      (let [idxs (mapv #(or (col-index ptbl %) (mismatch)) to)
            ok (or (and (= 1 (count idxs)) (= (first idxs) (:ipk ptbl)))
                   (and (= (set idxs) (set (:pk-cols ptbl))) (seq (:pk-cols ptbl)))
                   (some (fn [ix] (and (:unique ix) (nil? (:where ix)) (every? :col (:cols ix))
                                       (= (set (map :col (:cols ix))) (set idxs))))
                         (filter #(= (:table %) (lc (:name ptbl))) (vals (:indexes db)))))]
        (when-not ok (mismatch))
        idxs)
      (let [pk (:pk-cols ptbl)]
        (when (or (empty? pk) (not= (count pk) (count (:from fk)))) (mismatch))
        (vec pk)))))

(declare parent-touched? referencing)
(defn locate-all!
  "SQLite locates the parent key of every foreign key referring to ptbl as soon as one of them is affected
  by a change (update old->new, or a delete when new is nil); an unusable key is 'foreign key mismatch'."
  [db ptbl old new]
  (let [refs (referencing db ptbl)]
    (when (some (fn [[c fk]] (parent-touched? db c fk old new)) refs)
      (doseq [[c fk] refs] (parent-cols db c fk)))))

(defn parent-touched?
  "Whether an UPDATE old->new changes a column of the parent key fk refers to (always true for a delete)."
  [db child fk old new]
  (or (nil? new)
      (let [ptbl (get-in db [:tables (parent-key db child fk)])
            raw (if-let [to (:to fk)] (map #(col-index ptbl %) to) (:pk-cols ptbl))]
        (or (empty? raw)
            (some (fn [pi] (or (nil? pi)
                               (let [a (nth old pi) b (nth new pi)]
                                 (not (or (and (nil? a) (nil? b))
                                          (and (some? a) (some? b) (zero? (v/sql-cmp a b "BINARY"))))))))
                  raw)))))

(defn- values-eq? [a b coll] (and (not (nil? a)) (not (nil? b)) (zero? (v/sql-cmp a b coll))))

(defn- coerce [val ptbl pi] (v/apply-affinity val (:aff (nth (:cols ptbl) pi))))

(defn- key-match? [row cidxs prow pidxs ptbl]
  (every? true? (map (fn [ci pi]
                       (values-eq? (coerce (nth row ci) ptbl pi) (nth prow pi) (or (:coll (nth (:cols ptbl) pi)) "BINARY")))
                     cidxs pidxs)))

(defn parent-exists? [db child fk row]
  (let [ptbl (get-in db [:tables (parent-key db child fk)])
        pidxs (parent-cols db child fk)
        cidxs (mapv #(col-index child %) (:from fk))]
    (some (fn [[_ prow]] (key-match? row cidxs prow pidxs ptbl)) (:rows ptbl))))

(defn- deferred? [db fk]
  (or (:deferred fk) (= 1 (get-in db [:pragmas "defer_foreign_keys"]))))

(defn referencing
  "[child-tbl fk] pairs whose foreign key refers to table ptbl (by name)."
  [db ptbl]
  (for [[_ c] (:tables db), fk (:fks c) :when (= (parent-key db c fk) (lc (:name ptbl)))] [c fk]))

(defn has-fks? [tbl] (boolean (seq (:fks tbl))))

(defn tracks?
  "Does a change to this table matter to foreign keys at all?"
  [db tbl]
  (and (enabled? db)
       (or (has-fks? tbl) (boolean (seq (referencing db tbl))))))

(defn- lit-node [x] {:op :lit :v x})
(defn- eq-where [cols vals]
  (reduce (fn [acc [c x]]
            (let [t {:op :bin :o :eq :l {:op :col :name c} :r (lit-node x)}]
              (if acc {:op :bin :o :and :l acc :r t} t)))
          nil (map vector cols vals)))

(defn- run-stmt [db st]
  (let [ch (:changes db) lr (:last-rowid db)
        db2 (@trg/exec-fn db "" st (StringBuilder.))]
    (assoc db2 :changes ch :last-rowid lr)))

(defn- children-rows [db child fk prow ptbl pidxs]
  (let [cidxs (mapv #(col-index child %) (:from fk))]
    (filter (fn [[_ crow]] (key-match? crow cidxs prow pidxs ptbl)) (:rows child))))

(defn on-parent-change
  "Applies the ON DELETE / ON UPDATE actions for a parent row that is deleted (new nil) or updated.
  Returns the new db."
  [db ptbl old new]
  (if-not (enabled? db)
    db
    (reduce
     (fn [db [child fk]]
       (let [_ (locate-all! db ptbl old new)
             pidxs (if (parent-touched? db child fk old new) (parent-cols db child fk) [])
             changed? (or (nil? new) (not (every? true? (map (fn [pi] (let [a (nth old pi) b (nth new pi)]
                                                                       (or (and (nil? a) (nil? b)) (and (not (nil? a)) (not (nil? b)) (zero? (v/sql-cmp a b "BINARY"))))))
                                                              pidxs))))
             child (get-in db [:tables (lc (:name child))])]
         (if-not (and (seq pidxs) changed? (some (fn [pi] (not (nil? (nth old pi)))) pidxs))
           db
           (let [kids (children-rows db child fk old ptbl pidxs)
                 act (if (nil? new) (or (:on-delete fk) :no-action) (or (:on-update fk) :no-action))
                 ccols (:from fk)]
             (if (empty? kids)
               db
               (case act
                 :restrict (err "FOREIGN KEY constraint failed")
                 :cascade
                 (if (nil? new)
                   (run-stmt db {:kind :delete :table (:name child) :where (eq-where ccols (map #(nth old %) pidxs))})
                   (run-stmt db {:kind :update :table (:name child)
                                 :sets (mapv (fn [c pi] [[c] (lit-node (nth new pi))]) ccols pidxs)
                                 :where (eq-where ccols (map #(nth old %) pidxs))}))
                 :set-null
                 (run-stmt db {:kind :update :table (:name child)
                               :sets (mapv (fn [c] [[c] (lit-node nil)]) ccols)
                               :where (eq-where ccols (map #(nth old %) pidxs))})
                 :set-default
                 (run-stmt db {:kind :update :table (:name child)
                               :sets (mapv (fn [c] [[c] (or (:default (nth (:cols child) (col-index child c))) (lit-node nil))]) ccols)
                               :where (eq-where ccols (map #(nth old %) pidxs))})
                 db))))))
     db (referencing db ptbl))))

(defn- violation?
  "Does the end state violate a foreign key because of the touched rows? Returns the fk if deferred-violation info."
  [db tbl fk row]
  (let [cidxs (mapv #(col-index tbl %) (:from fk))]
    (and (every? #(not (nil? (nth row %))) cidxs)
         (not (parent-exists? db tbl fk row)))))

(defn verify
  "End-of-statement check over the touched rows. Raises 'FOREIGN KEY constraint failed' for immediate violations."
  [db child-log parent-log]
  (when (enabled? db)
    (let [explicit-tx (boolean (:tx db))
          fail! (fn [fk] (when-not (and (deferred? db fk) explicit-tx)
                           (throw (ex-info "FOREIGN KEY constraint failed" {:sql-error "FOREIGN KEY constraint failed" :fk-deferred (boolean (deferred? db fk))}))))]
      ;; child side
      (doseq [[tkey old new] child-log]
        (let [tbl (get-in db [:tables tkey])]
          (when (and tbl new)
            (doseq [fk (:fks tbl)]
              (let [cidxs (mapv #(col-index tbl %) (:from fk))]
                (when (or (nil? old) (not= (mapv #(nth old %) cidxs) (mapv #(nth new %) cidxs)))
                  (when (violation? db tbl fk new) (fail! fk))))))))
      ;; parent side (NO ACTION): children still pointing at a key that no longer exists
      (doseq [[ptname old new] parent-log]
        (let [ptbl (get-in db [:tables ptname])]
          (when ptbl
            (when old (locate-all! db ptbl old new))
            (doseq [[child fk] (referencing db ptbl)
                    :when (if old (parent-touched? db child fk old new)
                              (or (> (count (filter (fn [[_ o _]] (nil? o)) parent-log)) 1) (deferred? db fk)))]
              (let [pidxs (parent-cols db child fk)
                    child (get-in db [:tables (lc (:name child))])]
                (when (some (fn [pi] (not (nil? (nth old pi)))) pidxs)
                  (when (and (seq (children-rows db child fk old ptbl pidxs))
                             (not (some (fn [[_ prow]] (every? true? (map (fn [pi] (values-eq? (nth old pi) (nth prow pi) "BINARY")) pidxs)))
                                        (:rows ptbl))))
                    (fail! fk)))))))))))

(defn check-all
  "Violating rows of all (or one, by storage key) child tables: [table rowid parent fkid]. Newest tables first.
  in-scope?: optional table predicate (the PRAGMA's database)."
  [db tkey & [in-scope?]]
  (vec
   (for [[k tbl] (sort-by (fn [[_ t]] (- (long (or (:mseq t) 0)))) (:tables db))
         :when (and (seq (:fks tbl)) (or (nil? tkey) (= tkey k)) (or (nil? in-scope?) (in-scope? tbl)))
         [fkid fk] (map-indexed vector (reverse (:fks tbl)))
         :let [ptbl (get-in db [:tables (parent-key db tbl fk)])
               _ (when ptbl (parent-cols db tbl fk))
               cidxs (mapv #(col-index tbl %) (:from fk))]
         [rid row] (:rows tbl)
         :when (and (every? #(not (nil? (nth row %))) cidxs)
                    (or (nil? ptbl) (not (parent-exists? db tbl fk row))))]
     [(let [n (:name tbl) d (:db tbl)] (if (and d (str/starts-with? (lc n) (str d "."))) (subs n (inc (count d))) n))
      (when-not (:without-rowid tbl) rid) (:table fk) (long fkid)])))

(defn commit-check
  "At COMMIT: deferred constraints must hold."
  [db]
  (when (enabled? db)
    (doseq [[_ tbl] (:tables db) fk (:fks tbl)
            :when (get-in db [:tables (parent-key db tbl fk)])]
      (doseq [[_ row] (:rows tbl)]
        (when (violation? db tbl fk row)
          (err "FOREIGN KEY constraint failed"))))))

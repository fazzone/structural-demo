(ns cmd.edit
  (:require
   [datascript.core :as d]
   [embed :as e]
   [core :refer [get-selected-form move-selection-tx]]))

(defn form-delete-tx
  [e]
  (let [spine (first (:seq/_first e))
        next  (some-> spine :seq/next)
        prev  (some-> spine :seq/_next first)]
    (into [[:db/retractEntity (:db/id e)]
           [:db/retract (:db/id spine) :seq/first (:db/id e)]]
          (cond
            (and prev next)             ; middle element
            [[:db/retract (:db/id spine) :seq/next (:db/id next)]
             [:db/add (:db/id prev) :seq/next (:db/id next)]]
       
            prev                        ; last element
            [[:db/retract (:db/id prev) :seq/next (:db/id spine)]
             [:db/retract (:db/id spine) :seq/next (:db/id next)]]
       
            next                        ; first element
            [[:db/add (:db/id spine) :seq/first (:db/id (:seq/first next))]
             ;; keep our spine, retract next spine - preserve coll/type etc
             [:db/retractEntity (:db/id next)]
             (if-let [n (:seq/next next)]
               [:db/add (:db/id spine) :seq/next (:db/id n)]
               [:db/retract (:db/id spine) :seq/next (:db/id next)])]))))

;; overwrite with something that has not existed before
;; if you have a tempid, you want this one
(defn form-overwrite-tx
  "rplaca"
  [e replacement-eid]
  (let [spine (first (:seq/_first e))
        coll (first (:coll/_contains e))]
    #_(println "Overwrite" spine coll)
    (when (and spine coll)
      [[:db/retract (:db/id coll) :coll/contains (:db/id e)]
       [:db/add (:db/id coll) :coll/contains replacement-eid]
       [:db/add (:db/id spine) :seq/first replacement-eid]])))

;; replace with something from somewhere else
;; if you have a second entity, you want this one
(defn form-replace-tx
  [e r]
  (let [old-parent (first (:coll/_contains r))
        old-spine (first (:seq/_first r))]
    (into (form-overwrite-tx e (:db/id r))
          [(when old-spine
             [:db/retract (:db/id old-spine) :seq/first (:db/id r)])
           (when old-parent
             [:db/retract (:db/id old-parent) :coll/contains (:db/id r)])])))

(defn form-raise-tx
  [e]
  (when-let [parent (some-> (:coll/_contains e) first)]
    (when-not (= :chain (:coll/type parent))
      (into [[:db/add (:db/id parent) :form/edited-tx :db/current-tx]]
            (form-replace-tx parent e)))))




(defn insert-after-tx
  [target new-node]
  (when-let [spine (first (:seq/_first target))]
    (when-let [coll (first (:coll/_contains target))]
      [{:db/id (:db/id spine)
        :seq/next {:db/id "cons"
                   :seq/first (:db/id new-node)}}
       (when-let [old-next (:seq/next spine)]
         [:db/add "cons" :seq/next (:db/id old-next)])
       {:db/id (:db/id coll)
        :coll/contains (:db/id new-node)}])))

(defn insert-before-tx
  [target new-node]
  (let [spine (first (:seq/_first target))
        coll (first (:coll/_contains target))]
    #_(prn 'spine (:db/id spine) 'cloll (:db/id coll))
    (when (and spine coll)
      [{:db/id (:db/id spine)
        :seq/first (:db/id new-node)
        :seq/next {:db/id "insert-before-cons"
                   :seq/first (:db/id target)}}
       [:db/add (:db/id coll) :coll/contains (:db/id new-node)]
       (when-let [next (:seq/next spine)]
         [:db/add "insert-before-cons" :seq/next (:db/id next) ])])))



(defn insert-editing-tx
  [db before-or-after edit-initial]
  (let [sel (get-selected-form db)
        new-node {:db/id "newnode"
                  :form/editing true
                  :form/edit-initial (or edit-initial "")}
        itx (case before-or-after
              :before (insert-before-tx sel new-node)
              :after (insert-after-tx sel new-node))]
    (into [new-node]
          (concat itx (move-selection-tx (:db/id sel) "newnode")))))




(defn exactly-one
  [[x & more :as xs]]
  (when more
    (throw (ex-info "More than one" {:xs xs}))
    #_(println "More than one" xs))
  x)

(defn exchange-with-previous-tx
  [sel]
  (let [spine  (exactly-one (:seq/_first sel))
        prev   (some-> spine :seq/_next exactly-one)
        next   (some-> spine :seq/next)
        parent (first (:coll/_contains sel))]
    (when (and prev parent)
      [[:db/add (:db/id prev) :seq/first (:db/id sel)]
       [:db/add (:db/id spine) :seq/first (:db/id (:seq/first prev))]
       [:db/add (:db/id parent) :form/edited-tx :db/current-tx]])))

(defn exchange-with-next-tx
  [sel]
  (let [spine  (first (:seq/_first sel))
        next   (some-> spine :seq/next)
        parent (first (:coll/_contains sel))]
    (when (and next parent)
      [[:db/add (:db/id next) :seq/first (:db/id sel)]
       [:db/add (:db/id spine) :seq/first (:db/id (:seq/first next))]
       [:db/add (:db/id parent) :form/edited-tx :db/current-tx]])))

(defn form-duplicate-tx
  [e]
  (letfn [(dup-spine [parent head] 
            (if-let [x (:seq/first head)]
              (cond-> {:seq/first (assoc (form-duplicate-tx x) :coll/_contains parent )}
                (:seq/next head) (assoc :seq/next (dup-spine parent (:seq/next head))))))]
    (cond 
      (:symbol/value e)  {:symbol/value (:symbol/value e)}
      (:keyword/value e) {:keyword/value (:keyword/value e)}
      (:string/value e)  {:string/value (:string/value e)}
      (:number/value e)  {:number/value (:number/value e)}
      (:coll/type e)     (let [us (e/new-tempid)]
                           (merge (cond-> {:db/id us :coll/type (:coll/type e)}
                                    (some? (:form/indent e)) (assoc :form/indent (:form/indent e))
                                    (some? (:form/linebreak e)) (assoc :form/linebreak (:form/linebreak e)))
                                  (dup-spine us e))))))

(defn insert-duplicate-tx
  [db]
  (let [sel (get-selected-form db)
        new-node (-> (form-duplicate-tx sel)
                     (update :db/id #(or % "dup-leaf")))]
    (into [new-node]
          (insert-after-tx sel new-node))))

(defn form-wrap-tx
  [e ct]
  (into [{:db/id "newnode"
          :coll/type ct
          :coll/contains (:db/id e)
          :seq/first (:db/id e)}]
        (form-overwrite-tx e "newnode")))

(defn wrap-and-edit-first-tx
  [sel ct]
  (let [spine (first (:seq/_first sel))
        coll (first (:coll/_contains sel))
        new-node {:db/id "first"
                  :coll/type ct
                  :coll/_contains (:db/id coll)
                  :seq/first {:db/id "funcname"
                              :coll/_contains "first"
                              :form/editing true}
                  :seq/next {:seq/first (:db/id sel)}}]
    (into [new-node]
          (concat
           (form-overwrite-tx sel "first")
           (move-selection-tx (:db/id sel) "first")))))


(defn begin-edit-existing-node-tx
  [e]
  [[:db/add (:db/id e) :form/editing true]])

(defn edit-selected-tx
  [db]
  (let [sel (get-selected-form db)]
    (when-not (:coll/type sel)
     (begin-edit-existing-node-tx sel))))


(defn edit-new-wrapped-tx
  [db coll-type init]
  (let [sel (get-selected-form db)
        new-node {:db/id "newnode"
                  :coll/type coll-type
                  :coll/contains "inner" 
                  :seq/first {:db/id "inner"
                              :coll/_contains "newnode"
                              :form/edit-initial (or init "")
                              :form/editing true}}]
    (into [new-node]
          (concat (insert-after-tx sel new-node)
                  (move-selection-tx (:db/id sel) "inner")))))


(defn move-last*
  [e]
  (if-let [n (:seq/next e)]
    (recur n)
    (:seq/first e)))

(defn slurp-right-tx
  [e]
  (let [spine     (some-> e :seq/_first exactly-one)
        coll      (some-> e :coll/_contains exactly-one)
        next      (some-> spine :seq/next)
        nnext     (some-> next :seq/next)
        last-cons (loop [s e]
                    (if-let [n (:seq/next s)]
                      (recur n)
                      s))]
    (cond
      (nil? (:coll/type e)) (recur coll)
      (nil? next)           nil
      :else
      [[:db/retract (:db/id coll) :coll/contains (:db/id (:seq/first next))]
       [:db/add (:db/id e) :coll/contains (:db/id (:seq/first next))]
       (when nnext
         [:db/retract (:db/id next) :seq/next (:db/id nnext)])
       (if nnext
         [:db/add (:db/id spine) :seq/next (:db/id nnext)]
         [:db/retract (:db/id spine) :seq/next (:db/id next)])
       [:db/add (:db/id last-cons) :seq/next (:db/id next)]])))

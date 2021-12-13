(ns cmd.edit
  (:require
   [datascript.core :as d]
   [embed :as e]
   [core :refer [get-selected-form
                 move-selection-tx]]))

(defn form-delete-tx
  [e]
  (let [spine (first (:seq/_first e))
        next  (some-> spine :seq/next)
        prev  (some-> spine :seq/_next first)]
    (into [[:db/retractEntity (:db/id e)]]
     (cond
       (and prev next) [[:db/add (:db/id prev) :seq/next (:db/id next)]]
       prev            [[:db/retract (:db/id prev) :seq/next (:db/id spine)]]
       next            [(when-let [f (:seq/first next)]
                          [:db/add (:db/id spine) :seq/first (:db/id f)])
                        (if-let [n (:seq/next next)]
                          [:db/add (:db/id spine) :seq/next (:db/id n)]
                          (when (:seq/next spine)
                            [:db/retract (:db/id spine) :seq/next (:db/id next)]))]))))

;; overwrite with something that has not existed before
;; if you have a tempid, you want this one
(defn form-overwrite-tx
  "rplaca"
  [e replacement-eid]
  (let [spine (first (:seq/_first e))
        coll (first (:coll/_contains e))]
    (println "Overwrite" spine coll)
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

(defn raise-selected-form-tx
  [db]
  (form-raise-tx (get-selected-form db)))


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
    (prn 'spine (:db/id spine) 'cloll (:db/id coll))
    (println  )
    (when (and spine coll)
      [{:db/id (:db/id spine)
        :seq/first (:db/id new-node)
        :seq/next {:db/id "insert-before-cons"
                   :seq/first (:db/id target)}}
       [:db/add (:db/id coll) :coll/contains (:db/id new-node)]
       (when-let [next (:seq/next spine)]
         [:db/add "insert-before-cons" :seq/next (:db/id next) ])
       
       ])))



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

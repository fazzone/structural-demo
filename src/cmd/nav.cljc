(ns cmd.nav
  (:require
   [cmd.move :as move]
   [core :as core
    :refer [get-selected-form
            move-selection-tx]]))

(defn select-form-tx
  [db eid]
  (move-selection-tx (:db/id (get-selected-form db))
                     eid))

(defn parents-seq
  [e]
  (->> e
       (iterate (partial move/move :move/up))
       (take-while some?)))

(defn parents-vec
  [e]
  (vec (parents-seq e)))

(defn hop-target
  [chain]
  (or (:chain/selection chain) chain))

(defn hop*
  [mover db]
  (let [sel (get-selected-form db)]
    (if (= :chain (:coll/type sel))
      (do #_(println "Hop from chain")
          (when-let [hop-chain (mover sel)]
            (move-selection-tx (:db/id sel) (:db/id (hop-target hop-chain)))))
      (let [chain (some-> sel parents-vec peek :coll/_contains first)]
        #_(println "The chain is" (d/touch chain))
        (when-let [hop-chain (mover chain)]
          #_(println "The hop-chain is" (d/touch hop-chain) "The tarrget is " (hop-target hop-chain))
          (concat (move-selection-tx (:db/id sel) (:db/id (hop-target hop-chain)))
                  [{:db/id (:db/id chain)
                    :chain/selection (:db/id sel)}]))))))

(defn seq-prev [e] (some-> e :seq/_first first :seq/_next first :seq/first))
(defn seq-next [e] (some-> e :seq/_first first :seq/next :seq/first))

(def hop-left (partial hop* seq-prev))
(def hop-right (partial hop* seq-next))

(defn select-chain-tx
  [sel]
  (if-let [cs (:chain/selection sel)] 
    (move-selection-tx (:db/id sel) (:db/id cs))
    (let [top-level (peek (parents-vec sel))
          chain (some-> top-level :coll/_contains first)]
      (when (= :chain (:coll/type chain))
       (concat (move-selection-tx (:db/id sel) (:db/id chain))
               [{:db/id (:db/id chain)
                 :chain/selection (:db/id sel)}])))))

(defn restore-chain-selection-tx
  [db chain]
  (when-let [prev-sel (:chain/selection chain)]
    (select-form-tx db (:db/id prev-sel))))




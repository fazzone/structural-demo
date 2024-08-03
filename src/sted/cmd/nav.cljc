(ns sted.cmd.nav
  (:require
   [sted.cmd.move :as move]
   [datascript.core :as d]
   [sted.core :as core :refer [get-selected-form
                               move-selection-tx]]))


(defn select-form-tx
  [db eid]
  (move-selection-tx (:db/id (get-selected-form db))
                     eid))


(defn parents-seq
  [e]
  (->> e
       (iterate move/up)
       (take-while some?)))


(defn parents-vec
  [e]
  (vec (parents-seq e)))


(defn hop-target
  [chain]
  (or (:chain/selection chain) chain))


(defn hop*
  [mover sel]
  (if (= :chain (:coll/type sel))
    (do #_(println "Hop from chain")
        (when-let [hop-chain (mover sel)]
          (move-selection-tx (:db/id sel) (:db/id (hop-target hop-chain)))))
    (let [chain (some-> sel parents-vec peek :coll/_contains first)]
      #_(println "The chain is" (d/touch chain))
      (when-let [hop-chain (mover chain)]
        #_(println "The hop-chain is" (d/touch hop-chain) "The tarrget is " (hop-target hop-chain))
        (concat (move-selection-tx (:db/id sel)
                                   (let [csel (:chain/selection hop-chain)]
                                     (cond (= (:db/id csel) (:db/id sel))
                                             (do #_"You cannot hop to yourself, you split the chain didn't you?"
                                                 (:db/id hop-chain))
                                           csel (:db/id csel)
                                           :else (:db/id hop-chain))))
                [{:db/id (:db/id chain)
                  :chain/selection (:db/id sel)}])))))


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


(defn lazy-bfs
  [top]
  ((fn iter [[e :as front]]
     (when (some? e)
       (cond->> (lazy-seq (iter (cond-> (subvec front 1)
                                  (:seq/next e)  (conj (:seq/next e))
                                  (:seq/first e) (conj (:seq/first e)))))
         (or (:coll/type e) (:token/type e)) (cons e))))
   [top]))


(defn no-double-colls
  [nmv]
  ((fn iter [[x y :as xs] firsts]
     (when x
       (let [fid (:db/id (:seq/first x))
             add? (not (contains? firsts (:db/id x)))]
         (cond->> (lazy-seq (iter (next xs) (conj firsts fid)))
           add? (cons x)))))
   nmv #{}))


(defn get-numeric-movement-vec
  [sel]
  (let [children (next (take 8 (no-double-colls (lazy-bfs sel))))]
    (if (move/up sel)
      (into [(peek (parents-vec sel))] children)
      (into [nil] children))))


(defn numeric
  [n sel]
  (when-let [nmv (get-numeric-movement-vec sel)]
    (when (< -1 n (count nmv))
      (nth nmv n))))
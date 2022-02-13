(ns sted.cmd.move
  (:require
   [sted.embed :as e]
   [sted.core :refer [get-selected-form
                      move-selection-tx]]))

#_(defmulti move (fn [t _] t))

#_(defmethod move :move/most-nested [_ e]
  (last (tree-seq :coll/type e/seq->vec e)))

(defn most-nested [e]
  (last (tree-seq :coll/type e/seq->vec e)))

#_(defmethod move :move/most-upward [_ e]
  (->> e
       (iterate (partial move :move/up))
       (take-while some?)
       last))

(defn up [e]
  (when-let [cs (:coll/_contains e)]
    (let [up (first cs)]
      (when-not (= :chain (:coll/type up))
        up))))

(defn most-upward [e]
  (->> e
       (iterate up)
       (take-while some?)
       (last)))

#_(defmethod move :move/next-sibling [_ e]
  (some-> (:seq/_first e) first :seq/next :seq/first))

#_(defmethod move :move/prev-sibling [_ e]
  (some-> (:seq/_first e) first :seq/_next first :seq/first))

(defn next-sibling [e] (some-> (:seq/_first e) first :seq/next :seq/first))
(defn prev-sibling [e] (some-> (:seq/_first e) first :seq/_next first :seq/first))



(defn flow*
  [e]
  (when e
    (or (next-sibling e)
        (recur (up e)))))

#_(defmethod move :move/flow [_ e]
  (or (:seq/first e)
      (flow* e))
  #_(if-let [f (:seq/first e)]
    (do (println "Flowfirst" e)
        f)
    (do (println "Flow* " e)
        (flow* e))))

(defn flow [e]
  (or (:seq/first e)
      (flow* e)))

#_(defmethod move :move/back-flow [_ e]
  (or (->> e
           (move :move/prev-sibling)
           (move :move/most-nested))
      (move :move/up e)))

(defn flow-left [e]
  (or (->> e
           (prev-sibling)
           (most-nested))
      (up e)))

#_(defmethod move :move/up [_ e]
  #_(some-> (:coll/_contains e) first)
  (when-let [cs (:coll/_contains e)]
    (let [up (first cs)]
      (when-not (= :chain (:coll/type up))
        up))))



#_(defmethod move :move/forward-up [_ e]
  (or (move :move/next-sibling e)
      (move :move/up e)))

(defn forward-up [e]
  (or (next-sibling e)
      (up e)))

(defn backward-up [e]
  (or (prev-sibling e)
      (up e)))

(defn movement-tx
  [db mover]
  (let [src (get-selected-form db)
        dst (mover src)]
    (when dst
      (move-selection-tx (:db/id src) (:db/id dst)))
    #_(if-not dst
        nil
        #_(println "Cannot" movement-type "from" src)
        (move-selection-tx (:db/id src) (:db/id dst)))))

#_(defn repeat-movement-tx
  [db movement-type reps]
  (let [sel (get-selected-form db)
        dst (loop [i 0
                   n sel]
              (if-not (< i reps)
                n
                (if-let [nn (move movement-type n)]
                  (recur (inc i) nn)
                  n)))]
    (if-not dst
      (println "Cannot" movement-type "from" sel)
      (move-selection-tx (:db/id sel) (:db/id dst)))))

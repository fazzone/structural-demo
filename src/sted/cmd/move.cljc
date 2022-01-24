(ns sted.cmd.move
  (:require
   [sted.embed :as e]
   [sted.core :refer [get-selected-form
                      move-selection-tx]]))

(defmulti move (fn [t _] t))

(defmethod move :move/most-nested [_ e]
  (last (tree-seq :coll/type e/seq->vec e)))

(defmethod move :move/most-upward [_ e]
  (->> e
       (iterate (partial move :move/up))
       (take-while some?)
       last))

(defmethod move :move/next-sibling [_ e]
  (some-> (:seq/_first e) first :seq/next :seq/first))

(defmethod move :move/prev-sibling [_ e]
  (some-> (:seq/_first e) first :seq/_next first :seq/first))

(defn flow*
  [e]
  (when e
    (or (move :move/next-sibling e)
        (recur (move :move/up e)))
    #_(if-let [m (move :move/next-sibling e)]
      (do (println "Flow*move" e m) m)
      (do (println "Flow*recurup" e)
          (recur (move :move/up e))))))

(defmethod move :move/flow [_ e]
  (or (:seq/first e)
      (flow* e))
  #_(if-let [f (:seq/first e)]
    (do (println "Flowfirst" e)
        f)
    (do (println "Flow* " e)
        (flow* e))))

(defmethod move :move/back-flow [_ e]
  (or (->> e
           (move :move/prev-sibling)
           (move :move/most-nested))
      (move :move/up e)))

(defmethod move :move/up [_ e]
  #_(some-> (:coll/_contains e) first)
  (when-let [cs (:coll/_contains e)]
    (let [up (first cs)]
      (when-not (= :chain (:coll/type up))
          up))))

(defmethod move :move/forward-up [_ e]
  (or (move :move/next-sibling e)
      (move :move/up e)))

(defmethod move :move/backward-up [_ e]
  (or (move :move/prev-sibling e)
      (move :move/up e)))

(defn movement-tx
  [db movement-type]
  (let [src (get-selected-form db)
        dst (move movement-type src)]
    (when dst
      (move-selection-tx (:db/id src) (:db/id dst)))
    #_(if-not dst
        nil
        #_(println "Cannot" movement-type "from" src)
        (move-selection-tx (:db/id src) (:db/id dst)))))

(defn repeat-movement-tx
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

(ns db-reactive
  (:require
   [datascript.core :as d]
   [rum.core :as rum]))

(def ereactive-components-by-eid (js/Array. 1024))
(def ereactive-maxt (js/Array. 1024))
(def ereactive-render-t (js/Array. 1024))

(defn add-ereactive-component!
  [tag eid rc]
  (when tag (println "Start listening" tag eid))
  (aset ereactive-components-by-eid  eid
        (doto (or (aget ereactive-components-by-eid eid)
                  (js/Array.))
          (.push rc))))

(defn remove-ereactive-component!
  [tag eid rc]
  (when-let [cs (aget ereactive-components-by-eid eid)]
    (loop [i 0]
      (cond
        (= i (.-length cs))
        nil

        (js/Object.is rc (aget cs i))
        (do (.splice cs i 1)
            (when tag (println "Stopped listening" tag eid)))
        :else (recur (inc i))))))

;; mixin for components which take datascript entity as their first argument
;; forces re-render with updated state when entity is touched by a transaction
(defn ereactive-with-debug-print
  [tag]
  {:init (fn [{:rum/keys [args] :as state}]
           (let [eid (some-> state :rum/args first :db/id)]
             (if-not eid
               state
               (do (add-ereactive-component! tag eid (:rum/react-component state))
                   (assoc state ::eid eid)))))
   :should-update (fn [old-state new-state]
                    (let [new-eid (-> new-state :rum/args first :db/id)]
                      #_(println "SU"
                                 (aget ereactive-maxt new-eid)
                                 (aget ereactive-components-by-eid new-eid))
                      ;; WRONG - save it!
                      #_(some? (aget ereactive-maxt new-eid))
                      true))
   :will-remount (fn [old-state new-state]
                   (let [old-eid (-> old-state :rum/args first :db/id)
                         new-eid (-> new-state :rum/args first :db/id )]
                     (when-not (= old-eid new-eid)
                       (remove-ereactive-component! tag old-eid (:rum/react-component new-state))
                       (add-ereactive-component! tag new-eid (:rum/react-component new-state))))
                   new-state)
   :will-unmount (fn [{:rum/keys [args] ::keys [eid] :as state}]
                   (remove-ereactive-component! tag eid (:rum/react-component state)))})

(def ereactive (ereactive-with-debug-print nil))

(def areactive-components (atom {}))
;; mixin to react to all transactions which touch a specific attribute
(defn areactive
  [& as]
  {:init
   (fn [state props]
     (doseq [a as]
      (swap! areactive-components assoc a
             (doto (or (get @areactive-components a)
                       (js/Array.))
               (.push (:rum/react-component state)))))
     state)
   :will-unmount
   (fn [state]
     (let [c (:rum/react-component state)]
       (doseq [a as]
         (let [cs (get @areactive-components a )]
           (if-not cs
             state
             (loop [i 0]
               (cond
                 (= i (.-length cs))
                 state

                 (js/Object.is c (aget cs i))
                 (do (.splice cs i 1)
                     state)
             
                 :else (recur (inc i)))))))))})

(defn tx-listen-fn
  [{:keys [db-after tx-data tx-meta tempids] :as tx-report}]
  (doseq [attr (distinct (map second tx-data))]
    (when-let [cs (get @areactive-components attr)]
      (doseq [^js/React.Component c cs]
        (.setState c (fn setstate-areactive [state props]
                       (let [rst (aget state :rum/state)]
                         (vswap! rst assoc :rum/args (cons db-after (next (:rum/args @rst)))))
                       state)))))
  (doseq [[e a v t] tx-data]
    (when (aget ereactive-components-by-eid e )
      (aset ereactive-maxt e t)))
  (doseq [eid (dedupe (map first tx-data))]
    (when-let [cs (aget ereactive-components-by-eid eid)]
      (doseq [^js/React.Component c cs]
        (.setState c
                   (fn setstate-ereactive [state props]
                     (let [rst (aget state :rum/state)]
                       (vswap! rst assoc :rum/args (cons (d/entity db-after eid)
                                                         (next (:rum/args @rst)))))
                     state))))))


(defn reset-for-reload!
  []
  (reset! areactive-components {})
  (dotimes [i (count ereactive-components-by-eid)]
    (aset ereactive-components-by-eid i nil))
  (dotimes [i (count ereactive-maxt)]
    (aset ereactive-maxt i nil))
  (dotimes [i (count ereactive-render-t)]
    (aset ereactive-render-t i nil)))

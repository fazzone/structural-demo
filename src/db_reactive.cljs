(ns db-reactive
  (:require
   [datascript.core :as d]
   [rum.core :as rum]))

(def ereactive-components-by-eid (js/Array. 1024))

;; mixin for components which take datascript entity as their first argument
;; forces re-render with updated state when entity is touched by a transaction
(def ereactive
  {:init
   (fn [state props]
     (let [eid (some-> props (aget :rum/initial-state) :rum/args first :db/id)]
       (if-not eid
         (do (println "No db/id! Did you mess up deletion?" props)
             state)
         (do (aset ereactive-components-by-eid  eid
                   (doto (or (aget ereactive-components-by-eid eid)
                             (js/Array.))
                     (.push (:rum/react-component state))))
             (assoc state ::eid eid)))))
   :will-unmount
   (fn [state]
     (let [c (:rum/react-component state)
           cs (some->> state ::eid (aget ereactive-components-by-eid))]
       (if-not cs
         state
         (loop [i 0]
           (cond
             (= i (.-length cs))
             state

             (js/Object.is c (aget cs i))
             (do (.splice cs i 1)
                 state)
             
             :else (recur (inc i)))))))})

(def areactive-components (atom {}))


;; mixin to react to all transactions which touch a specific attribute
(defn areactive
  [a]
  {:init
   (fn [state props]
     (swap! areactive-components assoc a
            (doto (or (get @areactive-components a)
                      (js/Array.))
              (.push (:rum/react-component state))))
     state)
   :will-unmount
   (fn [state]
     (let [c (:rum/react-component state)
           cs (get @areactive-components a )]
       (if-not cs
         state
         (loop [i 0]
           (cond
             (= i (.-length cs))
             state

             (js/Object.is c (aget cs i))
             (do (.splice cs i 1)
                 state)
             
             :else (recur (inc i)))))))})

(defn tx-listen-fn
  [{:keys [db-after tx-data tx-meta tempids] :as tx-report}]
  (doseq [attr (distinct (map second tx-data))]
    (when-let [cs (get @areactive-components attr)]
      (doseq [^js/React.Component c cs]
        (.setState c (fn [state props]
                       (let [rst (aget state :rum/state)]
                         (vswap! rst assoc :rum/args (cons db-after (next (:rum/args @rst)))))
                       state)))))
  (doseq [eid (dedupe (map first tx-data))]
    (when-let [cs (aget ereactive-components-by-eid eid)]
      (doseq [^js/React.Component c cs]
        (.setState c
                   (fn [state props]
                     (let [rst (aget state :rum/state)]
                       (vswap! rst assoc :rum/args (cons (d/entity db-after eid)
                                                         (next (:rum/args @rst)))))
                     state))))))





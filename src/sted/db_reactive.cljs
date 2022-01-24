(ns sted.db-reactive
  (:require
   [datascript.core :as d]
   [cljs.core.async :as async]
   [sted.core :as core]
   [rum.core :as rum]
   [sted.embed :as e])
  (:require-macros
   [cljs.core.async.macros :refer [go
                                   go-loop]]))

(def hack-cbq (atom []))

(defn flush-cbq
  []
  (js/ReactDOM.unstable_batchedUpdates
   (fn []
     (doseq [c @hack-cbq]
       (c))))
  (reset! hack-cbq []))

(defn update-first-arg!
  [^js/React.Component rc e ident]
  #_(try
    (js/console.time ident)
    (.setState rc (fn setstate-arx [state props]
                      (let [rst (deref (aget state :rum/state))]
                        #_(vswap! rst assoc :rum/args (cons e (next (:rum/args @rst))))
                        #_(println "Returning new state for " (:db/id e))
                        
                        #js {":rum/state"
                             (volatile!
                              (assoc rst :rum/args (cons e (next (:rum/args rst)))))}
                        #_(js/Object.assign #js {:extra 1} state))))
    (finally (js/console.timeEnd ident)))
  (.setState rc (fn setstate-arx [state props]
                  (let [rst (deref (aget state :rum/state))]
                    #js {":rum/state"
                         (volatile!
                          (assoc rst :rum/args (cons e (next (:rum/args rst)))))}))))

(def ereactive
  ;; mixin for components taking [entity bus ...]
  {:init          (fn [{:rum/keys [react-component] :as state} props]
                    (let [[ent bus & args] (some-> state :rum/args)
                          ch               (async/chan)
                          nupdate          (volatile! false)
                          subber           (fn [eid]
                                             (core/sub-entity bus eid
                                                              (fn [new-ent]
                                                                (vreset! nupdate true)
                                                                (update-first-arg! react-component new-ent
                                                                                   (str "erx " (:db/id new-ent))))))]
                      (when-not (and bus (:db/id ent))
                        (throw (ex-info (str "Cannot use ereactive " (pr-str ent)) {})))
                      (assoc state
                             ::nupdate nupdate
                             ::subber subber
                             ::unsubber (subber (:db/id ent)))))
   :should-update (fn [old-state new-state]
                    (some-> new-state ::nupdate deref))
   :will-remount  (fn [old-state new-state]
                    (let [[old-e old-bus] (-> old-state :rum/args)
                          [new-e bus]     (-> new-state :rum/args)
                          old-eid         (:db/id old-e)
                          new-eid         (:db/id new-e)]
                      (if (= old-eid new-eid)
                        new-state
                        (do
                          ((::unsubber old-state))
                          (assoc new-state ::unsubber ((::subber new-eid)))))))
   :will-unmount  (fn [state]
                    (when-some [u (::unsubber state)]
                      (u))
                    state)
   :after-render  (fn [state]
                   (some-> state ::nupdate (vreset! nil))
                   state)})

#_(defn areactive
  ;; mixin for components taking [db bus ...]
  [& as]
  {:init
   (fn [{:rum/keys [react-component] :as state} props]
     (let [[_ bus] (:rum/args state)
           ch (async/chan)
           ident (str "arx " as)]
       (go-loop []
         (let [[_ db] (async/<! ch)]
           (update-first-arg! react-component db ident)
           (recur)))
       ;; subscribe to updates about each attr
       (doseq [a as]
         (core/connect-sub! bus a ch))
       (assoc state ::areactive.chan ch)))
   :will-unmount
   (fn [{:rum/keys [react-component] :as state}]
     (let [[_ bus] (:rum/args state)
           ch (::areactive.chan state)]
       (doseq [a as]
         (core/disconnect-sub! bus a ch)))
     state)})

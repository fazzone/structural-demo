(ns db-reactive
  (:require
   [datascript.core :as d]
   [cljs.core.async :as async]
   [core :as core]
   [rum.core :as rum])
  (:require-macros
   [cljs.core.async.macros :refer [go
                                   go-loop]]))

(defn update-first-arg!
  [^js/React.Component rc e]
  (.setState rc (fn setstate-arx [state props]
                  (let [rst (aget state :rum/state)]
                    (vswap! rst assoc :rum/args (cons e (next (:rum/args @rst))))
                    state))))

(def ereactive
  ;; mixin for components taking [entity bus ...] 
  {:init          (fn [{:rum/keys [react-component] :as state} props]
                    (let [[ent bus & args] (some-> state :rum/args)
                          ch               (async/chan)
                          nupdate          (atom nil)]
                      (when-not (and bus (:db/id ent))
                        (throw (ex-info "Cannot use ereactive" {})))
                      (go-loop []
                        (let [[_ updated-entity] (async/<! ch)]
                          (reset! nupdate true)
                          (update-first-arg! react-component updated-entity)
                          (recur)))
                      (core/connect-sub! bus (:db/id ent) ch)
                      (assoc state ::ereactive.chan ch ::nupdate nupdate)))
   :should-update (fn [old-state {::keys [nupdate] :as new-state}]
                    (let [[e _ oi old-props] (:rum/args old-state)
                          [_ _ ni new-props] (:rum/args new-state)]
                      #_(println " " (:db/id e) "Old props" old-props
                                 "\n " (:db/id e) "New props" new-props "eq?" (= old-props new-props))
                      #_(when (and
                               (not= old-props new-props)
                               (= (get old-props (:db/id e))
                                  (get new-props (:db/id e))))
                          (println "Nopropchange " (:db/id e) new-props))
                        
                      (if-let [g (get new-props (:db/id e))]
                        (println "G" (:db/id e) g))
                      
                      (cond
                        (not= oi ni) true
                        
                        (get new-props (:db/id e))
                        true
                        
                        (some? @nupdate)
                        (do (reset! nupdate false)
                            true)
                        
                        )))
   :will-remount  (fn [old-state new-state]
                    (let [[old-e old-bus] (-> old-state :rum/args)
                          [new-e bus]     (-> new-state :rum/args)
                          old-eid         (:db/id old-e)
                          new-eid         (:db/id new-e)]
                      (when-not (identical? bus old-bus)
                        (throw (ex-info "The bus cannot change" {})))
                      (when-not (identical? (::ereactive.chan old-state) (::ereactive.chan new-state))
                        (throw (ex-info "The chan cannot change" {})))
                      (when-not (= old-eid new-eid)
                        (println "!!!!! The eids changed" old-eid new-eid)
                        (core/disconnect-sub! bus old-eid (::ereactive.chan old-state))
                        (core/connect-sub! bus new-eid (::ereactive.chan new-state)))
                      new-state))
   :will-unmount  (fn [{:rum/keys [args] :as state}]
                    (let [[e bus] args]
                      (core/disconnect-sub! bus (:db/id e) (::ereactive.chan state))
                      state))})

(defn areactive
  ;; mixin for components taking [db bus ...]
  [& as]
  {:init
   (fn [{:rum/keys [react-component] :as state} props]
     (let [[_ bus] (:rum/args state)
           ch (async/chan)]
       (go-loop []
         (let [[_ db] (async/<! ch)]
           (update-first-arg! react-component db)
           (recur)))
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













(ns db-reactive
  (:require
   [datascript.core :as d]
   [cljs.core.async :as async]
   [core :as core]
   [rum.core :as rum])
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
  [^js/React.Component rc e]
  (let [cs (swap! hack-cbq conj
                  (fn []
                    (.setState rc (fn setstate-arx [state props]
                                    (let [rst (aget state :rum/state)]
                                      (vswap! rst assoc :rum/args (cons e (next (:rum/args @rst))))
                                      
                                      state)))))] 
    (js/window.setTimeout
     flush-cbq
     0))
  #_(.setState rc (fn setstate-arx [state props]
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
                        (throw (ex-info (str "Cannot use ereactive " (pr-str ent)) {})))
                      (go-loop []
                        (let [[_ updated-entity] (async/<! ch)]
                          (reset! nupdate true)
                          (update-first-arg! react-component updated-entity)
                          (recur)))
                      (core/connect-sub! bus (:db/id ent) ch)
                      (assoc state ::ereactive.chan ch ::nupdate nupdate)))
   :should-update (fn [old-state {::keys [nupdate] :as new-state}]
                    (let [[e _ & old-props] (:rum/args old-state)
                          [_ _ & new-props] (:rum/args new-state)]
                      #_(println " " (:db/id e) "Old props" old-props
                               "\n " (:db/id e) "New props" new-props "eq?" (= old-props new-props))
                      (cond
                        (not= old-props new-props) true
                        (some? @nupdate)           (not (reset! nupdate false)))))
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













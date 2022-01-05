(ns core
  (:require [datascript.core :as d]
            #?(:clj [clojure.core.async :as async :refer [go go-loop]]
               :cljs [cljs.core.async :as async]))
  #?(:cljs
     (:require-macros
      [cljs.core.async.macros :refer [go
                                      go-loop]])))

(defn get-selected-form
  [db]
  (d/entity db [:form/highlight true]))

(defn move-selection-tx
  [src-eid dst-eid]
  [(when src-eid [:db/retract src-eid :form/highlight true])
   [:db/add dst-eid :form/highlight true]])

(defprotocol IBus
  (send! [this msg])
  (connect-sub! [this topic ch])
  (disconnect-sub! [this toipic ch])
  (zchan [this]))

(defn bus
  []
  (let [ch (async/chan)
        pub (async/pub ch first)]
    (reify IBus
      (send! [this msg] (async/put! ch msg))
      (connect-sub! [this topic sch] (async/sub pub topic sch))
      (disconnect-sub! [this topic sch] (async/unsub pub topic sch))
      (zchan [this] ch))))

(def blackhole
  (reify IBus
    (send! [_ _])
    (connect-sub! [_ _ _])
    (disconnect-sub! [_ _ _])))

(defprotocol IApp
  (sub-chan [this topic])
  (with [this msg]))

(defrecord App [conn bus])

(defn register-mutation!
  [{:keys [conn bus]} topic mut-fn]
  (let [ch (async/chan)]
    (connect-sub! bus topic ch)
    (go (async/<!
         (async/reduce
          (fn [a m]
            (or (mut-fn m @conn bus) a))
          :ok
          ch)))))

(defn register-simple!
  [{:keys [bus conn] :as app} topic mut-fn]
  (let [ch (async/chan)]
    (go-loop []
      (let [[_ & args] (async/<! ch)
            tx-data (apply mut-fn @conn args)]
        (try (d/transact! conn tx-data)
             (catch #?(:cljs js/Error :clj Exception) e
               (println "Error transacting" e)
               (println "Tx-data")
               (cljs.pprint/pprint tx-data)))
        (recur)))
    (connect-sub! bus topic ch)))

(defn app
  [conn]
  (let [the-bus (bus)]
    (map->App
     {:bus the-bus
      :conn (doto conn
              (d/listen! (fn [{:keys [tx-data tempids db-after] :as tx-report}]
                           (let [new-entities (set (vals tempids))
                                 es (into #{} (comp
                                               (filter (comp not new-entities))
                                               (map (fn [[e a v t]] e))) tx-data)
                                 as (into #{} (map (fn [[e a v t]] a)) tx-data)]
                             (doseq [e es]
                               (when-not (empty? (d/datoms db-after :eavt e))
                                 (send! the-bus [e (d/entity db-after e)])))
                             (doseq [a as]
                               (send! the-bus [a db-after]))))))})))




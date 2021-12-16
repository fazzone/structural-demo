(ns core
  (:require [datascript.core :as d]
            [cljs.core.async :as async])
  (:require-macros
   [cljs.core.async.macros :refer [go
                                   go-loop]]))

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
  (disconnect-sub! [this toipic ch]))

(defn bus
  []
  (let [ch (async/chan)
        pub (async/pub ch first)]
    (reify IBus
      (send! [this msg] (async/put! ch msg))
      (connect-sub! [this topic sch] (async/sub pub topic sch))
      (disconnect-sub! [this topic sch] (async/unsub pub topic sch)))))

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
  [{:keys [conn] :as app} topic mut-fn]
  (let [ch (async/chan)]
    (go (async/<!
         (async/reduce
          (fn [_ [_ & args]]
            #_(println "Rs" topic args)
            (d/transact! conn
                         (apply mut-fn @conn args)))
          :ok
          ch)))
    (connect-sub! (:bus app) topic ch)))

(defn app
  [schema init-tx-data]
  (let [the-bus (bus)]
    (map->App
     {:bus the-bus
      :conn (doto (d/create-conn schema)
              (d/transact! init-tx-data)
              (d/listen! (fn [{:keys [tx-data db-after] :as tx-report}]
                           (let [es (into #{} (map (fn [[e a v t]] e)) tx-data)
                                 as (into #{} (map (fn [[e a v t]] a)) tx-data)]
                             (doseq [e es]
                               (send! the-bus [e (d/entity db-after e)]))
                             (doseq [a as]
                               (send! the-bus [a db-after]))))))})))




(ns core
  (:require [datascript.core :as d]
            [embed :as e]
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

(defrecord App [conn bus history])

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

#_(defn register-simple!
  [{:keys [bus conn] :as app} topic mut-fn]
  (let [ch (async/chan)]
    (go-loop [last-tx nil]
      (let [[_ & args :as mut] (async/<! ch)
            db         @conn
            hent       (d/entity db :page/history)
            tx-data    (concat (apply mut-fn db args)
                               (when (:seq/first hent)
                                 [{:db/id (:db/id hent)
                                   :seq/first {:db/id :db/current-tx
                                               :coll/_contains (:db/id hent)
                                               :form/linebreak true
                                               :string/value (pr-str mut)}
                                   :seq/next {:db/id "nh"
                                              :seq/first (:db/id (:seq/first hent))}}
                                  (when-let [next (:seq/next hent)]
                                    [:db/add "nh" :seq/next (:db/id next) ])]))
            report     (try (d/transact! conn tx-data)
                            (catch #?(:cljs js/Error :clj Exception) e
                              {:error e}))]
        (when-let [e (:error report)]
          (println "Error transacting" e)
          (println "Tx-data")
          (run! prn tx-data))
        (recur (or (get (:tempids report) :db/current-tx)
                   last-tx))))
    (connect-sub! bus topic ch)))

(defn register-simple!
  [{:keys [bus conn history] :as app} topic mut-fn]
  (let [ch (async/chan)]
    (go-loop [last-tx nil]
      (let [[_ & args :as mut] (async/<! ch)
            db         @conn
            hent       (d/entity db :page/history)
            tx-data    (apply mut-fn db args)
            report     (try (and tx-data (d/transact! conn tx-data))
                            (catch #?(:cljs js/Error :clj Exception) e
                              {:error e}))]
        
        (when (:tempids report)
          (reset! history (cons (assoc report :mut (first mut)) @history)))
        
        #_(prn
           (for [h @history]
             (if (map? h)
               (:db/current-tx (:tempids h))
               (map #(:db/current-tx (:tempids %)) h))))
        
        #_(doseq [h @history]
          
            #_(prn (get (:tempids h) :db/current-tx) (:tx-data h)))
        (when-let [e (:error report)]
          (println "Error transacting" e)
          (println "Tx-data")
          (run! prn tx-data))
        (recur (or (get (:tempids report) :db/current-tx)
                   last-tx))))
    (connect-sub! bus topic ch)))


(defn revert-txdata
  [tx-data]
  (for [[e a v t a?] (reverse tx-data)]
    [(if a? :db/retract :db/add) e a v]))

(defn setup-undo!
  [{:keys [bus conn history] :as app}]
  (let [ch (async/chan)]
    (go-loop []
      (let [_                    (async/<! ch)
            [prior-undo & undos] (take-while list? @history)
            [u & more]           (drop-while list? @history)]
        (prn "Udo"
             (for [h @history]
               (if (map? h)
                 (:mut h)
                 (map :mut h))))
        (when u
          (d/transact! conn (revert-txdata (:tx-data u)))
          (reset! history
                  (cons (cons u prior-undo)
                        (concat undos more)))))
      (recur))
    (connect-sub! bus :undo ch))

  (let [ch (async/chan)]
    (connect-sub! bus :reify-undo ch)
    (go-loop []
      (let [_ (async/<! ch)
            rd (for [h @history]
                       (if (map? h)
                         (:mut h)
                         (map :mut h)))
            ]
        (prn "Reify" rd)
        (send! bus [:insert-data rd])
        (recur))))
  app)



(defn app
  [conn]
  (let [the-bus (bus)]
    (-> {:bus the-bus
         :history (atom ())
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
                                  (send! the-bus [a db-after]))))))}
        (map->App)
        (setup-undo!))))




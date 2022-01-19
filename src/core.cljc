(ns core
  (:require [datascript.core :as d]
            [embed :as e]
            #? (:clj [clojure.core.async :as async :refer [go go-loop]]
               :cljs [cljs.core.async :as async]))
  #? (:cljs
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
  
  (fire-subs! [this eid])
  (sub-entity [this eid func])
  
  ;; history?
  (get-history [this txid])
  (save-history! [this txid tx-report])
  ;; hacks
  (zchan [this]))

(defprotocol ICursor
  (selection [this ent])
  (get-selection [this])
  (set-selection! [this ent]))

(defn cursor
  []
  (let [c (atom nil)]
    (reify ICursor
      (selection [this e] (= (:db/id e) (:db/id @c)))
      (get-selection [this] @c)
      (set-selection! [this e] (reset! c e)))))

(defn bus
  []
  (let [ch (async/chan)
        pub (async/pub ch first)
        hs (atom {})
        
        sub-map (js/Map.)]
    (reify IBus
      (send! [this msg] (async/put! ch msg))
      (connect-sub! [this topic sch]
        (async/sub pub topic sch))
      (disconnect-sub! [this topic sch]
        (async/unsub pub topic sch))
      
      (fire-subs! [this entity]
        (some-> (.get sub-map (:db/id entity))
                (.forEach (fn [f] (f entity)))))
      (sub-entity [this eid func]
        (let [sub-set (or (.get sub-map eid)
                          (let [s (js/Set.)]
                            (.set sub-map eid s)
                            s))]
          (.add sub-set func)
          (fn [] (.delete sub-set func))))
      
      (get-history [this txid] (get @hs txid))
      (save-history! [this txid r] (swap! hs assoc txid r))
      (zchan [this] ch))))

(defn context
  []
  (let [b (bus)
        c (cursor)]
    (reify
      ICursor
      (selection [t e] (selection c e))
      (set-selection! [t e] (set-selection! c e))
      (get-selection [t] (get-selection c))
      IBus
      (send! [t m] (send! b m))
      (connect-sub! [t o s] (connect-sub! b o s))
      (disconnect-sub! [t o s] (disconnect-sub! b o s))
      (sub-entity [t o s] (sub-entity b o s))
      (fire-subs! [t e]
        (fire-subs! b e))
      (get-history [t x] (get-history b x))
      (save-history! [t x r] (save-history! b x r))
      (zchan [t] (zchan b)))))

(def blackhole
  (reify IBus
    (send! [_ _])
    (connect-sub! [_ _ _])
    (disconnect-sub! [_ _ _])
    (get-history [this txid])
    (sub-entity [_ _ _])
    (save-history! [this txid r])
    (zchan [this])))

(defprotocol IApp
  (sub-chan [this topic])
  (with [this msg]))

(defrecord App [conn bus! history])

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

;; One go-loop per mutation type is stupid because there is no way to do blocking apply


;; Is it allowed to reorder them?


(defn register-simple!
  [{:keys [bus conn history] :as app} topic mut-fn]
  (let [ch (async/chan)]
    (go-loop [last-tx nil]
      (let [[mut-name & args :as mut] (async/<! ch)
            db                 @conn
            tx-data            (apply mut-fn db args)
            tid                (str mut-name)
            ;; _                  (js/console.time tid)
            report             (try (and tx-data
                                         (assoc (d/transact! conn tx-data)
                                                :mut mut))
                                    (catch #? (:cljs js/Error :clj Exception) e
                                      {:error e})
                                    #_(finally (js/console.timeEnd tid)))]
        (when (:db-after report)
          (if-let [txid (get (:tempids report) :db/current-tx)]
            (do (save-history! bus txid report)
                #_(println txid mut)
                (reset! history (cons report @history)))
            (println "No current-tx?")))
        (when-let [e (:error report)]
          (println (str "Error transacting " (pr-str mut)) e)
          (println "Tx-data")
          (run! prn tx-data))
        (recur (or (get (:tempids report) :db/current-tx)
                   last-tx))))
    (connect-sub! bus topic ch)))

(defn movement->mutation
  [mover]
  (fn [db & args]
    (let [sel (get-selected-form db)
          nf  (apply mover sel args)]
      (when nf
        (move-selection-tx (:db/id sel) (:db/id nf))))))

#_(defn register-movement!
  [{:keys [bus conn history] :as app} topic mover]
  (let [ch (async/chan)]
    (go-loop []
      (let [[_ & args :as mut] (async/<! ch)
            src                (get-selected-form @conn)
            dst                (try (apply mover src args)
                                    (catch #? (:cljs js/Error :clj Exception) e
                                      {:error e}))]
        (when-not (:error dst))
        (recur)))
    (connect-sub! bus topic ch)))

(defn revert-txdata
  [tx-data]
  (for [[e a v t a?] (reverse tx-data)]
    [(if a? :db/retract :db/add) e a v]))

(def ^:const tx0   536870912)

(def ^:const emax  2147483647)

(def ^:const txmax 2147483647)

(defn setup-undo!
  [{:keys [bus conn history] :as app}]
  (let [ch (async/chan)]
    (go-loop []
      (let [_                    (async/<! ch)
            [prior-undo & undos] (take-while list? @history)
            [u & more]           (drop-while list? @history)]
        #_(println "Undof"
                 (for [h @history]
                   (if (map? h)
                     (get (:tempids h) :db/current-tx)
                     (for [s h]
                       (get (:tempids s) :db/current-tx)))))
        (when u
          (let [rr (d/transact! conn (revert-txdata (:tx-data u)))]
            (println "RR Max-tx" (:max-tx (:db-after rr)) "Tempids" (:tempids rr)))
          #_(let [rr (-> u
                       (update :db-before update :max-tx inc)
                       (update :tempids assoc))])
          #_(do (reset! conn (update (:db-before u) :max-tx inc))
                (publish-tx-report! bus (:db-before u) (:tx-data u) (:tempids u)))
          (reset! history
                  (cons (cons u prior-undo)
                        (concat undos more)))))
      (recur))
    (connect-sub! bus :undo ch))
  (let [ch (async/chan)]
    (connect-sub! bus :reify-undo ch)
    (go-loop []
      (let [_ (async/<! ch)]
        (println "UndoR"
         (for [h @history]
           (if (map? h)
             (get (:tempids h) :db/current-tx)
             (for [s h]
               (get (:tempids s) :db/current-tx)))))
        (send! bus [:insert-txdata
                    (-> (for [h @history]
                          (if (map? h)
                            (get (:tempids h) :db/current-tx)
                            (for [s h]
                              (get (:tempids s) :db/current-tx))))
                        (e/->tx)
                        (assoc :coll/type :undo-preview))])
        (recur))))
  app)

(defn app
  [conn]
  (let [the-bus (context)]
    (-> {:bus     the-bus
         :history (atom ())
         :conn    (doto conn
                    #_(d/listen! (fn [{:keys [db-after db-before tx-data tempids] :as report}]
                                   (if-let [txid (get tempids :db/current-tx)]
                                  (do (save-history! the-bus txid report)
                                      (reset! history (cons report @history)))
                                  (println "No current-tx?"))
                                (publish-tx-report! the-bus db-after tx-data tempids)))
                    (d/listen! (fn [{:keys [db-after db-before tx-data tempids]}]
                                 (try
                                   ;; (js/console.time "publish")
                                   (let [emax (:max-eid db-before)
                                         es (into #{}
                                                  (keep (fn [[e]]
                                                          (when-not (> e emax)
                                                            e)))
                                                  tx-data)
                                         as (into #{} (map (fn [[_ a]] a)) tx-data)]
                                     ;; (println "Actual tx-data:")
                                     ;; (run! prn tx-data)
                                     #_(println "Tempids" tempids)
                                     
                                     #_(run! prn tx-data)
                                     #_(println "Transacted" (count tx-data) "datoms" "Es" es "As" as)
                                     #_(doseq [a as]
                                       (send! the-bus [a db-after]))
                                     (js/ReactDOM.unstable_batchedUpdates
                                      (fn []
                                        (doseq [e es]
                                          (when-not (empty? (d/datoms db-after :eavt e))
                                            (fire-subs! the-bus (d/entity db-after e))))))
                                     
                                     
                                     #_(doseq [e es]
                                       (when-not (empty? (d/datoms db-after :eavt e))
                                         #_(println "Fire subs for" e)
                                         (fire-subs! the-bus (d/entity db-after e))
                                         #_(send! the-bus [e (d/entity db-after e)]))))
                                   #_(finally (js/console.timeEnd "publish"))))))}
        (map->App)
        (setup-undo!))))

(ns sted.core
  (:require [datascript.core :as d]
            [sted.embed :as e]
            #? (:clj [clojure.core.async :as async :refer [go go-loop]]
                :cljs [cljs.core.async :as async]))
  #? (:cljs
      (:require-macros
       [cljs.core.async.macros :refer [go
                                       go-loop]])))

;; This is a really stupid hack
;; But a div that fights back when you try to scroll is even stupider 
(defonce scroll-sequence-number (volatile! 0))
(defonce scroll-snapshot (volatile! 0))
(defn scroll-locked? []
  #_#?(:cljs
       (js/console.log "SL" @scroll-sequence-number @scroll-snapshot))
  #_true
  (not= @scroll-sequence-number @scroll-snapshot))

(defn get-selected-form
  [db]
  (d/entity db [:form/highlight true]))

(defn move-selection-tx
  [src-eid dst-eid]
  (when (not= src-eid dst-eid)
    [(when src-eid [:db/retract src-eid :form/highlight true])
     [:db/add dst-eid :form/highlight true]]))

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
  (zchan [this])

  (uniqueid [this])
  (reset [this])

  (set-app! [this app])
  (get-app [this])

  ;; more hacks
  (should-update [this]))

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
  ([] (bus nil))
  ([last-sub-map]
   (let [ch (async/chan)
         pub (async/pub ch first)
         hs (atom {})
         app-ref (atom nil)
         #?@(:cljs [sub-map (or last-sub-map (js/Map.))]) 
         uu (str (d/squuid))]
     #_(when last-sub-map
       (js/console.log "Re-use sub-map" last-sub-map))
     (reify IBus
       (send! [this msg]
         (async/put! ch msg))
       (connect-sub! [this topic sch]
         (async/sub pub topic sch))
       (disconnect-sub! [this topic sch]
         (async/unsub pub topic sch))
      
       (fire-subs! [this entity]
         #?(:cljs
            (some-> (.get sub-map (:db/id entity))
                    (.forEach (fn [f] (f entity))))))
       (sub-entity [this eid func]
         #?(:cljs
            (let [sub-set (or (.get sub-map eid)
                              (let [s (js/Set.)]
                                (.set sub-map eid s)
                                s))]
              (.add sub-set func)
              (fn [] (.delete sub-set func)))))
      
       (get-history [this txid] (get @hs txid))
       (save-history! [this txid r] (swap! hs assoc txid r))
       (zchan [_this] ch)
       (uniqueid [_this] uu)
       (reset [_this]
         #?(:cljs (bus sub-map)))

       ;; hacks?
       (set-app! [_this the-app] (reset! app-ref the-app))
       (get-app [_] @app-ref)
       (should-update [_] nil)))))

(def blackhole
  (reify IBus
    (send! [_ _])
    (connect-sub! [_ _ _])
    (disconnect-sub! [_ _ _])
    (get-history [this txid])
    (sub-entity [_ _ _] (fn []))
    (save-history! [this txid r])
    (zchan [this])
    (set-app! [_ _])
    (get-app [_])
    (should-update [_] true)))

(defrecord App [conn bus history system])

(defn register-mutation!
  [{:keys [conn bus] :as app} topic mut-fn]
  (let [ch (async/chan)]
    (connect-sub! bus topic ch)
    (go (async/<!
         (async/reduce
          (fn [a m]
            (try
              (or (mut-fn m @conn bus) a)
              (catch #? (:cljs js/Error :clj Exception) e
                (#?(:clj println :cljs js/console.log)
                 "Exception in mutation"
                 (str topic)
                 e))))
          :ok
          ch)))
    app))

(defn register-simple!
  [{:keys [bus conn history] :as app} topic mut-fn]
  (let [ch (async/chan)]
    (go-loop [last-tx nil]
      (let [[mut-name & args :as mut] (async/<! ch)
            db @conn
            sel (get-selected-form db)
            tx-data (apply mut-fn sel args)
            _ (prn (uniqueid bus) mut-name)
            report (try (and tx-data
                             #_(assoc (d/transact! conn tx-data)
                                      :mut mut)
                             #_(js/console.time "Transacting")
                             (let [ans (assoc (d/transact! conn tx-data)
                                              :mut mut)]
                               #_(js/console.timeEnd "Transacting")
                               ans))
                        (catch #? (:cljs js/Error :clj Exception) e
                          {:error e}))]
        
        (when (:db-after report)
          (if-let [txid (get (:tempids report) :db/current-tx)]
            (do (save-history! bus txid report)
                
                #_(println "================================================================" )
                #_(println txid mut )
                #_(run! prn (:tx-data report))
                
                (reset! history (cons report @history)))
            (println "No current-tx?")))
        (when-let [e (:error report)]
          (println (str "Error transacting " e (pr-str mut)) e)
          (println "Tx-data")
          (run! prn tx-data))
        (recur (or (get (:tempids report) :db/current-tx)
                   last-tx))))
    (connect-sub! bus topic ch)
    app))

(defn movement->mutation
  [mover]
  (fn [sel & args]
    (let [nf  (apply mover sel args)]
      (when nf
        (move-selection-tx (:db/id sel) (:db/id nf))))))

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

  
  ;; Broken by something
  #_(let [ch (async/chan)]
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

  (let [ch (async/chan)]
    (connect-sub! bus :save-demo ch)
    (go-loop []
      (let [_ (async/<! ch)]
        (prn (type @history))
        (println "UndoR")
        (cljs.pprint/pprint
         (reverse
          (for [h @history
                :when (map? h)]
            (:mut h))))
        #_(cljs.pprint/pprint
           (vec (for [h @history]
                  (if (map? h)
                    (:mut h)
                    (mapv :mut h)))))
        #_(for [h @history]
            (if (map? h)
              (:mut h)
            
              )
          
          
            )
        (recur))))
  app)

(defn app
  ([conn] (app conn nil))
  ([conn pbus]
   (let [the-bus (or pbus (bus))
         my-id (uniqueid the-bus)
         the-app (-> {:bus     the-bus
                      :history (atom ())
                      :conn    (doto conn
                                 (d/listen!
                                  (with-meta
                                    (fn [{:keys [db-after db-before tx-data tempids]}]
                                      (try
                                        (let [emax (:max-eid db-before)
                                              es (into #{}
                                                       (keep (fn [[e]]
                                                               (when-not (> e emax)
                                                                 e)))
                                                       tx-data)
                                              as (into #{} (map (fn [[_ a]] a)) tx-data)]
                                          
                                          (vreset! scroll-snapshot @scroll-sequence-number)
                                          #_(js/console.time "batchedUpdates")
                                          #?(:cljs
                                             (js/ReactDOM.unstable_batchedUpdates
                                              (fn []
                                                (doseq [e es]
                                                  (when-not (empty? (d/datoms db-after :eavt e))
                                                    (fire-subs! the-bus (d/entity db-after e)))))))
                                          #_(js/console.timeEnd "batchedUpdates"))
                                        (catch #? (:cljs js/Error :clj Exception) e
                                          (#?(:clj println :cljs js/console.log)
                                           "Exception in listener" e))))
                                    {:core/id my-id})))}
                     (map->App)
                     (setup-undo!))]
     (set-app! the-bus the-app)
     the-app)))

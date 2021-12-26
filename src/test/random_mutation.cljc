(ns test.random-mutation
  (:require
   [clojure.edn :as edn]
   [embed :as e]
   [schema :as s]
   [datascript.core :as d]
   [clojure.core.async :as async]
   [cmd.move :as move]
   [cmd.edit :as edit]
   [cmd.mut :as mut]
   [core :refer [get-selected-form
                 move-selection-tx]]))

(defn setup-db
  [init-form]
  (let [ftx (e/->tx init-form)]
    (d/db-with @(d/create-conn s/schema)
               [{:db/ident ::state
                 :state/bar {:db/id "bar"
                             :coll/type :bar
                             :seq/first {:coll/type :chain
                                         :coll/_contains "bar"
                                         :coll/contains #{(:db/id ftx)}
                                         :seq/first ftx}}}])))

(defn run
  [init-form muts] 
  (let [conn (d/create-conn s/schema)
        form-txdata (e/->tx init-form)
        {:keys [db-after tempids] :as init-report}
        (d/transact! conn
                     )
        
        reports (reductions
                 (fn [{:keys [db-after]} [m & args]]
                   (if-let [mut-fn (get mut/dispatch-table m)]
                     (try
                       (let [tx (apply mut-fn db-after args)]
                         (assoc (d/with db-after tx) :input-tx tx))
                       (catch Exception e
                         (reduced {:failure {:message (ex-message e)
                                             :data (ex-data e)}})))
                     (throw (ex-info "No mutation" {:m m}))))
                 init-report
                 muts)]
    (last reports)))

(def population
  [[:flow-right]
   [:flow-right]
   [:flow-left]
   [:flow-left]
   [:float]
   [:sink]
   [:wrap]
   [:delete-left]
   [:delete-right]
   [:clone]
   [:barf-right]
   [:slurp-right]])

(defn search-for-failure
  [init-form maxn]
  (loop [i         0
         db        (setup-db init-form)
         mutations []]
    (let [[m & args :as mut] (rand-nth population)
          tx                 (try (apply (mut/dispatch-table m) db args)
                                  (catch Exception e {::mut-error {:e e :mut mut}}))
          mut-error          (::mut-error tx)
          ndb                (try (when-not mut-error (d/db-with db tx))
                                  (catch Exception e {::tx-error {:e e :tx tx}}))
          tx-error           (::tx-error ndb)]
      (cond
        (nil? mut) (do (println "Retry " i "xd")
                       (recur (inc i) db mutations))
        mut-error  {:i i ::mut-error (ex-message (:e mut-error)) :muts (conj mutations mut)}
        tx-error   {:i i ::tx-error (ex-message (:e tx-error)) :muts (conj mutations mut)}
        (< i maxn) (recur (inc i) ndb (conj mutations mut))  
        :else      nil))))


(comment

  (+ 18 18 9 9 9)
  (def the-future
    (future
      (time
       (dotimes [i 99]
         (prn i)
         (when-let [r (search-for-failure '[a ^:form/highlight b c] 9544)]
           (prn r)))))))


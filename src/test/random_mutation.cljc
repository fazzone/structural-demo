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
        (d/transact! conn)
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

#_(def population
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
(defn try-mutations
  [db muts]
  (loop [db                          db
         [[m & args :as mut] & more] muts]
    (if-not mut
      db
      (let [tx  (try (apply (mut/dispatch-table m) db args)
                     (catch Exception e
                       (throw (ex-info "Mutator failure"
                                       {:type ::mutation-failure
                                        :mutation m
                                        :args args}
                                       e))))
            ndb (try (d/db-with db tx)
                     (catch Exception e
                       (throw (ex-info "Transaction failure"
                                       {:type ::transaction-failure
                                        :mutation m
                                        :args args
                                        :tx-data tx}
                                       e))))]
        (recur ndb more)))))



#_(defn search-for-failure
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

(def population
  [[[:flow-right]]
   [[:flow-right]]
   [[:flow-left]]
   [[:flow-left]]
   [[:float]] [[:float]] [[:float]]
   [[:sink]] [[:sink]]
   [[:wrap]] [[:wrap]] [[:wrap]]
   [[:delete-left]] [[:delete-left]] [[:delete-right]] [[:delete-right]]
   [[:clone]] [[:clone]]
   [[:barf-right]]
   [[:slurp-right]] [[:slurp-right]] [[:slurp-right]]
   [[:raise]] [[:raise]]
   [[:new-vec] [:edit/finish "x"]]
   [[:new-vec] [:edit/finish "x"]]
   [[:new-vec] [:edit/finish "x"]]
   [[:new-vec] [:edit/finish "x"]]
   [[:new-vec] [:edit/finish "x"]]
   [[:new-vec] [:edit/reject]]
   [[:new-vec] [:edit/reject]]
   [[:new-vec] [:edit/reject]]
   [[:new-vec] [:edit/reject]]])

(defn search-for-failure
  [init-form maxn]
  (loop [i         0
         db        (setup-db init-form)
         mutations []]
    (let [mut-seq (rand-nth population)
          ndb (try-mutations db mut-seq)]
      (if-not (< i maxn)
        db
        (recur (inc i) ndb (conj mutations mut-seq))))))



(comment
  (do
    (def the-db
      (search-for-failure
       '[a ^:form/highlight b c]
       999
       ))
    (clojure.pprint/pprint (e/->form (d/entity the-db 4)))))



(comment
  (e/->string
   (d/entity
    (try-mutations
     (setup-db '[a ^:form/highlight b c])
     [[:flow-right] [:flow-right] [:flow-right] [:flow-right] [:flow-right] [:flow-right] [:flow-right]])
    2)))

(comment
  (+ 18 18 9 9 9)
  (do the-future)

  (def the-future
    (future
      (time
       (dotimes [i 25]
         (prn i)
         (when-let [r (search-for-failure '[a ^:form/highlight b c] 959)]
           (prn r)))))))

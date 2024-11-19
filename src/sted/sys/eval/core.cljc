(ns sted.sys.eval.core
  (:require [datascript.core :as d]
            [sted.cmd.nav :as nav]
            [sted.embed :as e]
            [sted.cmd.edit :as edit]
            [sted.core :as core]))


(defprotocol IEvalSession
  (do-eval [this conn db]))

(defonce eval-sessions (atom {}))

#_(defn create-session!
  [inst]
  (let [id (random-uuid)]
    (swap! eval-sessions assoc id inst)
    id))

#_(defn do-gen-eval [conn db bus])

(defn bind-session-tx
  [db eid ses-id]
  (let [top-level (peek (nav/parents-vec (d/entity db eid)))
        chain (some-> top-level :coll/_contains first)
        msg {:db/id "conn-ok-msg"
             :token/type :string
             :token/value "nrepl connected"}]
    (if-not chain
      (println "Can't find chain - has it been deleted? ")
      (into [msg
             [:db/add (:db/id chain) :eval/session ses-id]]
            (edit/form-cons-tx msg (d/entity db :messages))))))

(defn bind
  [{:as app {::keys [state]} :system} f]
  (let [id (random-uuid)]
    (swap! state assoc id f)
    id))

(defn bind*
  [{:as app {::keys [state] :as sys} :system} id f]
  (swap! state assoc id f))

(defn setup!
  [{:keys [conn bus] :as app}]
  (let [state eval-sessions]
    #_(d/transact! conn [{:db/ident :sted.page/state ::state state}])
    (-> app
        (update :system assoc ::state state)
        (core/register-mutation! :gen-eval
                                 (fn [m db bus]
                                   (let [sel (core/get-selected-form db)
                                         [target ses-id] (loop [e sel]
                                                           (if-let [s (:eval/session e)]
                                                             [e s]
                                                             (if-let [u (some-> e :coll/_contains first)]
                                                               (recur u)
                                                               nil)))
                                         
                                         _ (println "Ses-id" ses-id "KState" (keys @state))
                                         sesf (or (get @state ses-id)
                                                  (get @state (core/uniqueid bus)))]
                                     
                                     
                                     (sesf sel )
                                     
                                     
                                     ))))))


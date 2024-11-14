(ns sted.sys.nrepl.setup
  (:require [datascript.core :as d]
            [sted.sys.nrepl.bencode :as benc]
            [sted.embed :as e]
            [sted.core :as core]
            [sted.cmd.nav :as nav]
            [sted.sys.eval.core :as ec]
            [sted.cmd.edit :as edit]
            [clojure.edn :as edn])
  (:require-macros
   [sted.macros :as m]))


;; survive reloads


(defonce secret-persistent-nrepl-state (atom nil))


(defn exactly-one
  [[x & more :as xs]]
  (when more
    (throw (ex-info (str "More than one: " (pr-str (map :db/id xs)))
                    {:xs xs})))
  x)


(defn process-one-message
  [sock state conn conn-id {:as msg :strs [id]}]
  (case id
    1 (let [nses (get msg "new-session")]
        (swap! state update conn-id merge {:session nses :counter (atom 16) :pending (sorted-map)})
        (swap! state assoc :active conn-id)
        (.write sock (benc/encode {:id 2  :op :describe})))
    2 (do (swap! state update conn-id assoc :describe msg)
          ;;
          (.write sock
                  (benc/encode {:op :load-file
                                :id 3
                                :session (-> @state
                                             (get conn-id)
                                             (get :session))
                                :file-path "src/sted/embed/common.cljc"
                                :file-name "common.cljc"
                                :file (m/macro-slurp "src/sted/embed/common.cljc")}))
          ;;
          (.write sock
                  (benc/encode {:op :load-file
                                :id 4
                                :session (-> @state
                                             (get conn-id)
                                             (get :session))
                                :file-path "src/sted/embed/data.cljc"
                                :file-name "data.cljc"
                                :file (m/macro-slurp "src/sted/embed/data.cljc")})))
    3 :ok
    4 (do
        (when (= '("done") (msg "status"))
          (println "Update eval action of chain")
          (let [sel (core/get-selected-form @conn)
                top-level (peek (nav/parents-vec sel))
                chain (some-> top-level
                              :coll/_contains
                              first)
                msg {:db/id "conn-ok-msg"
                     :token/type :string
                     :token/value "nrepl connected"}]
            (d/transact! conn
                         (into [[:db/add (:db/id chain) :eval/action :eval-nrepl]
                                [:db/add (:db/id chain) :eval/session conn-id]
                                msg]
                               (edit/form-cons-tx
                                msg (d/entity @conn :messages)))))))
    (let [f (get (:pending @state) id)]
      (if-not f
        (println "No cont?" id)
        (f msg)))))

(defn create-pending-eval!
  [conn target]
  (let [id  "peval"
        txe {:db/id            id
             :coll/type        :eval-result
             :eval/result-type :pending
             :eval/of          (:db/id target)}
        tx  (into [txe] (edit/insert-after-tx target txe))
        r   (d/transact! conn tx)]
    (get (:tempids r) id)))

(defn make-eval-string*
  [target er-eid limit]
  (str "(sted.embed.data/rplacd " "(" "list " (e/->string target) ")" " " limit " " er-eid " " er-eid ")"))

(defn do-nrepl-eval
  [conn state db]
  (let [{:as st  :keys [active]} @state
        {:keys [sock session counter]} (get st active)
        target (peek (nav/parents-vec (core/get-selected-form db)))
        er-eid (create-pending-eval! conn target)
        limit (or (:state/limit (d/entity db :sted.page/state))
                  128) 
        code (make-eval-string* target er-eid limit)]
    (println "Code:\n" code)
    (if-not sock
      (println "No socket to send on")
      (let [id (swap! counter inc)]
        ;; callback
        (swap! state update
               :pending
               assoc
               id
               (fn [{:as eval-resp  :strs [value status]}]
                 (when (some #(= % "done") status)
                   (swap! state update :pending dissoc id))
                 (when value
                   (d/transact! conn
                                (into [[:db/add er-eid :eval/result-type :ok]]
                                      (edn/read-string value))))))
        ;; send it
        (.write sock
                (benc/encode {:id id
                              :op :eval
                              :session session
                              :code code}))))))

(defn mk-proc
  [{:as app :keys [conn]} db write]
  (let [session (atom nil)
        counter (atom 16)
        pending (atom {})]
    
    (fn [{:as msg :strs [id]}]
      (case id
        1 (do (reset! session (get msg "new-session"))
              (write {:id 2  :op :describe}))
        2 (do
            ;; 
            (write {:op :load-file
                    :id 3
                    :session @session
                    :file-path "src/sted/embed/common.cljc"
                    :file-name "common.cljc"
                    :file (m/macro-slurp "src/sted/embed/common.cljc")})
            ;;
            (write {:op :load-file
                    :id 4
                    :session @session
                    :file-path "src/sted/embed/data.cljc"
                    :file-name "data.cljc"
                    :file (m/macro-slurp "src/sted/embed/data.cljc")}))
        3 :ok
        4 (when (= '("done") (msg "status"))
            (do (println "Connection finished OK")
                (ec/bind* app @session
                          (fn [target]
                            (let [er-eid (create-pending-eval! conn target)
                                  limit 128
                                  code (make-eval-string* target er-eid limit)
                                  id (swap! counter inc)]
                              (swap! pending assoc id
                                     (fn [{:as eval-resp :strs [value status]}]
                                       (when (some #(= % "done") status)
                                         (swap! pending dissoc id))
                                       (when value
                                         (d/transact! conn
                                                      (into [[:db/add er-eid :eval/result-type :ok]]
                                                            (edn/read-string value))))))
                              ;; send it
                              (write {:id id :op :eval :session @session :code code}))))
                ;; finally
                (let [sel (core/get-selected-form db)]
                 (d/transact! conn
                              (ec/bind-session-tx db (:db/id (core/get-selected-form db)) @session)))))
        ;;  
        (if-let [f (get @pending id)]
          (f msg)
          (println "No cont?" id))))))


(defn setup!
  [{:keys [conn bus]  :as app}]
  (let [state secret-persistent-nrepl-state]
    (d/transact! conn [{:db/ident :sted.page/state  ::state state}])
    (-> app
        (core/register-mutation!
         :nrepl-connect
         (fn [[_ host port] db bus]
           (when-let [require (aget js/window "secret_electron_require")]
             (let [^js net (require "node:net")
                   sock (.connect net port host)
                   buffer (atom (js/Buffer.alloc 0))

                   write (fn [msg] (.write sock (benc/encode msg)))
                   proc (mk-proc app db write)]
               
               (doto sock
                 (.on "connect" (fn [a b c] (.write sock (benc/encode {:id 1  :op :clone}))))
                 (.on "end" (fn [] (println "Socket end")))
                 (.on "data"
                      (fn [sockdata]
                        (let [[msgs nbuf] (benc/decode-all (js/Buffer.concat #js [@buffer sockdata]))]
                          (reset! buffer nbuf)
                          (doseq [m msgs]
                            (println m)
                            (proc m)
                            #_(proc write session counter pending msg)
                            #_(process-one-message sock state conn conn-id m))))))
               
               
               ))))
        (core/register-mutation!
         :eval-nrepl (fn [_ db bus] (do-nrepl-eval conn state db))))))

(ns sted.sys.nrepl.setup
  (:require [datascript.core :as d]
            [sted.sys.nrepl.bencode :as benc]
            [sted.embed :as e]
            [sted.core :as core]
            [sted.cmd.nav :as nav]
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

(defn setup!
  [{:keys [conn bus] :as app}]
  (let [state secret-persistent-nrepl-state]
    (d/transact! conn [{:db/ident :sted.page/state ::state state}] )
    (-> app
        (core/register-mutation!
         :nrepl-connect
         (fn [[_ host port] db bus]
           (when-let [require (aget js/window "secret_electron_require")]
             (let [^js net (require "node:net")
                   sock    (.connect net port host)
                   conn-id (count @state) 
                   buffer  (atom (js/Buffer.alloc 0))]
               
               (swap! state assoc conn-id {:sock sock})
               
               (doto sock
                 (.on "connect" (fn [a b c] (.write sock (benc/encode {:id 1 :op :clone}))))
                 (.on "end" (fn [] (println "Socket end")))
                 (.on "data" (fn [sockdata]
                               (let [[msgs nbuf] (benc/decode-all (js/Buffer.concat #js [@buffer sockdata]))]
                                 (reset! buffer nbuf)
                                 (doseq [{:as msg :strs [id]} msgs]
                                   (println "Msg" msg)
                                   (case id
                                     1 (let [nses (get msg "new-session")]
                                         (swap! state update conn-id merge {:session nses
                                                                            :counter (atom 16)
                                                                            :pending (sorted-map)})
                                         (swap! state assoc :active conn-id)

                                         (.write sock (benc/encode {:id 2 :op :describe})))
                                     2 (do (swap! state update conn-id assoc :describe msg)
                                           ;; 
                                           (.write sock (benc/encode {:op        :load-file
                                                                      :id        3
                                                                      :session   (-> @state (get conn-id) (get :session))
                                                                      :file-path "src/sted/embed/common.cljc"
                                                                      :file-name "common.cljc"
                                                                      :file      (m/macro-slurp "src/sted/embed/common.cljc")}))
                                           ;; 
                                           (.write sock (benc/encode {:op        :load-file
                                                                      :id        4
                                                                      :session   (-> @state (get conn-id) (get :session))
                                                                      :file-path "src/sted/embed/data.cljc"
                                                                      :file-name "data.cljc"
                                                                      :file      (m/macro-slurp "src/sted/embed/data.cljc")})))
                                     3 :ok
                                     4 (do
                                         (when (= '("done") (msg "status"))
                                           (println "Update eval action of chain")
                                           (let [sel (core/get-selected-form @conn)
                                                 top-level (peek (nav/parents-vec sel))
                                                 chain (some-> top-level :coll/_contains first)]
                                             (d/transact! conn [[:db/add (:db/id chain) :eval/action :eval-nrepl]
                                                                [:db/add (:db/id chain) :eval/session conn-id]]))))
                                     
                                     (let [f (get (:pending @state) id)]
                                       (if-not f
                                         (println "No cont?" id)
                                         (f msg)))))))))))))

        (core/register-mutation!
         :eval-nrepl
         (fn [_ db bus]
           (println "Eval nrepl!")
           (let [{:as st :keys [active]}        @state
                 {:keys [sock session counter]} (get st active)
                 target                         (peek (nav/parents-vec (core/get-selected-form db)))
                 
                 spine (exactly-one (:seq/_first target))
                 coll  (exactly-one (:coll/_contains target))
                 nn    {:db/id     "ncell"
                        :seq/_next (:db/id spine)}
                 
                 code (str
                       "(sted.embed.data/go "
                       (e/->string target)
                       " " (or (:state/limit (d/entity db :sted.page/state))
                               128)
                       " " (pr-str (:db/id coll))
                       " " (pr-str (:db/id nn))
                       ")")]
             (println "Code:")
             (println code)
             
             (if-not sock
               (println "No socket to send on")
               (let [id (swap! counter inc)]
                 ;; callback
                 (swap! state update :pending assoc id
                        (fn [{:as eval-resp :strs [value status]}]
                          (when (some #(= % "done") status)
                            (swap! state update :pending dissoc id))
                          
                          (when value
                            (let [inner (edn/read-string value)
                                  [_ [_ _ _ inner-eid]] inner
                                  
                                  {:keys [tx-data tempids]}
                                  (d/transact! conn
                                               (->> inner
                                                    (into [nn
                                                           [:db/add inner-eid :eval/of (:db/id target)]
                                                           (when-let [old-next (:seq/next spine)]
                                                             [:db/add (:db/id nn) :seq/next (:db/id old-next)])])))]
                              ;; (println "Eval tx data")
                              ;; (run! prn tx-data)
                              ;; (println "Tempids")
                              ;; (run! prn tempids)
                              )
                            
                            #_(core/send! bus [:eval-result (:db/id target) value]))))
                 ;; send it
                 (.write sock (benc/encode {:id      id
                                            :op      :eval
                                            :session session
                                            :code    code}))))))))))

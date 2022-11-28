(ns sted.comp.demo
  (:require
   [sted.embed :as e]
   [sted.schema :as s]
   [sted.embed.data :as sed]
   [datascript.core :as d]
   [sted.comp.root :as cr]
   [clojure.string :as string]
   [rum.core :as rum]
   [sted.db-reactive :as dbrx]
   [sted.core :as core
    :refer [get-selected-form
            move-selection-tx]]))


(defn create-conn
  [{:keys [form]}]
  (doto (d/create-conn s/schema)
    (d/transact! (into [{:db/ident :sted.page/state
                         :state/bar {:db/id "bar"
                                     :coll/type :bar
                                     :coll/contains "chain"
                                     :seq/first {:db/id "chain"
                                                 :coll/type :chain
                                                 :seq/first "fcell"}}}]
                       (sed/go form 0xffffffff "fcell" "chain")))))

(rum/defc demo
  [opts rec classes]
  (let [conn (create-conn opts)
        app (core/app conn)]
    #_(println "Conn" conn)
    [:div {:class (str "alternate-reality " classes)}
     ^:inline (cr/root app rec)]))

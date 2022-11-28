(ns sted.comp.root
  (:require
   [datascript.core :as d]
   [sted.core :as core]
   [rum.core :as rum]
   [sted.comp.common :as cc]))

(rum/defc root
  [{:keys [conn bus] :as app} rec]
  (let [db @conn
        state (d/entity db :sted.page/state)
        ml-ref (rum/create-ref)]
    (js/console.log "Conn" conn)
    (println "Render root" (core/uniqueid bus))
    [:div.bar-container {}
     (rum/bind-context [cc/*modeline-ref* ml-ref]
                       (rec (:state/bar state) bus 0 nil))
     [:div.modeline-outer {:id "modeline" :ref ml-ref}]]))


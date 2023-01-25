(ns sted.comp.root
  (:require
   [datascript.core :as d]
   [sted.core :as core]
   [rum.core :as rum]
   [sted.comp.common :as cc]
   [sted.comp.search :as cs]))

(rum/defc root
  [{:keys [conn bus] :as app} rec]
  (let [db @conn
        state (d/entity db :sted.page/state)
        ml-ref (rum/create-ref)
        [h set-h!] (rum/use-state nil)]
    
    [:div.bar-container {}
     (rum/bind-context
      [cc/*modeline-ref* ml-ref]
      (rec (:state/bar state) bus 0 nil))
     
     [:div.modeline-outer.code-font {:id "modeline" :ref ml-ref}
      (cs/rs** bus (d/entity db :search/state))]]))


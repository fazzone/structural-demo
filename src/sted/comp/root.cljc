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
        ml-ref (rum/create-ref)]
    #_(js/console.log "Conn" conn)
    #_(println "Render root" (core/uniqueid bus))
         ;; "Test stuff"
     ;; [:div {:style {:display :flex :flex-direction "column"}}
     ;;  (cs/testcomp "Binkus" "nku")
     ;;  (cs/testcomp "smalltext" "small")
      
     ;;  ]
     ;; [:hr]

    [:div.bar-container {}
     (rum/bind-context [cc/*modeline-ref* ml-ref]
                       (rec (:state/bar state) bus 0 nil))
     [:div.modeline-outer {:id "modeline" :ref ml-ref}]]))


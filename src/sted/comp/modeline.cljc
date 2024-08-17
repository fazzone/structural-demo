(ns sted.comp.modeline
  (:require
   [datascript.core :as d]
   [rum.core :as rum]
   [sted.comp.edit-box]
   [sted.cmd.nav :as nav]
   [sted.db-reactive :as dbrx]
   [sted.comp.common :as cc]
   [sted.comp.inspect :as ci]
   [sted.core :as core :refer [get-selected-form
                               move-selection-tx]]
   [goog.string :as gstring]))

(def save-status (atom nil))

(def kick (atom 0))

(rum/defc modeline-inner < rum/reactive
  [sel bus rec]
  (let [db (d/entity-db sel)]
    (rum/fragment
     [:span.modeline-content.mlc {}
      [:span {:style {:margin-left "1ex"}}
       [:span.mlgloss {} "eid"]
       (str (:db/id sel))]
      [:span {}
       [:span.mlgloss {} "tx"]
       (.toString (:max-tx db) 16)]
      [:span {}
       [:span.mlgloss {} "type"]
       (str (or (:token/type sel) (:coll/type sel)))]
      

      (when-let [v (some-> sel :nav/pointer meta)]
        [:span {} [:span.mlgloss {} "nav/pointer"] (str v)])

      (when-let [v (:db/ident sel)]
        [:span {} [:span.mlgloss {} "ident"] (str v)])

      (when-let [v (:eval/action sel )]
        [:span {} [:span.mlgloss {} "action"] (str v)])

      #_(when-let [v (:chain/filename sel )]
        [:span {} [:span.mlgloss {} "file"] (str v)])

      (when-let [v (loop [s sel]
                     (or (:chain/filename s)
                         (some->> s :coll/_contains core/exactly-one recur)))]
        
        [:span {} [:span.mlgloss {} "file"] (str v)])

      (when-let [v (:handle/token sel )]
        [:span {} [:span.mlgloss {} "handle"] (str v)])

      (when-let [v (:eval/of sel)]
        [:span {} [:span.mlgloss {} "eval/of"] (str (:db/id v))])
      
      #_[:span {}
         (str (some-> sel :nav/pointer meta)
              " " (:db/ident sel)

              " " (:eval/action sel)
              " " (:handle/token sel)
              " " (:chain/filename sel)
            
              )]])))

(rum/defc modeline-nest-next < rum/reactive
  [sel bus rec]
  (let [ k (rum/react kick)]
    (rum/with-context [my-ref cc/*modeline-ref*]
      (some->> my-ref
               (rum/deref)
               (rum/portal (modeline-inner sel bus rec))))))

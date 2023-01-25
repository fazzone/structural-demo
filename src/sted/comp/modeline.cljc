(ns sted.comp.modeline
  (:require
   [datascript.core :as d]
   [rum.core :as rum]
   [sted.comp.edit-box]
   [sted.comp.search :as cs]
   [sted.cmd.nav :as nav]
   [sted.db-reactive :as dbrx]
   [sted.comp.common :as cc]
   [sted.comp.edit-box :as eb]
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
     [:span.modeline-content {}
      ^String (str (:db/id sel)
                   "/"
                   (:max-tx db)
                   " " (some-> sel :nav/pointer meta)
                   " " (:db/ident sel)
                   " " (or (:token/type sel) (:coll/type sel))
                    
                   " " (:handle/token sel)
                   " " (:chain/filename sel)
                   )])))

(rum/defc modeline-nest-next < rum/reactive
  [sel bus rec]
  (let [ k (rum/react kick)]
    (rum/with-context [my-ref cc/*modeline-ref*]
      (some->> my-ref
               (rum/deref)
               (rum/portal (modeline-inner sel bus rec))))))

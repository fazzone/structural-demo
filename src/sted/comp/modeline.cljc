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

(rum/defc modeline-inner < rum/reactive
  [sel bus rec eps]
  (let [db (d/entity-db sel)
        {:keys [^String text valid]} (some-> eps rum/react)]
    
    (println "ML?" (:db/id sel))
    #_(println "ML"
               (keys (core/get-app bus))
               (pr-str (type (:system (core/get-app bus)))))
    
    [:span {:class (str "modeline code-font"
                        #_(if text " editing modeline-search" " modeline-fixed")
                        "modeline-fixed"
                        #_(when (and (not (empty? text)) (not valid)) " invalid"))}
     [:span.modeline-echo
      {}
      (let [{:keys [on at status file]} (rum/react save-status)]
        (when (= on (:db/id sel))
          (case status
            :saving "Saving"
            :ok (str file "@" at)
            :error "Error"
            "")))]
    
     #_(when text ^:inline (cs/results db bus :token/value text rec))
    
     [:span.modeline-content {}
      ^String (str (:db/id sel)
                   "/"
                   (:max-tx db)
                   " " (some-> sel :nav/pointer meta)
                   " " (:db/ident sel)
                   " " (or (:token/type sel) (:coll/type sel))
                   
                   " " (:handle/token sel)
                   
                   )]
    
     #_(when-some [insp (js/document.getElementById "inspector")]
         (rum/portal (ci/inspect-inner (d/entity-db sel) bus) insp))
    
     #_[:input.edit-box.code-font {:type :text}]]))

(rum/defc modeline-nest-next
  [sel bus rec]
  (rum/with-context [my-ref cc/*modeline-ref*]
    (println "MLnnref" my-ref)
    (some->> my-ref
             (rum/deref)
             (rum/portal (modeline-inner sel bus rec eb/editbox-ednparse-state)))))

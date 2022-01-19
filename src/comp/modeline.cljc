(ns comp.modeline
  (:require
   [datascript.core :as d]
   [rum.core :as rum]
   [comp.edit-box]
   [comp.search]
   [cmd.nav :as nav]
   [db-reactive :as dbrx]
   [comp.common :as cc]
   [comp.edit-box :as eb]
   [core :as core
    :refer [get-selected-form
            move-selection-tx]]))

(def save-status (atom nil))


(rum/defc modeline-inner < rum/reactive
  [sel bus rec eps]
  (let [{:keys [^String text valid]} (some-> eps rum/react)]
   [:span {:class (str "modeline code-font"
                       (if text " editing modeline-search" " modeline-fixed")
                       (when (and (not (empty? text)) (not valid)) " invalid"))}
    [:span.modeline-echo
     {}
     (let [{:keys [on at status file]} (rum/react save-status)]
       (when (= on (:db/id sel))
         (case status
           :saving "Saving"
           :ok (str file "@" at)
           :error "Error"
           "")))]
    (if text
      ^:inline (comp.search/results (d/entity-db sel) bus :token/value text rec)
      #_(stupid-symbol-search (d/entity-db sel)  :token/value text)
      [:span.modeline-content
       (if-not sel
         "(no selection)"
         (str "#" (:db/id sel)
              " "
              (or (:coll/type sel)
                  #_(pr-str (d/touch sel)))))])
    #_[:input.edit-box.code-font {:type :text}]]))

(rum/defc modeline-nest-next 
  [sel bus rec]
  (rum/with-context [my-ref cc/*modeline-ref*]
    (some->> my-ref
             (rum/deref)
             (rum/portal (modeline-inner sel bus rec eb/editbox-ednparse-state)))))






(ns comp.modeline
  (:require
   [datascript.core :as d]
   [rum.core :as rum]
   [comp.edit-box]
   [comp.search]
   [cmd.nav :as nav]
   [db-reactive :as dbrx]
   [comp.common :as cc]
   [core :as core
    :refer [get-selected-form
            move-selection-tx]]))

(def save-status (atom nil))


(rum/defc modeline-inner < rum/reactive
  [sel bus {:keys [^String text valid] :as edn-parse-state}]
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
     (comp.search/results (d/entity-db sel)  :token/value text)
     #_(stupid-symbol-search (d/entity-db sel)  :token/value text)
     [:span.modeline-content
      (if-not sel
        "(no selection)"
        (str "#" (:db/id sel)
             " " (:coll/type sel)))])])

(rum/defc modeline-portal  < rum/reactive (dbrx/areactive :form/highlight :form/editing)
  [db bus]
  (let [sel (get-selected-form db)
        rpa (reverse (nav/parents-vec sel))
        nn (.getElementById js/document (cc/modeline-portal-id (:db/id (first rpa))))]
    (when nn
      (-> (modeline-inner
           sel
           bus
           (when (:form/editing sel)
             (rum/react comp.edit-box/editbox-ednparse-state)))
          (rum/portal nn)))))






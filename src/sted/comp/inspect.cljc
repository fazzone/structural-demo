(ns sted.comp.inspect
  (:require
   [rum.core :as rum]
   [sted.schema :as s]
   [sted.db-reactive :as dbrx]
   [datascript.core :as d]
   [sted.comp.debug :as cd]
   [sted.core :as core :refer [get-selected-form]]))

(rum/defc inspect-inner #_(dbrx/areactive :form/highlight :form/edited-tx)
  [db bus]
  (let [sel (get-selected-form db)]
    [:div.inspector.code-font
     [:span.form-title (str "Inspect #" (:db/id sel))]
     [:div ^:inline (cd/datoms-table-eavt*
                     (concat (d/datoms db :eavt  (:db/id sel))
                             (for [[a s] s/schema
                                   :when (= :db.type/ref (:db/valueType s))
                                   d (d/datoms db :avet a (:db/id sel))]
                               d)))]]))

(rum/defc inspect-portal
  []
  [:div#inspector])

(defn inspect [me bus]
  (inspect-inner (d/entity-db me) bus))

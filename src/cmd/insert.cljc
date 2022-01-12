(ns cmd.insert
  (:require
   [cmd.edit :as edit]
   [datascript.core :as d]
   [cmd.move :as move]
   [clojure.string :as string]
   [embed :as e]
   [clojure.edn :as edn]
   [core :as core :refer [get-selected-form
                          move-selection-tx]]))

(defn try-parse-edn
  [s]
  (try
    (println "Try to parse as code" (pr-str s))
    (e/string->tx s)
    (catch #?(:cljs js/Error :clj Exception) e
      (println "No edn" s)
      (js/console.log e)
      nil)))

(defn parse-token-tx
  [s eid]
  (println "PTT" eid (pr-str s))
  (when-not (empty? s)
    (some-> (or (try-parse-edn s)
                (and (string/starts-with? s "\"" )
                     (try-parse-edn (str s "\""))))
            (assoc :db/id eid))))

(defn accept-edit-tx
  [form-eid value]
  [{:db/id :db/current-tx
    :edit/of form-eid}
   
   [:db/retract form-eid :token/value]
   [:db/retract form-eid :token/type]
   
   (parse-token-tx value form-eid)
   [:db/add form-eid :form/edited-tx :db/current-tx]

   [:db/retract form-eid :form/editing true]
   [:db/retract form-eid :form/edit-initial]])

(defn reject-edit-tx
  [db form-eid]
  (concat (move/movement-tx db :move/backward-up)
          (edit/form-delete-tx (d/entity db form-eid))))

(defn wrap-edit-tx
  [ed ct value]
  (into [[:db/retract (:db/id ed) :form/editing true]
         {:db/id "newnode"
          :coll/type ct
          :seq/first {:db/id "inner"
                      :coll/_contains "newnode"
                      :form/edit-initial (or value "")
                      :form/editing true}}]
        (concat
         (edit/form-overwrite-tx ed "newnode")
         (move-selection-tx (:db/id ed) "inner")
         (when (:form/linebreak ed)
           [[:db/retract (:db/id ed) :form/linebreak true]
            [:db/add "newnode" :form/linebreak true]]))))

(defn finish-edit-tx
  [db eid text]
  (if (empty? text)
    (reject-edit-tx db eid)
    (accept-edit-tx eid text)))

(defn finish-edit-and-move-up-tx
  [db eid text]
  (if (empty? text)
    (reject-edit-tx db eid)
    (into (accept-edit-tx eid text)
          (move/movement-tx db :move/up))))

(defn finish-edit-and-edit-next-tx
  [ed text]
  (into (accept-edit-tx (:db/id ed) text)
        (edit/insert-editing-after ed)))




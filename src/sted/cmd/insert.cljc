(ns sted.cmd.insert
  (:require
   [sted.cmd.edit :as edit]
   [datascript.core :as d]
   [sted.cmd.move :as move]
   [clojure.string :as string]
   [sted.embed :as e]
   [clojure.edn :as edn]
   [sted.core :as core :refer [get-selected-form
                               move-selection-tx]]))

(defn parse-editbox-tx
  [s eid]
  (when-not (empty? s)
    (some-> (or (e/parse-token-tx s)
                (and (string/starts-with? s "\"")
                     (e/parse-token-tx (str s "\"")))
                nil)
            ;; todo - handle complex paste (don't clobber tempid)
            (assoc :db/id eid))))

(defn accept-edit-tx
  [form-eid value]
  (if-not form-eid
    (println "NO FORM EDIT!" value)
    (when-some [ptx (parse-editbox-tx value form-eid)]
      [{:db/id :db/current-tx
        :edit/of form-eid}
       [:db/retract form-eid :token/value]
       [:db/retract form-eid :token/type]
       ptx
       [:db/add form-eid :form/edited-tx :db/current-tx]
       [:db/retract form-eid :form/editing true]
       [:db/retract form-eid :form/edit-initial]])))

(defn reject-edit-tx
  [db form-eid]
  (let [e (d/entity db form-eid)]
   (concat (move/backward-up e)
           (edit/form-delete-tx e))))

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
    (some-> (accept-edit-tx eid text)
            (into (move/movement-tx db move/up)))))

(defn finish-edit-and-edit-next-tx
  [ed text]
  (some-> (accept-edit-tx (:db/id ed) text)
          (into (edit/insert-editing-after ed))))

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

(defn parse-token-tx
  [s eid]
  (let [quote-start (string/starts-with? s "\"" )]
    ;; cannot be a valid leaf, must be a string
    (if (or quote-start (string/includes? s " "))
      {:db/id eid
       :string/value
       (subs s
             (if-not quote-start 0 1)
             (- (count s)
                (if-not (string/ends-with? s "\"" )
                  0
                  1)))}
      (try
        (-> s
            (edn/read-string)
            (e/->tx)
            (assoc :db/id eid))
        (catch #?(:cljs js/Error :clj Exception) e
          #_(println "No edn" text-value)
          #_(js/console.log e)
          nil)))))

(defn accept-edit-tx
  [form-eid value]
  [{:db/id :db/current-tx
    :edit/of form-eid}
   [:db/retract form-eid :symbol/value]
   [:db/retract form-eid :keyword/value]
   [:db/retract form-eid :string/value]
   [:db/retract form-eid :number/value]
   (parse-token-tx value form-eid)
   [:db/add form-eid :form/edited-tx :db/current-tx]

   [:db/retract form-eid :form/editing true]
   [:db/retract form-eid :form/edit-initial]])

(defn reject-edit-tx
  [db form-eid]
  (concat (move/movement-tx db :move/backward-up)
          (edit/form-delete-tx (d/entity db form-eid))))

(defn wrap-edit-tx
  [db form-eid ct value]
  (into [[:db/retract form-eid :form/editing true]
         {:db/id "newnode"
          :coll/type ct
          :seq/first {:db/id "inner"
                      :coll/_contains "newnode"
                      :form/edit-initial (or value "")
                      :form/editing true}}]
        (concat
         (edit/form-overwrite-tx (d/entity db form-eid) "newnode")
         (move-selection-tx form-eid "inner"))))

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
  [db eid text]
  (into (accept-edit-tx eid text)
        (edit/insert-editing-tx db :after "")))


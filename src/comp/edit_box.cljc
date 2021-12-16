(ns comp.edit-box
  (:require
   [clojure.edn :as edn]
   [cmd.move :as move]
   [cmd.edit :as edit]
   [datascript.core :as d]
   [cljs.core.async :as async]
   [embed :as e]
   [rum.core :as rum]
   [clojure.string :as string]
   [core :as core :refer [get-selected-form
                          move-selection-tx]]))


;; edit box


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
        (catch js/Error e
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

(defn editbox-keydown-mutation
  [text key]
  (case key
    "Escape"      [:edit/reject]
    "Backspace"   (when (empty? text)
                    [:edit/reject])
    "Enter"       [:edit/finish text]
    (")" "]" "}") [:edit/finish-and-move-up text]
    ("[" "(" "{") [:edit/wrap (case key "(" :list "[" :vec "{" :map) text]
    " "           (cond
                    (empty? text)
                    [:edit/reject]
                    
                    (= "\"" (first text))
                    (println "Quotedstring")
                    
                    :else
                    [:edit/finish-and-edit-next-node text])
    nil))

(defn focus-ref-on-mount
  [ref-name]
  {:did-mount (fn [state]
                (some-> state :rum/react-component (.-refs) (aget ref-name) (.focus))
                state)})

(def global-editing-flag (atom false))
(def editing-when-mounted
  {:did-mount (fn [state]
                (reset! global-editing-flag true)
                state)
   :will-unmount (fn [state]
                   (reset! global-editing-flag false)
                   state)})

(def editbox-ednparse-state (atom nil))

(rum/defcs edit-box
  < (rum/local [] ::text) (focus-ref-on-mount "the-input") editing-when-mounted
  [{::keys [text]} e bus]
  (let [value (if (= [] @text)
                (or (:form/edit-initial e)
                    (some-> (:symbol/value e) str)
                    (some-> (:keyword/value e) str)
                    (some-> (:string/value e) pr-str)
                    (some-> (:number/value e) str))
                @text)
        form-eid (:db/id e)]
    [:input.edit-box.code-font
     {:type        :text
      :ref         "the-input"
      :value       (or value "")
      :style       {:width (str (max 1 (inc (count value))) "ch")}
      :on-change   #(let [new-text (string/triml (.-value (.-target %)))
                          token (parse-token-tx new-text form-eid)]
                      (reset! text new-text)
                      (reset! editbox-ednparse-state
                              {:form-eid form-eid
                               :text new-text
                               :valid (some? token)
                               :type (some-> token first val)})
                      (prn "PTT" token )
                      )
      :on-key-down (fn [ev]
                     (when-let [mut (editbox-keydown-mutation value (.-key ev))]
                       (.preventDefault ev)
                       (.stopPropagation ev)
                       (core/send! bus mut)
                       #_(async/put! bus mut)))
      ;; :on-blur #(pub! [:edit/finish @text])
      }]))


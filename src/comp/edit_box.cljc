(ns comp.edit-box
  (:require
   [clojure.edn :as edn]
   [cmd.move :as move]
   [cmd.edit :as edit]
   [cmd.insert :as insert]
   [datascript.core :as d]
   [cljs.core.async :as async]
   [embed :as e]
   [rum.core :as rum]
   [clojure.string :as string]
   [core :as core :refer [get-selected-form
                          move-selection-tx]]))


;; edit box

(defn editbox-keydown-mutation
  [text key]
  (case key
    "Escape"      [:edit/reject]
    "Backspace"   (when (empty? text)
                    [:edit/reject])
    "Enter"       [:edit/finish text]
    "C-/"         [:undo]
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

(defn event->kbd
  [^KeyboardEvent ev]
  (str (when (.-altKey ev) "M-")
       (when (.-ctrlKey ev ) "C-")
       (when (.-shiftKey ev) "S-")
       (.-key ev)))

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
      :style       {:width (str (max 1 (count value)) "ch")}
      :on-change   #(let [new-text (string/triml (.-value (.-target %)))
                          token (insert/parse-token-tx new-text form-eid)]
                      (reset! text new-text)
                      (reset! editbox-ednparse-state
                              {:form-eid form-eid
                               :text new-text
                               :valid (some? token)
                               :type (some-> token first val)})
                      #_(prn "PTT" token )
                      nil
                      )
      :on-key-down (fn [ev]
                     (when-let [mut (editbox-keydown-mutation
                                     value
                                     (event->kbd ev))]
                       (.preventDefault ev)
                       (.stopPropagation ev)
                       (core/send! bus mut)
                       #_(async/put! bus mut)))
      ;; :on-blur #(pub! [:edit/finish @text])
      }]))


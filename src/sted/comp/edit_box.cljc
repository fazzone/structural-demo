(ns sted.comp.edit-box
  (:require
   [clojure.edn :as edn]
   [sted.cmd.move :as move]
   [sted.cmd.edit :as edit]
   [sted.cmd.insert :as insert]
   [sted.sys.kbd.evt :as ske]
   [datascript.core :as d]
   [cljs.core.async :as async]
   [sted.embed :as e]
   [rum.core :as rum]
   [clojure.string :as string]
   [sted.core :as core :refer [get-selected-form
                               move-selection-tx]]))

;; edit box


(defn editbox-keydown-mutation
  [text key]
  (case key
    "Escape"          [:edit/reject]
    "Backspace"       (when (empty? text)
                        [:edit/reject])
    "Enter"           [:edit/finish text]
    "C-/"             [:undo]
    ("S-)" "]" "S-}") [:edit/finish-and-move-up text]
    "["               [:edit/wrap :vec text]
    "S-("             [:edit/wrap :list text]
    "S-{"             [:edit/wrap :map text]
    (" " "S- ")       (cond
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
                #_(some-> state :rum/react-component (.-refs) (aget ref-name) (.focus))
                (when-let [el (some-> state :rum/react-component (.-refs) (aget ref-name))]
                  (when-not (.closest el ".alternate-reality")
                    (.focus el)))
                state)})

(def editbox-ednparse-state (atom nil))

(rum/defcs edit-box
  < (rum/local [] ::text) 
  (focus-ref-on-mount "the-input")
  [{::keys [text]} e bus]
  #_(println "Edit box" 'text text 'e e)
  (let [value (if (= [] @text)
                (or (:form/edit-initial e)
                    (:token/value e))
                @text)
        form-eid (:db/id e)]
    [:input.edit-box.code-font
     #_:textarea.edit-box.code-font
     {:type        :text
      ;; :wrap :off
      :ref         "the-input"
      :value       (or value "")
      :style       {:width (str (max 1 (count value)) "ch")}
      :on-change   #(let [new-text (string/triml (.-value (.-target %)))
                          token (e/parse-token-tx new-text)]
                      (reset! text new-text)
                      (reset! editbox-ednparse-state
                              {:form-eid form-eid
                               :text new-text
                               :valid (some? token)
                               :type (some-> token first val)})
                      nil)
      :on-key-down (fn [ev]
                     (let [kbd (ske/event->kbd ev)
                           mut (editbox-keydown-mutation value kbd)]
                       (when mut
                         (.preventDefault ev)
                         (.stopPropagation ev)
                         (when (not= :edit/wrap (first mut))
                           (reset! editbox-ednparse-state nil))
                         (some->> mut (core/send! bus))
                         #_(async/put! bus mut))))}]))

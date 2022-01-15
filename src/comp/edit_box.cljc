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

(def global-editing-flag (atom false))

(defn editing-when-mounted
  [ref-name]
  (letfn [(setgef [state v]
            (let [el (some-> state :rum/react-component (.-refs) (aget ref-name))
                  ar (some-> el (.closest ".alternate-reality"))]
              (when (and el (nil? ar))
                (reset! global-editing-flag v)))
            state)]
    {:did-mount    (fn [state] (setgef state true))
     :will-unmount (fn [state] (setgef state false))}))

(def editbox-ednparse-state (atom nil))

(defn event->kbd
  [^KeyboardEvent ev]
  (str (when (.-altKey ev) "M-")
       (when (.-ctrlKey ev) "C-")
       (when (.-shiftKey ev) "S-")
       (.-key ev)))

(rum/defcs edit-box
  < (rum/local [] ::text) (focus-ref-on-mount "the-input") (editing-when-mounted "the-input")
  [{::keys [text]} e bus]
  (println "Edit box" 'text text 'e e)
  (let [value (if (= [] @text)
                (or (:form/edit-initial e)
                    (:token/value e))
                @text)
        form-eid (:db/id e)]
    [:input.edit-box.code-font
     #_:textarea.edit-box.code-font
     {
      :type        :text
      ;; :wrap :off
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
                      #_(prn "PTT" token)
                      nil)
      :on-key-down (fn [ev]
                     (when-let [mut (editbox-keydown-mutation
                                     value
                                     (event->kbd ev))]
                       (.preventDefault ev)
                       (.stopPropagation ev)
                       (core/send! bus mut)
                       #_(async/put! bus mut)))
      ;; :on-blur #(pub! [:edit/finish @text])}]))
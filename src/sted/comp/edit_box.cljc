(ns sted.comp.edit-box
  (:require
   [clojure.edn :as edn]
   [sted.cmd.move :as move]
   [sted.cmd.edit :as edit]
   [sted.cmd.insert :as insert]
   [sted.sys.kbd.evt :as ske]
   [sted.sys.search.dom :as s]
   [sted.comp.search :as cs]
   [datascript.core :as d]
   [cljs.core.async :as async]
   [sted.embed :as e]
   [rum.core :as rum]
   [clojure.string :as string]
   [sted.core :as core :refer [get-selected-form
                               move-selection-tx]]))


;; edit box


(defn select-nth-search-result
  [n]
  (println "SNSR" n)
  (when-let [eid (some-> (js/document.getElementById (str "sr" n))
                         (.closest ".tk")
                         (.-dataset)
                         (.-eid)
                         (js/parseInt))]
    [:edit/reject-and-select eid]))


(defn editbox-keydown-mutation
  [e text key]
  (case key
    "Escape" [:edit/reject]
    "Backspace" (when (empty? text)
                  [:edit/reject])
    "Enter" (if (:form/edit-comp e)
              [:edit/finish-and-move-up text]
              [:edit/finish text])
    "C-/" [:undo]
    ("S-)" "]" "S-}") [:edit/finish-and-move-up text]
    "[" [:edit/wrap :vec text]
    "S-(" [:edit/wrap :list text]
    "S-{" [:edit/wrap :map text]
    "M-1" (select-nth-search-result 1)
    "M-2" (select-nth-search-result 2)
    "M-3" (select-nth-search-result 3)
    "M-4" (select-nth-search-result 4)
    "M-5" (select-nth-search-result 5)
    "M-6" (select-nth-search-result 6)
    "M-7" (select-nth-search-result 7)
    "M-8" (select-nth-search-result 8)
    "M-9" (select-nth-search-result 9)
    (" " "S- ") (cond
                  (empty? text) [:edit/reject]
                  (= "\"" (first text)) (println "Quotedstring")
                  #_(:form/edit-comp e)
                  #_[:edit/finish-and-move-up text]
                  :else [:edit/finish-and-edit-next-node text])
    nil))


(defn focus-ref-on-mount
  [ref-name]
  {:did-mount (fn [state]
                #_(some-> state :rum/react-component (.-refs) (aget ref-name) (.focus))
                (when-let [el (some-> state :rum/react-component (.-refs) (aget ref-name))]
                  #_(when-not (.closest el ".alternate-reality")
                    (.focus el))
                  (if-not (.closest el ".alternate-reality")
                    (.focus el)
                    (set! (.-tabindex el) -1)))
                state)})


(def editbox-ednparse-state (atom nil))


(rum/defcs edit-box
  < (rum/local [] ::text)
  (focus-ref-on-mount "the-input")
  [{::keys [text] :as ebst} e bus rec]
  (let [value    (if (= [] @text)
                   (or (:form/edit-initial e)
                       (:token/value e))
                   @text)
        form-eid (:db/id e)]
    (rum/fragment
     [:input.edit-box
      #_.code-font
      #_:textarea.edit-box.code-font
      {:type        :text
       ;; :wrap :off
       :ref         "the-input"
       :spellCheck  "false"
       :value       (or value "")
       :style       {:width (str (max 1 (count value)) "ch")}
       :on-change   #(let [new-text (string/triml (.-value (.-target %)))
                           token    (e/parse-token-tx new-text)]
                       (reset! text new-text)
                       #_(prn "EBonchange" (type bus) new-text)
                       (core/send! bus [:update-search new-text])
                       (reset! editbox-ednparse-state
                               {:form-eid form-eid
                                :text     new-text
                                :valid    (some? token)
                                :type     (some-> token first val)})
                       nil)
       :on-key-down (fn [ev]
                      (let [kbd (ske/event->kbd ev)
                            mut (editbox-keydown-mutation e value kbd)]
                        (when mut
                          (.preventDefault ev)
                          (.stopPropagation ev)
                          (when (not= :edit/wrap (first mut))
                            (reset! editbox-ednparse-state nil))
                          (println "Sending" mut)
                          (core/send! bus mut))))}]
     
     #_(when value ^:inline (cs/results (d/entity-db e) bus :token/value value rec))
     
     )))

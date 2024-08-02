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

(defonce last-entered-token (atom nil))

(defn editbox-keydown*
  [e text key]
  (case key
    "Escape"          {:msg [:edit/reject]}
    "Backspace"       (when (empty? text)
                        {:msg [:edit/reject]})
    "Enter"           (if (:form/edit-comp e)
                        {:accept true :msg [:edit/finish-and-move-up text]}
                        {:accept true :msg [:edit/finish text]})
    "C-/"             {:msg [:undo]}
    ("S-)" "]" "S-}") {:accept true :msg [:edit/finish-and-move-up text]}
    "["               {:msg [:edit/wrap :vec text]}
    "S-("             {:msg [:edit/wrap :list text]}
    "S-{"             {:msg [:edit/wrap :map text]}
    "C- "             {:new-text @last-entered-token}
    (" " "S- ")       (cond
                        (empty? text)         {:msg [:edit/reject]}
                        (= "\"" (first text)) (println "Quotedstring")
                        #_                    (:form/edit-comp e)
                        #_                    [:edit/finish-and-move-up text]
                        :else                 {:accept true :msg [:edit/finish-and-edit-next-node text]})
    nil))

(rum/defc edit-box
  [e bus r]
  (let [[text set-text!] (rum/use-state nil)
        value    (or text
                     (:form/edit-initial e)
                     (:token/value e))
        form-eid (:db/id e)
        iref (rum/create-ref)]
    
    (rum/use-layout-effect!
     (fn []
       (when-let [el (rum/deref iref)]
         (if-not (.closest el ".alternate-reality")
           (.focus el)
           (set! (.-tabindex el) -1)))
       nil)
     [])
    
    [:input.edit-box
     {:type        :text
      :ref         iref
      :spellCheck  "false"
      :value       (or value "")
      :style       {:width (str (max 1 (count value)) "ch")}
      :on-change   #(let [new-text (string/triml (.-value (.-target %)))
                          token    (e/parse-token-tx new-text)]
                      (set-text! new-text)
                      nil)
      :on-key-down (fn [ev]
                     (let [kbd (ske/event->kbd ev)
                           {:keys [msg accept new-text]} (editbox-keydown* e value kbd)]
                       (println "EditboxKeydown " kbd accept msg)
                       (cond
                         msg
                         (do (.preventDefault ev)
                             (.stopPropagation ev)
                             (when accept
                               (reset! last-entered-token value))
                             (core/send! bus msg))
                         
                         new-text
                         (set-text! new-text)
                         
                         
                         )))}]))

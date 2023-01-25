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
    (" " "S- ") (cond
                  (empty? text) [:edit/reject]
                  (= "\"" (first text)) (println "Quotedstring")
                  #_(:form/edit-comp e)
                  #_[:edit/finish-and-move-up text]
                  :else [:edit/finish-and-edit-next-node text])
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
                           mut (editbox-keydown-mutation e value kbd)]
                       (when mut
                         (.preventDefault ev)
                         (.stopPropagation ev)
                         (core/send! bus mut))))}]))

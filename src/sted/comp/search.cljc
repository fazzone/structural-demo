(ns sted.comp.search
  (:require [datascript.core :as d]
            [rum.core :as rum]
            [clojure.string :as string]
            [sted.cmd.move :as move]
            [sted.comp.common :as cc]
            [sted.sys.search.db :as sdb]
            [sted.sys.kbd.evt :as ske]
            [sted.core :as core]
            [sted.sys.search.dom :as s]
            #_[comp.code :as code]))

(rum.core/set-warn-on-interpretation! true)

(defn search*
  [db sa text]
  ;; U+10FFFF
  (d/index-range db sa text (str text "􏿿")))

(rum/defc hlp
  [tkl off len jj utext? utk?]
  (let [left (- tkl)
        sj (str jj)]
    [:span {:style {:position :relative}}
     (cond
       (not (or utext? utk?))
       (rum/fragment
        [:span.hlp {:style {:width (str len "ch")
                            :left (str (+ off left)
                                       "ch")}}]
        [:span.hlp-tag
         {:id (str "sr" sj)
          :style {:left (str (+ off left) "ch")}}
         sj #_(subs sj 0 1)])
       
       utk?
       [:span.hlp.unique-token {:style {:width (str tkl "ch")
                                        :left (str left "ch")}}]
       :else
       [:span.hlp.unique-text {:style {:width (str tkl "ch")
                                       :left (str left "ch")}}])]))

(rum/defc dbsr
  [db text rec]
  [:div.search
   (for [[v [[e] :as ds]] (sdb/db-prefix-search db text 32)]
     (let [ent (d/entity db e)
           k (* 3 e)]
       (rum/fragment
        {:key (- k)}
        ;; ^:inline (rec ent core/blackhole 0 nil)
        (rum/bind-context [cc/*indenter* nil]
                          ^:inline (rec ent core/blackhole 0 nil))
        [:span {:key (- 1 k)} "x" (count ds)]
        [:span.last {:key (- 2 k)}
         (pr-str (take 5 (map first ds)))])))])



(defn dom-result-highlights!
  [text results]
  (let [unique-text?  (= 1 (count results))
        unique-token? (and unique-text?
                           false
                           #_(= 1 (count (val (first results)))))
        jj            (volatile! 0)]

    (for [^js r results]
      (->> (.-element r)
           (rum/portal (hlp (count (.-match r))
                            (.-index r)
                            (count text)
                            (vswap! jj inc)
                            unique-text?
                            unique-token?))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defn searchbox-keydown-mutation
  [key]
  (case key
    "1"      [:search/select 1]
    "2"      [:search/select 2]
    "3"      [:search/select 3]
    "4"      [:search/select 4]
    "5"      [:search/select 5]
    "6"      [:search/select 6]
    "7"      [:search/select 7]
    "8"      [:search/select 8]
    "9"      [:search/select 9]
    "Enter"  [:search/select 1]
    "Escape" [:search/cancel]
    
    nil))

(rum/defc search-box
  [bus]
  (let [[text set-text!] (rum/use-state "")
        iref (rum/create-ref)]
    
    (rum/use-layout-effect!
     (fn []
       (when-let [el (rum/deref iref)]
         (if-not (.closest el ".alternate-reality")
           (.focus el)
           (set! (.-tabindex el) -1)))
       nil)
     [])
    
    [:input.search-box
     {:type        :text
      :ref         iref
      :spellCheck  "false"
      :value       (or text "")
      :style       {:width (str (max 1 (count text)) "ch")}
      :on-change   #(let [new-text (string/triml (.-value (.-target %)))]
                      (set-text! new-text)
                      (core/send! bus [:update-search new-text])
                      nil)
      :on-key-down (fn [ev]
                     (let [kbd (ske/event->kbd ev)
                           mut (searchbox-keydown-mutation kbd)]
                        
                       (when mut
                         (.preventDefault ev)
                         (.stopPropagation ev)
                         (core/send! bus mut))
                       ))}]))


(rum/defc rs** < rum/reactive
  [bus sstate]
  (let [{:keys [text results] :as uu} (some-> sstate :bing (rum/react))
        bong (some-> sstate :bong (rum/react))]
    
    [:span.searcher {}
     (when bong "search: ")
     (when bong (search-box bus))
     (dom-result-highlights! text results)]))




(ns sted.comp.debug
  (:require
   [clojure.edn :as edn]
   [cljs.pprint :as pprint]
   [datascript.core :as d]
   [rum.core :as rum]
   [cljs.core.async :as async]))

(rum/defc datoms-table-eavt* [ds]
  (let [[se sa sv st :as widths] [5 20 32 8]]
    [:table {:style {:width (str (apply + widths) "ch")
                     :table-layout "fixed"}}
     [:colgroup
      [:col {:span 1 :style {:width (str se "ch")}}]
      [:col {:span 1 :style {:width (str sa "ch")}}]
      [:col {:span 1 :style {:width (str sv "ch")}}]
      [:col {:span 1 :style {:width (str st "ch")}}]]
     [:thead
      [:tr
       [:th {:scope "col" :style {:width (str se "ch")}} "E"]
       [:th {:scope "col" :style {:width (str sa "ch")}} "A"]
       [:th {:scope "col" :style {:width (str sv "ch")}} "V"]
       [:th {:scope "col" :style {:width (str st "ch")}} "T"]]]
     [:tbody
      {}
      (->> ds
           (map-indexed
            (fn [i [e a v t a?]]
              [:tr {:key   i
                    :class (str "eavt-row"
                                (when-not a? " retraction"))}
               [:td (str e)]
               [:td (str a)]
               [:td (let [s (str v)]
                      (if (> sv (count s))
                        s
                        [:abbr {:title s} (subs s 0 sv)]))]
               [:td (str t)]])))]]))



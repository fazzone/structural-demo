(ns sted.comp.debug
  (:require
   [clojure.edn :as edn]
   [cljs.pprint :as pprint]
   [datascript.core :as d]
   [rum.core :as rum]
   [cljs.core.async :as async]))

(rum/defc datoms-table-eavt* [ds {:keys [header? t?] :as opts
                                  :or {header? true
                                       t? true}}]
  (let [[se sa sv st :as widths] [5 20 20 (if t? 8 0)]]
    [:table {:cellSpacing 0
             :style {:width (str (apply + widths) "ch")
                     :height "min-content"
                     :table-layout "fixed"
                     ;; :text-align "right"
                     }}
     [:colgroup
      [:col {:span 1 :style {:width (str se "ch")}}]
      [:col {:span 1 :style {:width (str sa "ch")}}]
      [:col {:span 1 :style {:width (str sv "ch")}}]
      (when t? [:col {:span 1 :style {:width (str st "ch")}}])]
     (when header?
       [:thead
        [:tr
         [:th {:scope "col" :style {:width (str se "ch")}} "E"]
         [:th {:scope "col" :style {:width (str sa "ch")}} "A"]
         [:th {:scope "col" :style {:width (str sv "ch")}} "V"]
         (when t? [:th {:scope "col" :style {:width (str st "ch")}} "T"])]])
     [:tbody
      {}
      (->> ds
           (map-indexed
            (fn [i [e a v t a?]]
              [:tr {:key i
                    ;; :style {:height "1ch"}
                    :class (str "eavt-row"
                                (when-not a? " retraction"))}
               [:td (str e)]
               [:td (str a)]
               [:td (let [s (str v)]
                      (if (> sv (count s))
                        s
                        [:abbr {:title s} (subs s 0 sv)]))]
               (when t? [:td (str t)])])))]]))

(defn tx-data
  [ds]
  (datoms-table-eavt* ds {:t? nil
                          :header? nil}))





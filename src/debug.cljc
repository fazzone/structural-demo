(ns debug
  (:require
   [datascript.core :as d]
   [rum.core :as rum]))


(rum/defc datoms-table-eavt* [ds]
  [:table
   [:thead
    [:tr
     [:td {:style {:width "3em"}} "E"]
     [:td {:style {:width "20em"}} "A"]
     [:td {:style {:width "20em"}} "V"]
     [:td {:style {:width "10em"}} "T"]
     [:td "added?"]]]
   [:tbody
    {} 
    (->> ds
         (map-indexed
          (fn [i [e a v t r]]
            [:tr {:key i}
             [:td [:code (str e)]]
             [:td [:code (str a)]]
             [:td [:code (str v)]]
             [:td [:code (str t)]]
             [:td [:code (str r)]]])))]])



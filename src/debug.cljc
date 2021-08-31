(ns debug
  (:require
   [clojure.edn :as edn]
   [cljs.pprint :as pprint]
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
     #_[:td "added?"]]]
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
             #_[:td [:code (str r)]]])))]])

(rum/defc datoms-table-ave [ds ah vh eh]
  [:div.datoms-table.ave
   [:div (or ah "A")]
   [:div (or vh "V")]
   [:div (or eh "E")]
   (->> ds
        (map-indexed
         (fn [i [e a v t r]]
           (rum/fragment {:key i}
                         [:code {:key (+ (* 3 i) 1)} (str a)]
                         [:code {:key (+ (* 3 i) 2)} (str v)]
                         [:code {:key (+ (* 3 i) 3)} (str e)]))))])

(rum/defc datoms-table-eav [ds eh ah vh]
  [:div.datoms-table.eav
   [:div (or eh "E")]
   [:div (or ah "A")]
   [:div (or vh "V")]
   (->> ds
        (map-indexed
         (fn [i [e a v t r]]
           (rum/fragment
            {:key i}
            [:code {:key (+ (* 3 i) 3)} (str e)]
            [:code {:key (+ (* 3 i) 1)}
             (let [s (str a)
                   n 20]
               (if (> n (count s) )
                 s
                 [:abbr {:title s} (subs s 0 n) ])
               )]
            [:code {:key (+ (* 3 i) 2)} (str v)]))))])





(rum/defcs transaction-edit-area < (rum/local "" ::text) < (rum/local nil ::tx-result)
  [{::keys [text tx-result]} conn]
  [:div
   [:textarea {:value @text
               :style {:width "50%"
                       :height "100px"}
               :on-change (fn [ev]
                            (let [new-text (.-value (.-target ev))]
                              (reset! text new-text)))}]
   (try
     (let [edn (edn/read-string @text)]
       [:div
        [:pre (pr-str edn)]
        [:input {:type "button"
                 :value "transact!"
                 :on-click (fn [ev]
                             (try
                               (reset! tx-result {:tx-success (d/transact! conn edn)})
                               (catch js/Error e
                                 (reset! tx-result {:tx-error e}))))}]])
     (catch js/Error e
       [:div (str "error" e)]))
   (when-let [ex (:tx-error @tx-result)]
     [:div "Transaction error!"
      [:pre (str ex)]])
   (when-let [r (:tx-success @tx-result)]
     [:div
      "Transaction success!"
      [:br]
      "tx-data:"
      (datoms-table-eavt* (:tx-data r))
      [:br]
      "tempids"
      [:pre (with-out-str (pprint/pprint (:tempids r)))]])])

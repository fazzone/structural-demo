(ns tx-history
  (:require
   [datascript.core :as d]
   [rum.core :as rum]))


(def log2size 3)
(def history-index (atom 0))
(def history-size (bit-shift-left 1 log2size))
(def history-buffer (js/Array. history-size))

(defn clear!
  []
  (reset! history-index 0)
  (.fill history-buffer nil))

(defn next-index
  [i]
  (bit-and (inc i) (dec history-size)))

(defn prev-index
  [i]
  (if (= 0 i)
    (dec history-size)
    (dec i)))

(defn save-tx-report!
  [e]
  (let [n (next-index @history-index)]
    (aset history-buffer n e)
    (reset! history-index n)))

(defn backwards-index-seq
  []
  (->> (iterate prev-index @history-index)
       (take history-size)))

(defn backwards-time-seq
  []
  (->> (iterate prev-index @history-index)
       (take history-size)
       (map (fn [i]
              (aget history-buffer i)))
       (take-while some?)))



(rum/defcs history-view < (rum/local {} ::toggle) rum/reactive
  [{::keys [toggle]} the-conn]
  (let [i (rum/react history-index)]
    [:div
     [:a {:href "#"
          :on-click #(do (.preventDefault %)
                         (println "Swtf" toggle)
                         (swap! toggle update ::self not))}
      (if (get @toggle ::self)
        "Hide history"
        "Show history")]
     (when (get @toggle ::self)
       [:ul
        (for [e (backwards-index-seq)]
          (when-let [{:keys [tempids tx-data tx-meta] :as tx-report} (aget history-buffer e)]
            (let [t (:db/current-tx tempids)]
              [:li {:key (str e " " t)}
               [:a {:href "#"
                      :on-click #(do (.preventDefault %)
                                     (swap! toggle update t not))}
                (str e " " t)]
               " "
               [:code (pr-str (:mutation tx-meta))]
               (when (get @toggle t)
                 [:div
                  "input"
                  [:pre (with-out-str
                          (cljs.pprint/pprint (:input-tx-data tx-meta)))]
                  "transacted"
                  [:pre (with-out-str
                          (doseq [[e a v t a?] tx-data]
                            (println "[" e a (pr-str v) #_t a? "]")))]
                  [:button
                   {:on-click (fn [ev]
                                (d/transact! the-conn
                                             (for [[e a v t a?] (reverse tx-data)]
                                               [(if a? :db/retract :db/add) e a v])
                                             {:mutation [::revert (:db/current-tx tempids)]}))}
                   "Revert"]])])))])]))




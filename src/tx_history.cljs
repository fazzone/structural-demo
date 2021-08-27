(ns tx-history
  (:require
   [datascript.core :as d]
   [rum.core :as rum]))


(def log2size 10)
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
  (let [i (swap! history-index next-index)]
    (aset history-buffer i e)))

(defn backwards-index-seq
  []
  (->> (iterate prev-index (prev-index @history-index))
       (take history-size)))

(defn backwards-time-seq
  []
  (->> (iterate prev-index @history-index)
       (take history-size)
       (map (fn [i]
              (aget history-buffer i)))
       (take-while some?)))


(defn toggler
  [t lbl]
  (str lbl " " (if t "Hide" "Show")))

(rum/defcs history-view < (rum/local {} ::toggle) rum/reactive
  [{::keys [toggle]}]
  [:div
   [:a {:href "#"
        :on-click #(do (.preventDefault %)
                       (swap! toggle update ::self not))}
    "History"]
   (when  (get @toggle ::self)
     (let [i (rum/react history-index)]
       [:div [:code (str "Index " i)]
        [:ul
         (for [e (backwards-index-seq)]
           (when-let [{:keys [tempids tx-data] :as k} (aget history-buffer e)]
             (let [t (:db/current-tx tempids)]
               [:li {:key t}
                [:a {:href "#"
                     :on-click #(do (.preventDefault %)
                                    (swap! toggle update t not))}
                 (str t)]
                (when-not (get @toggle t)
                  [:pre (with-out-str
                          (doseq [[e a v t a?] tx-data]
                            (println "[" e a (pr-str v) #_t a? "]")))]
                  )])))]]))])



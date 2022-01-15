(ns comp.search
  (:require [datascript.core :as d]
            [rum.core :as rum]
            [comp.code :as code]))

(defn search*
  [db sa text]
  ;; U+10FFFF
  (d/index-range db sa text (str text "􏿿")))

(rum/defc results
  [db sa text]
  (when (pos? (count text))
    [:div.search
     {}
     (let [results (->> (search* db sa text)
                        (group-by #(nth % 2))
                        (sort-by (comp - count second))
                        (take 5))]
       (for [[v [[e] :as ds]] results]
         (rum/fragment
          (let [{:token/keys [type value]} (d/entity db e)
                tt (code/token-text type value)]
            [:span.tk {:key e :class (code/token-class type value)} ^String tt])
          ;; 2n+1 and 2n-1
          ;; -(2n+1) = -2n-1 = (- 0 1 (* 2 n))
          ;; -(2n-1) = -2n+1 = (- 1 (* 2 n))
          [:span {:key (- 0 1 (+ e e))} "x" (count ds)]
          [:span {:key (- 1 (+ e e))} (pr-str (take 5 (map first ds)))])))]))

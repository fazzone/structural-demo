(ns sted.comp.search
  (:require [datascript.core :as d]
            [rum.core :as rum]
            [sted.comp.common :as cc]
            [sted.core :as core]
            #_[comp.code :as code]))

(rum.core/set-warn-on-interpretation! true)

(defn search*
  [db sa text]
  ;; U+10FFFF
  (d/index-range db sa text (str text "􏿿")))

(rum/defc results
  [db bus sa text rec]
  (when #_(< 1 (count text))
        (< 0 (count text))
        [:div.search
         {}
         (let [tag (str  "Searching " text)
               _ (js/console.time tag)
               results (->> (search* db sa text)
                            (group-by #(nth % 2))
                            (sort-by (comp - count second))
                            (take 8))
               _ (js/console.timeEnd tag)]
           
           (for [[v [[e] :as ds]] results]
             (let [ent (d/entity db e)
                   k (* 3 e)]
               (rum/fragment
                {:key (- k)}
                ;; ^:inline (rec ent core/blackhole 0 nil)
                (rum/bind-context [cc/*indenter* nil]
                                  ^:inline (rec ent core/blackhole 0 nil))
                [:span {:key (- 1 k)} "x" (count ds)]
                [:span.last {:key (- 2 k)}
                 (pr-str (take 5 (map first ds)))]))))]))

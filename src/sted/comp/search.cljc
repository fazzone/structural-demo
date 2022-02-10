(ns sted.comp.search
  (:require [datascript.core :as d]
            [rum.core :as rum]
            [clojure.string :as string]
            [sted.comp.common :as cc]
            [sted.core :as core]
            [sted.sys.search.dom :as s]
            #_[comp.code :as code]))

(rum.core/set-warn-on-interpretation! true)

(defn search*
  [db sa text]
  ;; U+10FFFF
  (d/index-range db sa text (str text "􏿿")))

(rum/defc hlp
  [tkl off len utext? utk?]
  (let [left (- tkl)]
    [:span {:style {:position :relative}}
     (cond
       (not (or utext? utk?))
       [:span.hlp {:style {:width (str len "ch")
                           :left (str (+ off left) "ch")}}]
       utk?
       [:span.hlp.unique-token {:style {:width (str tkl "ch")
                                        :left (str left "ch")}}]
       :else
       [:span.hlp.unique-text {:style {:width (str tkl "ch")
                                       :left (str left "ch")}}])
     
     #_(if-not unique?
         [:span.hlp
          {:style {:width (str len "ch")
                   :left (str (+ off left) "ch")}}]
         [:span.hlp.unique-token
          {:style {:width (str tkl "ch")
                   :left (str left "ch")}}])]))

(rum/defc results
  [db bus sa text rec]
  [:div
   (when (< 1 (count text))
     (let [tag           (str  "Searching " text)
           _             (js/console.time tag)
           results       (s/substring-search-all-visible-tokens (string/lower-case text))
           _             (js/console.timeEnd tag)
           unique-text?  (= 1 (count results))
           unique-token? (and unique-text? (= 1 (count (val (first results)))))]
       (for [[matched rs] results
             [i n]        rs]
         (do
           #_(js/console.log matched i n)
           (->> n
                (rum/portal (hlp (count matched) i (count text)
                                 unique-text?
                                 unique-token?)))))))])

(rum/defc zzresults
  [db bus sa text rec]
  (when #_(< 1 (count text))
        (< 0 (count text))
        [:ul
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

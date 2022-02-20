(ns sted.comp.search
  (:require [datascript.core :as d]
            [rum.core :as rum]
            [clojure.string :as string]
            [sted.cmd.move :as move]
            [sted.comp.common :as cc]
            [sted.sys.search.db :as sdb]
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
        [:span
         {:id (str "sr" sj)
          :style {:position :absolute
                  :width "1ch"
                  :top "-1.25ex"
                  :color "#fff"
                  ;; :background-color "tomato"
                  :z-index 999
                  :left (str (+ off left) "ch")}}
         sj
         #_(subs sj 0 1)]
        [:span.hlp {:style {:width (str len "ch")
                            :left (str (+ off left)
                                       "ch")}}])
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
                   :left (str left "ch")}}])]
    ))

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

(rum/defc results
  < rum/reactive
  [db bus sa text rec]
  [:div
   {}
   (when (< 1 (count text))
     #_(let [results       (rum/react s/results)
             unique-text?  (= 1 (count results))
             unique-token? (and unique-text? (= 1 (count (val (first results)))))
             jj            (volatile! 0)]
         (for [[matched rs] results
               [i n]        rs]
           (->> n
                (rum/portal (hlp (count matched) i (count text)
                                 (vswap! jj inc)
                                 unique-text?
                                 unique-token?)))))
     #_(dbsr db text rec)
     [:div.search
      (for [eid (rum/react s/results)]
        (let [e (d/entity db eid)
              k (* 3 eid)]
          (rum/fragment
           {:key (- k)}
           (rum/bind-context [cc/*indenter* nil] ^:inline
                             (rec (or #_(move/up e)
                                      e)
                                  core/blackhole
                                  0
                                  nil))
           [:span {:key (- 1 k)} "x" ]
           [:span.last {:key (- 2 k)}
            (str eid)])))]     
     )])


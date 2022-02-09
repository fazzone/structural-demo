(ns sted.comp.search
  (:require [datascript.core :as d]
            [rum.core :as rum]
            [clojure.string :as string]
            [sted.comp.common :as cc]
            [sted.core :as core]
            #_[comp.code :as code]))

(rum.core/set-warn-on-interpretation! true)

(defn search*
  [db sa text]
  ;; U+10FFFF
  (d/index-range db sa text (str text "􏿿")))

(defn thinger [prefix]
  (let [w js/window.innerWidth
        h js/window.innerHeight]
   (letfn [(find-toplevel* [[p & ps]]
             (if (.contains (.-classList p) "top-level-form")
               p
               (recur ps)))
           (find-toplevel [efp] 
             (find-toplevel* (seq efp)))
           (bounds [node]
             (-> (.getClientRects node)
                 (aget 0)))
           (vis-above? [n cr] (when (<= 0 (.-y cr)) n)) 
           (vis-left?  [n cr] (when (<= 0 (.-x cr)) n))
           (vis-below? [n cr] (when (> h (+ (.-y cr) (.-height cr))) n))
           (vis-right? [n cr] (when (> w (+ (.-x cr) (.-width  cr))) n))
           (scan-up    [n] (some-> n (vis-above? (bounds n)) (.-previousSibling)))
           (scan-left  [n] (some-> n (vis-left?  (bounds n)) (.-previousSibling)))
           (scan-down  [n] (some-> n (vis-below? (bounds n)) (.-nextSibling)))
           (scan-right [n] (some-> n (vis-right? (bounds n)) (.-nextSibling)))
           (search-within-toplevel [acc tl]
             (->> (.querySelectorAll tl ".tk")
                  (reduce (fn [a ch]
                            (let [t (.-innerText ch)
                                  i (string/index-of (string/lower-case t) prefix)]
                              (cond-> a i (update t (fnil conj []) [i ch]))))
                          acc)))
           (search-within-chain [acc chain]
             (let [b (bounds chain)
                   midpt (+ (.-x b) (/ (.-width b) 2))
                   tl (-> (js/document.elementFromPoint midpt (/ h 2))
                          (.closest ".form-card"))]
               (reduce search-within-toplevel acc
                       (concat (take-while some? (iterate scan-up tl))
                               (take-while some? (next (iterate scan-down tl)))))))]
     (let [cch (-> (js/document.elementFromPoint (/ w 2) (/ h 2))
                   (.closest ".chain"))]
       (reduce search-within-chain {}
               (concat (take-while some? (iterate scan-left cch))
                       (take-while some? (next (iterate scan-right cch)))))))))

(rum/defc hlp
  [tkl off len]
  (let [left (- tkl)]
    [:span {:style {:position :relative}}
     [:span.hlp {:style {:width (str len "ch")
                         :left (str (+ off left) "ch")}}]]))

(rum/defc results
  [db bus sa text rec]
  [:div
   #_(when (< 0 (count text))
     (let [tag (str  "Searching " text)
           _ (js/console.time tag)
           results (thinger (string/lower-case text))
           _ (js/console.timeEnd tag)]
       
       (for [[matched rs] results
             [i n] rs]
         (do
           #_(js/console.log matched i n)
           (->> n
                (rum/portal (hlp (count matched) i (count text))))))))])

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

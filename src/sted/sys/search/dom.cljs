(ns sted.sys.search.dom
  (:require [clojure.string :as string]))


(def results (atom nil))


(defn substring-search-all-visible-tokens
  ;; (also search invisible tokens if they are under a visible toplevel)
  [bar-el prefix]
  (let [bar-bcr (.getBoundingClientRect bar-el)
        w (.-width bar-bcr)
        h (.-height bar-bcr)
        center-chain (or (some-> (js/document.elementFromPoint (+ (.-x bar-bcr) (/ w 2))
                                                               (+ (.-y bar-bcr) (/ h 2)))
                                 (.closest ".chain"))
                         #_(some-> (js/document.elementFromPoint 30 30)
                                   (.closest ".chain")))]
    
    (if-not center-chain
      (println "No center chain???????????")
      (letfn [(bounds [node]
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
                               (let [t (loop [c (.-firstChild ch)]
                                         (cond (nil? c) " "
                                               (instance? js/Text c) (.-textContent c)
                                               :else (recur (.-nextSibling c))))
                                     i (string/index-of (string/lower-case t) prefix)]
                                 #_(js/console.log "F.c." (instance? js/Text (.-firstChild ch)))
                                 
                                 (cond-> a i (conj #js [t i ch]))
                                 #_(cond-> a i (update t (fnil conj []) [i ch]))))
                             acc)))
              (bidi [start fwd back]
                (concat (take-while some? (iterate fwd start))
                        (take-while some? (next (iterate back start)))))
              (search-within-chain [acc chain]
                (let [b (bounds chain)
                      tl (some-> (js/document.elementFromPoint (+ 30 (.-x b))
                                                               (+ (.-y b) (/ h 2)))
                                 (.closest ".form-card"))]
                  (if tl
                    (->> (bidi tl scan-up scan-down)
                         (reduce search-within-toplevel acc))
                    ;; have to start from the beginning
                    (do
                      #_(js/console.log "Start from top" chain)
                      #_(println "Height is" (.-scrollHeight chain)
                                 "Offset is" (.-scrollTop chain))
                      (->> (.-children chain)
                           (some (fn [el] (vis-above? el b)))
                           (iterate scan-down)
                           (take-while some?)
                           (reduce search-within-toplevel acc))))))]
        (let [tag (str  "Searching " prefix)
              _ (js/console.time tag)
              nr (->> (bidi center-chain scan-left scan-right)
                      #_(map (fn [ch]
                               (js/console.log "Bid" ch)
                               ch))
                      #_(reduce search-within-chain {})
                      (reduce search-within-chain []))
              a (reset! results {:text prefix :results nr})
              _ (js/console.timeEnd tag)]
          #_(doseq [[t i e] nr]
              (js/console.log t i e))
          a)))))

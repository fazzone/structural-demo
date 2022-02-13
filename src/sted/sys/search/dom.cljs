(ns sted.sys.search.dom
  (:require [clojure.string :as string]))


(def results (atom nil))
(defn substring-search-all-visible-tokens
  ;; (also search invisible tokens if they are under a visible toplevel)
  [prefix]
  (println "SSAVT" prefix)
  (let [w js/window.innerWidth
        h js/window.innerHeight
        center-chain (some-> (js/document.elementFromPoint (/ w 2) (/ h 2))
                             (.closest ".chain"))]
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
                #_(js/console.log "SWTL" tl)
                (->> (.querySelectorAll tl ".tk")
                     (reduce (fn [a ch]
                               (let [t (.-innerText ch)
                                     i (string/index-of (string/lower-case t) prefix)]
                                 (cond-> a i (update t (fnil conj []) [i ch]))))
                             acc)))
              (bidi [start fwd back]
                (concat (take-while some? (iterate fwd start))
                        (take-while some? (next (iterate back start)))))
              (search-within-chain [acc chain]
                (let [b (bounds chain)
                      tl (some-> (js/document.elementFromPoint (+ 30 (.-x b)) (/ h 2))
                                 (.closest ".form-card"))]
                  (if tl
                    (->> (bidi tl scan-up scan-down)
                         (reduce search-within-toplevel acc))
                    ;; have to start from the beginning
                    (do
                      (js/console.log "Start from top" chain)
                      (println "Height is" (.-scrollHeight chain)
                               "Offset is" (.-scrollTop chain))
                      (->> (.-children chain)
                           (some (fn [el] (vis-above? el b)))
                           (iterate scan-down)
                           (take-while some?)
                           (reduce search-within-toplevel acc))))))]
        (reset! results
                (->> (bidi center-chain scan-left scan-right)
                     #_(map (fn [ch]
                              (js/console.log "Bid" ch)
                              ch))
                     (reduce search-within-chain {})))))))

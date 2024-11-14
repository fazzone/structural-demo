(ns sted.sys.search.dom
  (:require [clojure.string :as string]
            ["flexsearch" :as fs]))


(defn reduce-all-visible-tokens
  ;; (also search invisible tokens if they are under a visible toplevel)
  [bar-el chain-eid f init]
  #_(js/console.log "SSAVT" bar-el chain-eid)
  (let [bar-bcr (.getBoundingClientRect bar-el)
        w (.-width bar-bcr)
        h (.-height bar-bcr)
        center-chain (or
                      (when chain-eid
                        (.querySelector bar-el (str ":scope > [data-eid=\"" chain-eid "\"]")))
                      (some-> (js/document.elementFromPoint (+ (.-x bar-bcr) (/ w 2))
                                                            (+ (.-y bar-bcr) (/ h 2)))
                              (.closest ".chain"))
                      #_(some-> (js/document.elementFromPoint 30 30)
                                (.closest ".chain")))]
    
    (if-not center-chain
      (do
        (println "No center chain???????????")
        (println "No center chain???????????")
        (println "No center chain???????????")
        (println "EFP" (+ (.-x bar-bcr) (/ w 2))
                 (+ (.-y bar-bcr) (/ h 2)))
        (js/document.elementFromPoint (+ (.-x bar-bcr) (/ w 2))
                                      (+ (.-y bar-bcr) (/ h 2))))
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
              (reduce-toplevel* [acc tl]
                (->> (.querySelectorAll tl ".tk")
                     (reduce (fn [a ch]
                               (let [t (loop [c (.-firstChild ch)]
                                         (cond (nil? c) nil
                                               (instance? js/Text c) (.-textContent c)
                                               :else (recur (.-nextSibling c))))]
                                 ;; reduce
                                 (cond-> a t (f t ch))
                                 #_(if-not t
                                     a
                                     (f a t
                                        (some-> ^js ch
                                                (..  -dataset -eid)
                                                (js/parseInt))))))
                             acc)))
              (bidi [start fwd back]
                (concat (take-while some? (iterate fwd start))
                        (take-while some? (next (iterate back start)))))
              (reduce-chain* [acc chain]
                (let [b (bounds chain)
                      tl (some-> (js/document.elementFromPoint (+ 30 (.-x b))
                                                               (+ (.-y b) (/ h 2)))
                                 (.closest ".form-card"))]
                  (if tl
                    (->> (bidi tl scan-up scan-down)
                         (reduce reduce-toplevel* acc))
                    ;; have to start from the beginning
                    (->> (.-children chain)
                         (some (fn [el] (vis-above? el b)))
                         (iterate scan-down)
                         (take-while some?)
                         (reduce reduce-toplevel* acc)))))]
        
        (->> (bidi center-chain scan-left scan-right)
             (reduce reduce-chain* init))))))

(defn index-visible-tokens!
  [bar-el chain-eid]
  (js/console.time "index-visible-tokens!")
  (let [els #js []
        ts #js []
        index (reduce-all-visible-tokens
               bar-el
               chain-eid
               (fn [a t el]
                 (.add a (count els) t)
                 (.push els el)
                 (.push ts t)
                 a)
               (fs/Index. #js {:tokenize "full"}))
        c (volatile! 0)]
    (js/console.timeEnd "index-visible-tokens!")
    (println "Indexed " (count els) "visible tokens")
    
    (fn searcher [prefix]
      (for [i (.search index prefix)
            :let [^js el (nth els i)
                  token (nth ts i)
                  lower (string/lower-case token)
                  eid (some-> (..  el -dataset -eid)
                              (js/parseInt))]]
        #js {:token token
             :element el
             :eid eid
             :matches 
             (->> (string/split (string/lower-case prefix) #"\s+")
                  (mapcat
                   (fn [s]
                     (loop [i 0
                            a #js []]
                       (let [j (string/index-of lower s i)]
                         (if (nil? j)
                           a
                           (recur (inc j)
                                  (doto a (.push #js {:key (vswap! c inc)
                                                      :offset j
                                                      :length (count s) })))))))))}))))

(defn substring-search-all-visible-tokens
  [bar-el chain-eid prefix]
  (js/console.time "Indexer")
  (let [els #js []
        ts #js []
        index (reduce-all-visible-tokens
               bar-el
               chain-eid
               (fn [a t el]
                 (.add a (count els) t)
                 (.push els el)
                 (.push ts t)
                 a)
               (fs/Index. #js {:tokenize "full"}))]
    (println "Indexed " (count els) "visible token elements")
    (js/console.timeEnd "Indexer")
    (let [ans (into-array
               (for [i (.search index prefix)
                     :let [^js el (nth els i)
                           eid (some-> (..  el -dataset -eid)
                                       (js/parseInt))]]
                 #js {:match (nth ts i)
                      :index 0
                      :element el
                      :eid eid}))]
    
    
      ans)))

(defn numeric
  [bar-el sel-eid]
  (let [sel-el (.querySelector bar-el ".selected")

        rs (->> (.querySelectorAll sel-el ".c")
                (reduce (fn [a ch]
                          (conj a #js {:match "?"
                                       :index 0
                                       :element (.querySelector ch ".d")
                                       :eid (some-> ^js ch
                                                    (..  -dataset -eid)
                                                    (js/parseInt))}))
                        []))]
    (js/console.table (clj->js rs))
    rs
    ))

(ns comp.cons
  (:require
   [db-reactive :as dbrx]
   [clojure.set :as set]
   [embed  :as e ]
   [rum.core :as rum]))

(declare onecell)


#_(defn conscell-layout-bfs
  [top]
  (loop [[e & front] [top]
         out         []]
    (prn "E" (:db/id e) "Front" front "Out" (mapv :db/id out))
    (cond
      (nil? e)                    out
      :else
      (recur
       (cond-> (or front [])
         (:seq/next e) (conj (:seq/next e))
         (:seq/first e) (conj (:seq/first e))
         )
       (conj out e))
      )))


#_(defn conscell-layout-bfs
  [top]
  (loop [front [top]
         out         []]
    (prn (map :db/id out) "---" (map :db/id front))
    (if (empty? front)
      out
      (let [e (peek front)
            car (:seq/first e)
            cdr (:seq/next e)]
        (recur
           (cond-> (pop front)
             car (conj car)
             cdr (conj cdr))

           (conj out e))))))

#_(defn conscell-layout-bfs
  [top]
  (loop [i 0
         out []
         nextfront [top]
         firstfront []]
    (println (map :db/id out) "-" (map :db/id nextfront) "-" (map :db/id firstfront))
    (cond
      (< 99 i)
      out
      
      (seq nextfront)
      (let [e (peek nextfront)]
        #_(println "Next " (:db/id e))
        (recur  (inc i) (conj out e)
                (cond-> (pop nextfront) (:seq/next e) (conj (:seq/next e)))
                (cond-> firstfront (:seq/first e) (conj (:seq/first e)))))
      
      (seq firstfront)
      (let [e (first firstfront)]
        #_(println "First " (:db/id e))
        (recur  (inc i) (into out [{:db/id :FF} e])
                (cond-> nextfront (:seq/next e) (conj (:seq/next e)))
                (cond-> (subvec firstfront 1) (:seq/first e) (conj (:seq/first e)))))
      
      :else out)))

(defn conscell-layout-bfs
  [top]
  #_(loop [npc 0
         fpc 0
         out []
         nextfront [[top 0]]
         firstfront []]
    #_(println npc fpc (map :db/id out) "-" (map :db/id nextfront) "-" (map :db/id firstfront))
    (cond
      (seq nextfront)
      (let [e (peek nextfront)]
        (println "next" (:db/id e) fpc (inc npc))
        (recur  (inc npc)
                fpc
                (conj out e)
                (cond-> (pop nextfront) (:seq/next e) (conj (:seq/next e)))
                (cond-> firstfront (:seq/first e) (conj [npc (:seq/first e)]))))
      
      (seq firstfront)
      (let [[prev-npc e] (peek firstfront)]
        (println "first" (:db/id e)  (inc fpc) prev-npc) 
        (recur prev-npc
               (inc fpc)
               (into out [{:db/id :FF} e])
               (cond-> nextfront (:seq/next e) (conj (:seq/next e)))
               (cond-> (pop firstfront) (:seq/first e) (conj [prev-npc (:seq/first e)]))))
      
      :else out)))


(defn compute-thing
  [top]
  (loop [front [top]
         id->length {}
         id->depth {}]
    (if (empty? front)
      (do
        
        (doseq [i (sort (vec (set (concat (keys id->length)
                                          (keys id->depth)))))]
          (prn i (id->length i) (id->depth i)))
        [id->length id->depth])
      (let [e (peek front)
            id (:db/id e)
            rightwards (when-not (id->length id)
                         (take-while some? (iterate :seq/next e)))
            downwards (when-not (id->depth id)
                        (take-while some? (iterate :seq/first e)))]
        (recur
         (cond-> (pop front)
           rightwards (into rightwards)
           downwards (into downwards))
         (cond-> id->length rightwards
                 (into
                  (map vector
                       (map :db/id rightwards)
                       (iterate dec (count rightwards)))))
         (cond-> id->depth downwards
                 (into
                  (map vector
                       (map :db/id downwards)
                       (iterate dec (count downwards))))))))))



(defn compute-conscell-extents
  [root]
  (let [rmax (volatile! 0)
        cmax (volatile! 0)
        go (fn go [node row col]
             (vswap! rmax max row)
             (vswap! cmax max col)
             (some-> (:seq/next node)
                     (go row (inc col)))
             (some-> (:seq/first node)
                     (go (+ row
                            1
                            #_(if (:seq/first node)
                              2
                              1))
                         col)))]
    (go root 0 0)
    [@rmax @cmax]))

(defn compute-conscell-positions
  [root start-row start-col]
  (let [acc (volatile! #{})
        go (fn go [node row col]
             (vswap! acc conj [row col])
             (some-> (:seq/next node)
                     (go row (inc col)))
             (some-> (:seq/first node)
                     (go (inc row)
                         col)))]
    (go root start-row start-col)
    @acc))



#_(defn conscell-collides?
  [node proposed-row proposed-col]
  (let [go (fn go [subnode row col]
             (if (and (= row proposed-row)
                    (= col proposed-col)
                    (not (= (:db/id node) (:db/id subnode))))
               nil
               (or (some-> (:seq/next node)
                           (go row (inc col)))
                   (some-> (:seq/first node)
                           (go (inc row) col)))))]
    (go node proposed-row proposed-col)))

(defn conscell-collides?
  [root row col check-row check-col]
  (println "Check collision" (:db/id root) "@" check-row check-col )
  (let [go (fn go [node row col]
             
             (println "Check" (:db/id node) "@" row col)
             (if (and (= row check-row)
                      (= col check-col))
               
               [(:db/id node) row col]
               
               
               (or (some-> (:seq/next node)
                           (go row (inc col)))
                   (some-> (:seq/first node)
                           (go (inc row) col)))))]
    (go root row col)))

(defn onecell*
  [node size row col taken]
  (let [half   (* 0.5 size)
        double (* 2 size)
        width  (* 6 half)
        height double
        x      (* width col)
        y      (* height row)
        label  (str "#" (:db/id node) "-" (count taken))]
    #_(println "Onecell" (:db/id ))
    (rum/fragment
     (if-let [sv (or (:symbol/value node)
                     (:keyword/value node)
                     (:number/value node))]
       (rum/fragment
        [:text {:x x :y y} (str sv)]
        [:text {:x x :y (+ y half)} label])
       (rum/fragment
        [:text {:x (+ x (* 3 half)) :y (- y 4)} label]
        (when-let [ct (:coll/type node)]
         [:text {:x x :y (- y 4)} (pr-str ct)])
        [:rect {:x x :y y :fill :none :width size :height size}]
        [:rect {:x (+ x size) :y y :fill :none :width size :height size}]))
     
     (when-let [cdr (:seq/next node)]
       (let [arrowleft  (+ x (* 3 half))
             [rmax-below cmax-after] (compute-conscell-extents (:seq/next node))
             splay (inc col)
             arrowright (+ (* 0.2 size) (* width splay))
             control    (+ arrowleft (/ (- arrowright arrowleft) 2))]
         (rum/fragment
          (onecell cdr size row splay (assoc taken [row col] (:db/id node)))
          [:path {:marker-end "url(#head)" :fill :none :stroke "#fff"
                  :d          (str "M" (str arrowleft) "," (str (+ y half))
                                   " Q" (str control) "," (str y)
                                   " " (str arrowright) "," (str (+ y half)))}])))
     (when-let [car (:seq/first node)]
       (let [droop (+ row 1
                      (case (:db/id car)
                        36 1
                        43 2
                        0))]
         (rum/fragment
          (onecell car size droop col (assoc taken [row col] (:db/id node)))
          
          [:path {:marker-end "url(#head)" 
                  :d          (str "M" (str (+ x half)) "," (str (+ y half))
                                   " V" (str (- (* height droop) half)))}]))))))

(rum/defc onecell < dbrx/ereactive
  [node size row col taken]
  (if-not (:form/highlight node)
    (onecell* node size row col taken)
    [:g {:stroke "#ae81ff" :fill "#ae81ff"} (onecell* node size row col taken)]))

(rum/defcs svg-viewbox < 
  {:init (fn [state props]
           (assoc state
                  :computed-size (some-> state :rum/args first (compute-conscell-extents))
                  :taken-pos {}))}
  [{:keys [computed-size taken-pos]} root]
  (let [size 50
        [rmax cmax] computed-size
        width (* 3.5 size (+ 2 cmax))
        height (* 5 size (inc rmax))]
    #_(prn "Computer size" computed-size)
    #_(conscell-layout-bfs root)
    #_(compute-thing root)
    [:svg  {:viewBox (str "0 0 " width " " height)
            :style {:border "1px solid"
                    :background "transparent"}}
     [:defs
      [:marker {:id "head" :orient "auto" :markerWidth 20 :markerHeight 40 :refX 0.1 :refY 3}
       [:path {:fill "#fff" :d "M0,0 V6 L6,3 Z"}]]]
     [:g {:stroke "#fff"
          :transform (str "translate(" size "," size ")")
          :fill "#fff"
          :stroke-width 1}
      
      (onecell root size 0 0 taken-pos)]
     #_(for [i (range 9)]
         [:rect {:key i
                 :stroke "tomato"
                 :fill :none
                 :x 0
                 :y 0
                 :width (* i 3.5 size)
                 :height (* i 4 size)}])]))



(ns sted.comp.cons
  (:require
   [sted.db-reactive :as dbrx]
   [clojure.set :as set]
   [sted.embed  :as e]
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
         (:seq/first e) (conj (:seq/first e)))
       (conj out e)))))

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
  (println "Check collision" (:db/id root) "@" check-row check-col)
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
  [node bus size row col taken]
  (let [half   (* 0.5 size)
        double (* 2 size)
        width  (* 6 half)
        height double
        x      (* width col)
        y      (* height row)
        label  (str "#" (:db/id node) "-" (count taken))]
    #_(println "Oncell* " (:db/id node))
    (rum/fragment
     (if-let [sv (:token/value node)]
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
          (onecell cdr bus size row splay (assoc taken [row col] (:db/id node)))
          #_[:path {:marker-end "url(#head)" :fill :none :stroke "#fff"
                    :d          (str "M" (str arrowleft) "," (str (+ y half))
                                     " Q" (str control) "," (str y)
                                     " " (str arrowright) "," (str (+ y half)))}]
          [:path {:marker-end "url(#head)" :fill :none :stroke "#fff"
                  :d          (str "M" arrowleft "," (+ y half)
                                   "H" arrowright)}])))
     (when-let [car (:seq/first node)]
       (let [droop (+ row 1
                      (or  (:form/indent car) 0)
                      (case (:db/id car)
                        0))]
         (rum/fragment
          (onecell car bus size droop col (assoc taken [row col] (:db/id node)))
          [:path {:marker-end "url(#head)"
                  :d          (str "M" (str (+ x half)) "," (str (+ y half))
                                   " V" (str (- (* height droop) half)))}]))))))

(rum/defc onecell < dbrx/ereactive
  [node bus size row col taken]
  (if-not (:form/highlight node)
    (onecell* node bus size row col taken)
    [:g {:stroke "#ae81ff" :fill "#ae81ff"}
     (onecell* node bus size row col taken)]))

(rum/defcs svg-viewbox <
  {:init (fn [state props]
           (assoc state
                  :computed-size (some-> state :rum/args first (compute-conscell-extents))
                  :taken-pos {}))}
  [{:keys [computed-size taken-pos]} root bus]
  (let [size 45
        [rmax cmax] computed-size
        width (* 3.5 size (inc cmax))
        ;; height (* 2.5 size (inc rmax))
        height (* 2.5 size (inc rmax))]
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
      (onecell root bus size 0 0 taken-pos)]
     #_(for [i (range 9)]
         [:rect {:key i
                 :stroke "tomato"
                 :fill :none
                 :x 0
                 :y 0
                 :width (* i 3.5 size)
                 :height (* i 4 size)}])]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defn dongus [size id coll-type token row col]
  (let [half   (* 0.5 size)
        double (* 2 size)
        width  (* 6 half)
        height double
        x      (* width col)
        y      (* height row)]
    (rum/fragment
     (when coll-type
       [:g
        [:text {:x (+ x (* 3 half)) :y (- y 4)} (str coll-type)]
        [:rect {:x x :y y :fill :none :width size :height size}]
        [:rect {:x (+ x size) :y y :fill :none :width size :height size}]])
     (when token
       [:g [:text {:x x :y y} token]])
     (let [arrowleft  (+ x (* 3 half))
           splay (inc col)
           arrowright (+ (* 0.2 size) (* width splay))
           control    (+ arrowleft (/ (- arrowright arrowleft) 2))]
       [:path {:marker-end "url(#head)" :fill :none :stroke "#fff"
               :d          (str "M" arrowleft "," (+ y half)
                                "H" arrowright)}])
     (let [droop (+ row 1)]
       [:path {:marker-end "url(#head)"
               :d          (str "M" (str (+ x half)) "," (str (+ y half))
                                " V" (str (- (* height droop) half)))}]))))
(rum/defc nonrec
  < #_dbrx/ereactive
  {:key-fn (fn [n b s l r c] (:db/id n))}
  [node bus size eid->pos row col]
  (let [half   (* 0.5 size)
        double (* 2 size)
        width  (* 6 half)
        height double
        x      (* width col)
        y      (* height row)
        label  (str "#" (:db/id node))
        st (if (:form/highlight node)
             "#ae81ff"
             "#fff")]
    [:g {:stroke st}
     (if-let [sv (:token/value node)]
       [:g
        [:text {:x x :y y} (str sv)]
        [:text {:x x :y (+ y half)} label]]
       [:g  
        [:text {:x (+ x (* 3 half)) :y (- y 6)} label]
        [:text {:x x :y (- y 6)}
         (str row "," col)
         (when-let [ct (:coll/type node)]
           (str (e/open-delim ct) (e/close-delim ct)))]
        [:rect {:x x :y y :width (+ size size) :height size}]
        [:line {:x1 (+ x size) :y1 y :x2 (+ x size) :y2 (+ y size)}]])
     (when-let [cdr (:seq/next node)]
       (let [[nr nc]    (eid->pos (:db/id cdr))
             arrowleft  (+ x (* 3 half))
             arrowright (+ (* nc width) (* 0.2 size))]
         
         (when (not= row nr)
           (println "Bad layout - cdr " (:db/id cdr) "should be in row" row "but is in" nr "??"))
         
         #_(when (not= row nr) (throw (ex-info "Bad layout" {})))
         [:path {:marker-end "url(#head)"
                 :d          (str "M" arrowleft "," (+ y half)
                                  "H" arrowright)}]))
     (when-let [car (:seq/first node)]
       (let [[nr nc] (eid->pos (:db/id car))]
         #_(when (not= col nc) (throw (ex-info "Bad layout" {})))
         (when (not= col nc)
           (println "Bad layout - car " (:db/id car) "should be in column" col "but is in" nc "??"))
         [:path {:marker-end "url(#head)"
                 :d          (str "M" (str (+ x half)) "," (str (+ y half))
                                  " V" (str (- (* nr height) half)))}]))]))

#_(defn my-traversal
  [top]
  (loop [n          0
         nextfront  [[0 0 top]]
         firstfront []
         occ        []
         save       []]
    (letfn [(collide? [r c]
              (let [naive (and (< c (count occ))
                               (get (nth occ c) r))]
                naive))
            (claim [r c e]
              (update occ c (fnil assoc (sorted-map)) r e))]
      (cond
        (> n 999)
        (throw (ex-info "Too many" {}))
        
        (seq nextfront)
        (let [[r c e] (peek nextfront)]
          (if (collide? r c)
            (let [[nf ff nocc] (peek save)
                  [sr sc se] (peek ff)]
              (println "NF" (:db/id e) "collides with" (:db/id (get-in occ [c r])) "at" [r c])
              (println "\t" "Re-layout from" (:db/id se) "@" sr sc)
              (println "\t" "Place" (:db/id se) "at"
                       (inc (key (first (rseq (nth occ c)))))
                       ","
                       sc
                       "?")
              (recur (inc n)
                     nf
                     ff
                     nocc
                     (pop save)))
            (recur (inc n)
                   (cond-> (pop nextfront) (:seq/next e) (conj [r (inc c) (:seq/next e)]))
                   (cond-> firstfront      (:seq/first e) (conj [(inc r) c (:seq/first e)]))
                   (claim r c e)
                   save)))
        
        (seq firstfront)
        (let [[r c e] (peek firstfront)]
          (if (collide? r c)
            (let [[nf ff nocc] (peek save)]
              (println "FF" (:db/id e) "collides with" (:db/id (get-in occ [c r])) "at" [r c])
              (recur (inc n) nf ff nocc (pop save)))
            (recur (inc n)
                   (cond-> nextfront        (:seq/next e) (conj [r (inc c) (:seq/next e)]))
                   (cond-> (pop firstfront) (:seq/first e) (conj [(inc r) c (:seq/first e)]))
                   (claim r c e)
                   (conj save [nextfront
                               (conj (pop firstfront) [(inc r) c e])
                               
                               (update occ c (fnil assoc (sorted-map)) r ::fake)]))))
        
        :else
        (do
          (println "Layout explored" n)
          (into {}
                (for [[c col] (map vector (range) occ)
                      [r e]   col
                      :when (not= ::fake e)]
                  [[r c] e])))))))

(defn my-traversal
  [top]
  (loop [n          0
         nextfront  [[0 0 top]]
         firstfront []
         occ        []
         save       []]
    (letfn [(collide? [r c]
              (some-> (get occ c) (subseq >= r) seq))
            (claim [r c e]
              (update occ c (fnil assoc (sorted-map)) r e))]
      (cond
        (> n 999)
        (throw (ex-info "Too many" {}))
        
        (seq nextfront)
        (let [[r c e] (peek nextfront)]
          (if (collide? r c)
            (let [[sc se nf ff nocc] (peek save)
                  nr                 (inc (key (first (rseq (nth occ c)))))]
              (do
                #_ (println (:db/id e) "collides with" (:db/id se) "at" r c)
                #_ (println "\t"  "sc=" sc "nr=" nr ))
              
              (recur (inc n)
                     nf
                     (conj ff [nr sc se])
                     nocc
                     (pop save)))
            (recur (inc n)
                   (cond-> (pop nextfront) (:seq/next e) (conj [r (inc c) (:seq/next e)]))
                   (cond-> firstfront      (:seq/first e) (conj [(inc r) c (:seq/first e)]))
                   (claim r c e)
                   save)))
        
        (seq firstfront)
        (let [[r c e] (peek firstfront)]
          (if (collide? r c)
            (let [[nf ff nocc] (peek save)]
              (println "FF" (:db/id e) "collides with" (:db/id (get-in occ [c r])) "at" [r c])
              (recur (inc n) nf ff nocc (pop save)))
            (recur (inc n)
                   (cond-> nextfront        (:seq/next e) (conj [r (inc c) (:seq/next e)]))
                   (cond-> (pop firstfront) (:seq/first e) (conj [(inc r) c (:seq/first e)]))
                   (claim r c e)
                   (conj save [c e nextfront (pop firstfront) occ]))))
        
        :else
        (do
          #_(println "Layout explored" n)
          (into {}
                (for [[c col] (map vector (range) occ)
                      [r e]   col]
                  [[r c] e])))))))



(rum/defc testing < dbrx/deeply-ereactive
  [top bus]
  (let [size     50
        half     (* 0.5 size)
        width    (* 6 half)
        height   (* 2 size)
        inset    size
        _        (js/console.time "layout")
        pos->ent (my-traversal top)
        eid->pos (reduce-kv
                  (fn [a p e] (assoc a (:db/id e) p))
                  {}
                  pos->ent)
        rmax     (apply max (map first (keys pos->ent)))
        cmax     (apply max (map second (keys pos->ent)))
        _        (js/console.timeEnd "layout")]
    [:svg  {:viewBox (str "0 0 "
                          (+ inset (* (inc cmax) width))
                          " "
                          (+ inset (* (inc rmax) height)))
            :style   {:border     "1px solid"
                      :background "transparent"}}
     [:defs
      [:marker {:id "head" :orient "auto" :markerWidth 20 :markerHeight 40 :refX 0.1 :refY 3}
       [:path {:fill "#fff"
               :d    "M0,0 V6 L6,3 Z"}]]]
     [:g { ;; :stroke "#fff"
          :transform    (str "translate(" size "," size ")")
          ;; :fill "#fff"
          :stroke       :none
          :fill         :none
          :stroke-width 1}
      (for [[[r c] e] pos->ent]
        (nonrec e bus size eid->pos r c))]]))



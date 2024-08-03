(ns sted.comp.scroll
  (:require [sted.comp.common :as cc]
            [rum.core :as rum]))


(def ^:const scroll-hysteresis-px 3)


(defn do-hysteresis
  [pos npos]
  (when-not (< (- scroll-hysteresis-px)
               (- pos npos)
               (+ scroll-hysteresis-px))
    npos))


(defn scroll-1d
  [outer-size inner-size pos off]
  (let [align-start off
        align-end (- off (- outer-size inner-size))]
    (do-hysteresis
     pos
     (if (< (js/Math.abs (- pos align-start))
            (js/Math.abs (- pos align-end)))
       align-start
       align-end))))

(defn svgdoc
  [total-size asize apos bsize bpos ]
  (let [h 100
        svgstr (str "<svg xmlns=\"http://www.w3.org/2000/svg\" "
                    "  height=\"" h "\" viewBox=\"" 0 " " 0 " " total-size " " h "\">"
                    #_(* 0.5 h)
                    
                    (str "\n<rect fill=\"" "red"
                         "\" x=\"" 0
                         "\" y=\"" 0
                         "\" width=\"" total-size
                         "\" height=\"" h
                         "\"/>")

                    (str "\n<rect fill=\"" "blue"
                         "\" x=\"" apos
                         "\" y=\"" 0
                         "\" width=\"" asize
                         "\" height=\"" (* 0.5 h)
                         "\"/>")

                    (str "\n<rect fill=\"" "green"
                         "\" x=\"" bpos
                         "\" y=\"" (* 0.5 h)
                         "\" width=\"" bsize
                         "\" height=\"" (* 0.5 h)
                         "\"/>")
                    
                    "</svg>")]
    
    (js/console.log "%cX"
                    (str "padding-top: " h "px"
                         "; padding-left: " 90  "%"
                         "; background:url("
                         #_(.-src img)
                         (str "data:image/svg+xml;charset=utf-8,"
                              (js/encodeURIComponent svgstr))
                         ") no-repeat; background-size:contain;"))
    
    
    )
  
  )


(defn visible?
  [outer-size inner-size pos off]
  (<= 0 (- off pos) (- outer-size inner-size)))


(defn scroll1d-center
  [total-size asize apos bsize bpos]
  (let [acenter (+ apos (* 0.5 asize))
        bcenter (+ bpos (* 0.5 bsize))]
    (+ apos (- bcenter acenter))))

(defn scroll-to-selected*
  [el el-cr]
  (let [tl            (some-> el (.closest ".form-card"))
        tl-cr         (some-> tl (.getBoundingClientRect))
        chain         (some-> (or tl el) (.closest ".chain"))
        chain-cr      (some-> chain (.getBoundingClientRect))
        chh           (some-> chain-cr (.-height))
        chw           (some-> chain-cr (.-width))
        tlh           (some-> tl-cr (.-height) (js/Math.ceil))
        elh           (some-> el-cr (.-height) (js/Math.ceil))
        ;; fit entire toplevel if possible, otherwise just selection
        vst           (if (< tlh chh) tl el)
        h             (if (< tlh chh) tlh elh)
        vpos          (some-> chain (.-scrollTop))
        voff          (some-> vst (.-offsetTop))
        bar           (some-> chain (.closest ".bar"))
        bar-cr        (some-> bar (.getBoundingClientRect))
        ;; barw     (some-> bar-cr (.-width))
        barw          (some-> bar (.-scrollWidth))
        w             (some-> chain-cr (.-width) (js/Math.ceil))
        hpos          (some-> bar (.-scrollLeft))
        hoff          (some-> chain (.-offsetLeft))
        new-chain-top (scroll1d-center
                       (.-scrollHeight chain)             ; total size
                       chh                                ; visible size
                       (.-scrollTop chain)                ; visible offset
                       (.-height el-cr) 
                       (+ (.-scrollTop chain) (.-y el-cr)))
        
        new-bar-left (scroll1d-center
                      (.-scrollWidth bar)
                      (.-width bar-cr)
                      (.-scrollLeft bar)
                      (.-width el-cr)
                      (+ (.-scrollLeft bar) (.-x el-cr)))]
    
    
    ;; console minimap svg hack
    #_(let [vv         js/window.visualViewport
          all-chains (filter #(.matches % ".chain") (.-children bar))
          s          5
          imgw       (.-scrollWidth bar)
          imgh       (.-height vv)
          
          outsetf 1.2
          
          vboxw imgw
          vboxy (apply min (map #(- (.-scrollTop %)) all-chains))
          vboxh (+ (- vboxy)
                   (apply max (map #(- (.-scrollHeight %) (.-scrollTop %)) all-chains)))
          
          svgstr (str "<svg xmlns=\"http://www.w3.org/2000/svg\"   viewBox=\"" 0 " " vboxy " " vboxw " " vboxh "\">"
                      ;; each chain
                      (apply str
                             (for [c     (.-children bar)
                                   :when (.matches c ".chain")
                                   :let  [inset 0]]
                               (str "\n<rect fill=\""
                                    (if (identical? c chain) "purple" "blue")
                                    "\" " "stroke=\"black\" stroke-width=\"4"
                                    "\" x=\"" (+ (.-offsetLeft c) inset)
                                    "\" y=\"" (- (- (.-scrollTop c)) inset)
                                    "\" width=\"" (- (.-scrollWidth c) inset inset)
                                    "\" height=\"" (- (.-scrollHeight c) inset inset)
                                    "\"/>")))
                      
                      (str "\n<rect fill=\"" "green"
                           "\" x=\"" (+ hpos (.-x el-cr))
                           "\" y=\"" (.-y el-cr)
                           "\" width=\"" (.-width el-cr)
                           "\" height=\""  (.-height el-cr)
                           "\"/>")
                      
                      ;; path with hole to show what is visible
                      (str "\n<path fill=\"" "#000""\" " "fill-opacity=\"80%\" fill-rule=\"evenodd\" d=\""
                           "M" 0 "," vboxy " "
                           "L" vboxw "," vboxy " "
                           "L" vboxw "," (+ vboxh vboxy) " "
                           "L" 0 "," (+ vboxh vboxy) " Z "
                           
                           "M" hpos "," 0 " "
                           "L" (+ hpos (.-width vv)) "," 0 " "
                           "L" (+ hpos (.-width vv)) "," (.-height vv) " "
                           "L" hpos "," (.-height vv) " Z"
                           "\"/>") 
                      
                      "</svg>")]
      
      (js/console.log "%c "
                      (str ;; "font-size:1px; padding:" (/ vboxw s) "px " (/ vboxw s) "px; background:url("
                       "padding-top: " (/ vboxh s 2) "px"
                       "; padding-left: " (/ vboxh s) "px"
                       "; background:url(" (str "data:image/svg+xml;charset=utf-8," (js/encodeURIComponent svgstr))
                       ") no-repeat; background-size:contain;")))
    
    
    (when new-chain-top (.scrollTo chain #js {:top new-chain-top}))
    (when new-bar-left (.scrollTo bar #js {:left new-bar-left}))))

;; if an alias of something is visible on the screen,
;; there is no need to snap the view to the "real copy".
;; if both are on the screen at the same time, there is no way
;; for us to know which you are looking at and consider "real".
;; so, we just pick the one closest to the viewport origin.

(defn primary-element
  [els]
  (let [_ (js/console.log els)
        _ (js/console.log (into-array els))
        sorted (doto (into-array els)
                 (.sort (fn [a b]
                          (let [ra (.getBoundingClientRect a)
                                rb (.getBoundingClientRect b)]
                            (- (+ (.-y rb) (* 0.5 (.-height rb)))
                               (+ (.-y ra) (* 0.5 (.-height ra)))))))
                 (.sort (fn [a b]
                          (let [ra (.getBoundingClientRect a)
                                rb (.getBoundingClientRect b)]
                            (- (+ (.-x rb) (* 0.5 (.-width rb)))
                               (+ (.-x ra) (* 0.5 (.-width ra))))))))]
    (doseq [e els
            :let [r (.getBoundingClientRect e)]]
      (js/console.log "PRIMARY?" (identical? e (first sorted))
                      (+ (.-x r) (* 0.5 (.-width r)))
                      (+ (.-y r) (* 0.5 (.-height r)))
                      e)
      )
    (first sorted)))

(defn scroll-to-selected!
  []
  #_(let [[el & more :as els] (-> (js/document.querySelectorAll ".selected")
                                  (js/Array.from)
                                  (.filter #(nil? (.closest % ".alternate-reality")) ))]
      (when more
        (js/console.log "There should be one real selected element!" els))
      (if-not el
        (js/console.log "There should be one selected element!")
        (scroll-to-selected* el (.getBoundingClientRect el))))
  (println "Scroll to selected!")
  (let [el (primary-element (js/document.querySelectorAll ".selected"))]
    (scroll-to-selected* el (.getBoundingClientRect el))))


(def scroll-window-percents
  [20 0 80 0])


(rum/defc scroll-area-viz
  []
  [:div
   {:style
    {:position :fixed
     :top "20vh"
     :left "0px"
     :width "100vw"
     ;; :height "calc(80vh - 20px)"
     :height "60vh"
     :background-color "#4504b982"}}])

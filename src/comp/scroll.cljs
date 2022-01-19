(ns comp.scroll)

(def ^:const scroll-hysteresis-px 32)

(defn scroll-1d
  [size h pos off]
  (let [align-bottom (- off (- size h))
        top-closer? (< (js/Math.abs (- pos off))
                       (js/Math.abs (- pos align-bottom)))
        [best other] (if (or top-closer? (> h size))
                       [off align-bottom]
                       [align-bottom off])]
    (if-not (< (- scroll-hysteresis-px) (- pos best) scroll-hysteresis-px)
      (int best))))

(defn scroll-to-selected**
  [el always]
  (let [tl    (some-> el (.closest ".form-card"))
        chain (some-> el (.closest ".chain"))
        bar   (some-> chain (.closest ".bar"))
        chain-height (some-> chain (.-clientHeight))
        bar-width    (some-> bar (.-clientWidth))
        ;; fit the entire toplevel if we can, otherwise just the selection
        tlh  (some-> tl (.getBoundingClientRect) (.-height) (js/Math.ceil))
        elh  (some-> el (.getBoundingClientRect) (.-height) (js/Math.ceil))
        vst  (if (< tlh chain-height) tl  el)
        h    (if (< tlh chain-height) tlh elh)
        vpos (some-> chain (.-scrollTop))
        voff (some-> vst (.-offsetTop))
        #_form-visible-within-chain? #_(<)
        new-chain-top (and tl
                           chain
                           #_(< h chain-height)
                           (or always
                               (not (< vpos voff (+ h voff)
                                       (+ vpos chain-height))))
                           (scroll-1d chain-height h vpos voff))
        w    (some-> chain (.getBoundingClientRect) (.-width) (js/Math.ceil))
        hpos (some-> bar (.-scrollLeft))
        hoff (some-> chain (.-offsetLeft))
        new-bar-left (and bar
                          (or always
                              (not (< hpos hoff (+ w hoff)
                                      (+ hpos bar-width))))
                          (scroll-1d bar-width w hpos hoff))]
    #_(js/console.log "Tl" tl "Chain" chain "Bar" bar)
    #_(println "================================Scroll"
               "\nChain-height" chain-height
               "\nBar-width" bar-width
               "\nTLH" h
               "vpos" vpos
               "voff" voff
               "\nCHW" w
               "hpos" hpos
               "hoff" hoff
               "\nCan fit?" (< h chain-height)
               "Vis?" [vpos voff (+ h voff) (+ vpos chain-height)]
               "Already visible?"  (not (< vpos voff (+ h voff)
                                           (+ vpos chain-height)))
               "\nAlways scroll?" always)
    (when (> h chain-height)
      (println "Too bigby " (- h chain-height) h chain-height)
      (println "NCT" new-chain-top))
    (when new-chain-top (.scrollTo chain #js {:top new-chain-top}))
    (when new-bar-left  (.scrollTo bar   #js {:left new-bar-left}))))

(defn scroll-to-selected*
  [a b]
  (try
    (js/console.time "scrolling")
    (scroll-to-selected** a b)
    (finally
     (js/console.timeEnd "scrolling"))))

(defn scroll-to-selected!
  ([] (scroll-to-selected! true))
  ([always]
   (let [[el & more :as els] (js/document.querySelectorAll ".selected")]
     (when more
       (js/console.log "More than one selected element!" els))
     (scroll-to-selected* el always))))


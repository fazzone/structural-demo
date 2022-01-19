(ns comp.scroll)

(def ^:const scroll-hysteresis-px 3)

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

(defn scroll-to-selected*
  [el]
  (let [tl            (some-> el (.closest ".form-card"))
        chain         (some-> (or tl el) (.closest ".chain"))
        bar           (some-> chain (.closest ".bar"))
        chain-height  (some-> chain (.-clientHeight))
        bar-width     (some-> bar (.-clientWidth))
        ;; fit the entire toplevel if we can, otherwise just the selection
        tlh           (some-> tl (.getBoundingClientRect) (.-height) (js/Math.ceil))
        elh           (some-> el (.getBoundingClientRect) (.-height) (js/Math.ceil))
        vst           (if (< tlh chain-height) tl  el)
        h             (if (< tlh chain-height) tlh elh)
        vpos          (some-> chain (.-scrollTop))
        voff          (some-> vst (.-offsetTop))
        new-chain-top (and tl chain (scroll-1d chain-height h vpos voff))
        w             (some-> chain (.getBoundingClientRect) (.-width) (js/Math.ceil))
        hpos          (some-> bar (.-scrollLeft))
        hoff          (some-> chain (.-offsetLeft))
        new-bar-left  (and bar (scroll-1d bar-width w hpos hoff))]
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
             ;; "Vis?" [vpos voff (+ h voff) (+ vpos chain-height)]
             ;; "Already visible?"  (not (< vpos voff (+ h voff)
             ;;                             (+ vpos chain-height)))
             ;; "\nAlways scroll?" always
             "\nNCT" new-chain-top
             )
    #_(when (> h chain-height)
      (println "Too bigby " (- h chain-height) h chain-height)
      (println "NCT" new-chain-top))
    (when new-chain-top (.scrollTo chain #js {:top new-chain-top}))
    (when new-bar-left  (.scrollTo bar   #js {:left new-bar-left}))))

#_(defn scroll-to-selected!
  ([] (scroll-to-selected! true))
  ([always]
   (let [[el & more :as els] (js/document.querySelectorAll ".selected")]
     (when more
       (js/console.log "More than one selected element!" els))
     (scroll-to-selected* el always))))




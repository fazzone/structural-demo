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


(defn scroll-to-selected*
  [el el-cr]
  (let [tl       (some-> el (.closest ".form-card"))
        tl-cr    (some-> tl (.getBoundingClientRect))
        chain    (some-> (or tl el) (.closest ".chain"))
        chain-cr (some-> chain (.getBoundingClientRect))
        chh      (some-> chain-cr (.-height))
        chw      (some-> chain-cr (.-width))
        tlh      (some-> tl-cr (.-height) (js/Math.ceil))
        tlw      (some-> tl-cr (.-height) (js/Math.ceil))
        elh      (some-> el-cr (.-height) (js/Math.ceil))
        ;; fit entire toplevel if possible, otherwise just selection
        vst      (if (< tlh chh) tl el)
        h        (if (< tlh chh) tlh elh)
        vpos     (some-> chain (.-scrollTop))
        voff     (some-> vst (.-offsetTop))
        bar      (some-> chain (.closest ".bar"))
        bar-cr   (some-> bar (.getBoundingClientRect))
        barw     (some-> bar-cr (.-width))
        w        (some-> chain-cr (.-width) (js/Math.ceil))
        hpos     (some-> bar (.-scrollLeft))
        hoff     (some-> chain (.-offsetLeft))
        new-chain-top (and tl chain (scroll-1d chh h vpos voff))
        new-bar-left  (and bar (scroll-1d barw w hpos hoff))]
    (when new-chain-top
      (.scrollTo chain #js {:top new-chain-top}))
    (when new-bar-left
        (.scrollTo bar #js {:left new-bar-left}))))


(defn scroll-to-selected!
  ([]
   (let [[el & more :as els] (js/document.querySelectorAll ".selected")]
     (when more
       (js/console.log "There should be one selected element!" els))
     (if-not el
       (js/console.log "There should be one selected element!")
       (scroll-to-selected* el (.getBoundingClientRect el))))))


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

(ns sted.comp.cons
  (:require
   [sted.db-reactive :as dbrx]
   [clojure.set :as set]
   [datascript.core :as d]
   [sted.embed  :as e]
   [sted.core :as core]
   #_[sted.screenshot :as screenshot]
   [rum.core :as rum]))


(def font-size-ratio 0.4)

(rum/defc nonrec
  < {:key-fn (fn [n b s l r c] (:db/id n))}
  {:should-update (fn [astate bstate]
                    (let [[na _ _ _ ra ca] (:rum/args astate)
                          [nb _ _ _ rb cb] (:rum/args bstate)]
                      (or (not= ra rb)
                          (not= ca cb)
                          (not= (:max-tx (d/entity-db na))
                                (:max-tx (d/entity-db nb))))))}
  [node renderer size eid->pos row col sel?]
  (let [half   (* 0.5 size)
        double (* 2 size)
        width  (* 6 half)
        height double
        x      (* width col)
        y      (* height row)
        label  (str "#" (:db/id node) " " row "," col)
        st (if sel? "#ae81ff" "#fff")
        sv (:token/value node)
        ch (* half font-size-ratio)]
    (if-some [sv (:token/value node)]
      [:g {:fill st}
       [:text {:x x :y y} (str sv)]
       [:text {:x x :y (+ y half)} label]]
      [:g
       [:text {:fill st
               :x (+ x double (- (* ch (count label))))
               :y (- y ch)}
        label]
       [:text {:fill st :x x :y (- y ch)}
        (str (when-let [ct (:coll/type node)]
               (or (not-empty (str (e/open-delim ct) (e/close-delim ct)))
                   (case ct :hidden "-" ct))))]
       [:g {:stroke st}
        (when (nil? sv)
          [:rect {:x x :y y :width (+ size size) :height size}])
        (when (nil? sv) 
          [:line {:x1 (+ x size) :y1 y :x2 (+ x size) :y2 (+ y size)}])
        (when-some [cdr (:seq/next node)]
          (let [[nr nc]    (eid->pos (:db/id cdr))
                arrowleft  (+ x (* 3 half))
                arrowright (+ (* nc width) (* 0.2 size))]
            (when (not= row nr)
              (println "Bad layout - cdr " (:db/id cdr) "should be in row" row "but is in" nr "??"))
            [:path {:marker-end "url(#head)"
                    :d          (str "M" arrowleft "," (+ y half)
                                     "H" arrowright)}]))
        (when-some [car (:seq/first node)]
          (let [[nr nc] (eid->pos (:db/id car))]
            (when (not= col nc)
              (println "Bad layout - car " (:db/id car) "should be in column" col "but is in" nc "??"))
            [:path {:marker-end "url(#head)"
                    :d          (str "M" (str (+ x half)) "," (str (+ y half))
                                     "V" (str (- (* nr height) half)))}]))]])))

(defn my-traversal
  [top]
  (loop [n          0
         nextfront  [[0 0 top]]
         firstfront []
         occ        []
         save       []]
    (letfn [(claim [r c e] (update occ c (fnil assoc (sorted-map)) r e))
            (next-row [c] (inc (key (first (rseq (nth occ c))))))
            (collide? [r c] (some-> (get occ c) (subseq >= r) seq))]
      (cond
        (> n 999)
        (throw (ex-info "Too many" {}))
        
        ;; try to go right first
        (seq nextfront)
        (let [[r c e] (peek nextfront)]
          (if (collide? r c)
            ;; something is already here - backtrack to when we decided to put it on this row
            (let [[sc se nf ff nocc] (peek save)]
              (recur (inc n)
                     nf
                     ;; have to go far enough down to clear this column
                     (conj ff [(next-row c) sc se])
                     nocc
                     (pop save)))
            ;; safe
            (recur (inc n)
                   (cond-> (pop nextfront) (:seq/next e) (conj [r (inc c) (:seq/next e)]))
                   (cond-> firstfront      (:seq/first e) (conj [(inc r) c (:seq/first e)]))
                   (claim r c e)
                   ;; 
                   save)))
        
        ;; go down
        (seq firstfront)
        (let [[r c e] (peek firstfront)]
          (if (collide? r c)
            (throw (ex-info "not supposed to happen" {:r r :c c}))
            (recur (inc n)
                   (cond-> nextfront        (:seq/next e) (conj [r (inc c) (:seq/next e)]))
                   (cond-> (pop firstfront) (:seq/first e) (conj [(inc r) c (:seq/first e)]))
                   (claim r c e)
                   ;; 
                   (conj save [c e nextfront (pop firstfront) occ n]))))
        
        :else
        (into {}
              (for [[c col] (map vector (range) occ)
                    [r e]   col]
                [[r c] e]))))))



;; var btn = document.querySelector('button');
;; var svg = document.querySelector('svg');
;; var canvas = document.querySelector('canvas');
;; function triggerDownload (imgURI) {
;;   var evt = new MouseEvent('click', {
;;     view: window,
;;     bubbles: false,
;;     cancelable: true
;;   });
;;   var a = document.createElement('a');
;;   a.setAttribute('download', 'MY_COOL_IMAGE.png');
;;   a.setAttribute('href', imgURI);
;;   a.setAttribute('target', '_blank');
;;   a.dispatchEvent(evt);
;; }

(defn save-svg
  [svg w h]
  (let [canvas (js/document.createElement "canvas")
        _ (js/console.log "Canvas" canvas)
        g (.getContext canvas "2d")
        _ (js/console.log g)
        xml (.serializeToString (js/XMLSerializer.) svg)
        _ (js/console.log xml)
        img (js/Image.)
        blob (js/Blob. #js [xml]
                       #js {:type "image/svg+xml;charset=utf-8"})
        url (.createObjectURL js/window.URL blob)]
    
    (js/console.log "UU" url)
    #_(set! (.-onload img)
            (fn []
              (println "Loaderino")
              (set! (.-strokeStyle g) "#000")
              (.fillRect g 0 0 w h)
              (.drawImage g img 0 0)
              (.revokeObjectURL js/window.URL blob)
              (js/console.log
               (.toDataURL canvas "image/png"))
              #_(.toDataURL canvas "image/png")))
    (set! (.-onerror img)
          (fn [a b c]
            (js/console.log "Load error" a b c)))
    #_(set! (.-style img) "background-color: #000;")
    (set! (.-src img) url)
    (let [w (js/window.open "")]
      (js/console.log "W=" w (.-outerHTML img))
      (js/setTimeout
       (fn []
         (.write (.-document w) (.-outerHTML img))
         (set! (.-backgroundColor (.-style (.-body (.-document w))))
               "#000"))
       1))))

(rum/defc testing
  ;; < dbrx/deeply-ereactive
  [top bus renderer]
  (let [size     50
        half     (* 0.5 size)
        width    (* 6 half)
        height   (* 2 size)
        inset    size
        svgref   (rum/create-ref)
        ;; _        (js/console.time "layout")
        pos->ent (my-traversal top)
        eid->pos (reduce-kv
                  (fn [a p e] (assoc a (:db/id e) p))
                  {}
                  pos->ent)
        rmax     (apply max (map first (keys pos->ent)))
        cmax     (apply max (map second (keys pos->ent)))
        vw       (+ inset (* (inc cmax) width))
        vh       (+ inset (* (inc rmax) height))
        ;; _        (js/console.timeEnd "layout")
        sel      (core/get-selected-form (d/entity-db top))
        ]
    [:svg  {:viewBox    (str "0 0 " vw " " vh)
            :xmlns      "http://www.w3.org/2000/svg"
            ;; "xmlns:xlink" "http://www.w3.org/1999/xlink"
            :xmlnsXlink "http://www.w3.org/1999/xlink"
            
            :version "1.1" 
            :ref     svgref
            :style   {;; :outline    "1px solid"
                      :display    "block"
                      :background "transparent"
                      :height "100%"
                      :width "100%"}}
     [:defs
      [:marker {:id "head" :orient "auto" :markerWidth 20 :markerHeight 40 :refX 0.1 :refY 3}
       [:path {:fill "#fff"
               :d    "M0,0 V6 L6,3 Z"}]]]
     [:g { ;; :stroke "#fff"
          :transform    (str "translate(" size "," size ")")
          ;; :fill "#fff"
          :stroke       :none
          :fill         :none
          :stroke-width 1
          :font-size    (* 0.4 size)}
      (for [[[r c] e] pos->ent]
        (nonrec e renderer size eid->pos r c
                (= (:db/id e) (:db/id sel))))]]
    
    #_[:button {:on-click (fn [c]
                            (let [svg (rum/deref svgref)]
                              (js/console.log svg)
                              (save-svg svg vw vh)))}
       "Save svg"]))



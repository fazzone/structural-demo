(ns sted.eda.kicad-footprint
  (:require
   [clojure.string :as string]
   [goog.string :as gstring]
   [sted.df.async :as a]))

(def fp-draw-cols [:kicad_footprint_id :layer :d_mm :width_mm :text_type :text :font_size_w_mm :font_size_h_mm
                   :text_x_mm
                   :text_y_mm
                   :wkt
                   ])

(defn fp->draw-cols
  [[f & args]]
  (case f
    fp_text (let [[ttype text [_at x y] [_layer layer]
                   [_effects [_font [_size w h] [_thickness thk]]]] args]
              #js {:layer          layer
                   :width_mm       thk
                   :text           text
                   :text_type      ttype
                   :font_size_w_mm w
                   :font_size_h_mm h
                   :text_x_mm x
                   :text_y_mm y})
    fp_arc    (let [[[_start] [_end] [_angle] [_layer l] [_width w]] args]
                #js {:layer l :width_mm w})
    fp_circle (let [[[_center] [_end] [_layer l] [_width w]] args]
                #js {:layer l :width_mm w})
    fp_poly   (let [[[_pts] [_layer l] [_width w]] args]
                #js {:layer l :width_mm w})
    fp_curve  (throw (ex-info "not yet" {:kicad 'fp_curve}))
    fp_line (let [[[_start] [_end] [_layer l] [_width w]] args]
              #js {:layer l :width_mm w})
    nil))

(defn pts->d
  [[[sx sy] & pts]]
  (str "M " sx " " sy
       (apply str
              (for [[x y] pts]
                (str " L " x "," y)))))

(defn ellipse->d
  [x y rx ry]
  (let [dx (+ rx rx)]
    (str "M" (- x rx) "," y
         " a" rx "," ry ",0,1,0," (+ dx) "," 0
         " a" rx "," ry ",0,1,0," (- dx) "," 0)))

(defn arc*
  [cx cy r t1 t2]
  (println "T1" (/ t1 (/ js/Math.PI 180.0)) "deg"
           " T2 " (/ t2 (/ js/Math.PI 180.0)) "deg"
           ) 
  (let [
        ;; start is [r,0] rotated t1
        sx (+ cx (* r (js/Math.cos t1)))
        sy (+ cx (* r (js/Math.sin t1)))
        
        ex (+ cx (* r (js/Math.cos t2)))
        ey (+ cx (* r (js/Math.sin t2)))
        
        theta1 (cond-> t1
                 (neg? t1) (+ (* 2 js/Math.PI)))
        theta2 (cond-> t2
                 (neg? t2) (+ (* 2 js/Math.PI)))
        large-arc? (< (js/Math.abs (- theta2 theta1))
                      js/Math.PI)
        laf (if large-arc? (do "1") "0")]
    (str
     (ellipse->d cx cy 1 1)
     ";M" cx "," cy
     " L" sx "," sy
     ";M" cx "," cy
     " L" ex "," ey) 
    
    #_(str
       "M " ex "," ey
       " A" r "," r "," laf ",0,0," sx "," sy)))

(defn fp->d
  [[f & args]]
  (case f
    fp_line (let [[[_start sx sy] [_end ex ey]] args]
              (str "M" sx "," sy " L" ex "," ey))
    
    fp_arc (let [[[a] [b] :as arc-args] args]
             (case b
               end (println "Arrc Start End")
               mid (let [[[ss x1 y1] [sm x2 y2] [se x3 y3]] args
                         ;; TODO handle x1y1 == x3y3 (a circle)
                         ma (/ (- y2 y1)
                               (- x2 x1))
                         mb (/ (- y3 y2)
                               (- x3 x2))
                         x (/ (+ (* ma mb (- y1 y3))
                                 (* mb (+ x1 x2))
                                 (- (* ma (+ x2 x3))))
                              (* 2 (- mb ma)))
                         y (+ (* (/ -1 ma)
                                 (- x (/ (+ x1 x2) 2)))
                              (/ (+ y1 y2) 2))
                         r (js/Math.hypot (- x x1) (- y y1))
                         xrot "0"
                         laf "0"
                         sweep "1"]

                     (str
                      "M " x1 "," y1
                      " A" r "," r "," xrot "," laf "," sweep "," x3 "," y3))))
    
    fp_circle (let [[[_center cx cy] [_end ex ey]] args
                    r (js/Math.hypot (- ex cx) (- ey cy))]
                
                (ellipse->d cx cy r r)
                #_(str "M" cx "," cy
                       " A" r "," r ",0,1,0" cx (+ cy r)
                       " A" r "," r ",0,1,0" cx (- cy r)
                       " z"))
    fp_poly (let [[[_pts [[sx sy] & more :as pts]]] args]
              (str "M" sy "," sy
                   (apply str (for [[x y] more]
                                (str " L" x "," y)))))
    fp_curve (throw (ex-info "not yet" {:kicad 'fp_curve}))
    
    nil))

(def ^:dynamic *arc-approx-sectors* 36)
(defn fp->wkt
  [[f & args]]
  (case f
    fp_line (let [[[_start sx sy] [_end ex ey]] args]
              (str "LINESTRING (" sx " " sy ", " ex " " ey ")"))
    fp_arc (let [[[_start sx sy] [_end ex ey] [_angle angle]] args
                 r (js/Math.hypot (- ex sx) (- ey sy))
                 radians (* angle js/Math.PI (/ 1 180.0))
                 dx (* r (js/Math.sin radians))
                 dy (* -1 r (js/Math.cos radians))
                 end-radians (js/Math.atan2 (- ex sx) (- sy ey))
                 n  1 #_(js/Math.ceil
                    (/ (* *arc-approx-sectors* (js/Math.abs (-  radians end-radians)) )
                       (* 2 js/Math.PI)))
                 step (cond-> (/ (-  radians end-radians) n)
                        (< radians end-radians) (-))]
             (str "LINESTRING ("
                  (string/join ","
                               (for [i (range 0 (inc n))]
                                 (str
                                  "" (+ sx (* r (js/Math.sin (+ (* i step) radians))))
                                  " " (- sy (* r (js/Math.cos (+ (* i step) radians)))))))
                  ;; "," ex " " ey
                  ")"))
    #_(let [[[_center cx cy] [_start ex ey] [_angle span-deg]] args
          r (js/Math.hypot (- ex cx) (- ey cy))
          angular-span (* span-deg js/Math.PI (/ 1 180.0))
          start-angle (js/Math.atan2 (- ey cy) (- ex cx))
          n (js/Math.ceil (/ (* *arc-approx-sectors* angular-span)
                             (* 2 js/Math.PI)))
          step (/ angular-span n)]
      (println "Step" step "Satart" start-angle)
      (str "LINESTRING ("
           cx " " cy ","
           (+ cx (* r (js/Math.sin start-angle))) " " (- cy (* r (js/Math.cos start-angle)))
           
           (+ cx (* r (js/Math.sin (- start-angle 0.4))))
           " "
           (- cy (* r (js/Math.cos
                       (- start-angle 0.4)
                       )))
           #_(string/join ","
                          (for [i (range 0 (inc n))]
                            (str
                             "" (+ cx (* r (js/Math.sin (+ start-angle (* i step)))))
                             " " (- cy (* r (js/Math.cos (+ start-angle (* i step))))))))
           ;; "," ex " " ey
           ")"))
    
    fp_circle (let [[[_center cx cy] [_end ex ey]] args
                    r (js/Math.hypot (- ex cx) (- ey cy))
                    step (/ (* 2 js/Math.PI)
                            *arc-approx-sectors* )]
                (str "LINESTRING ("
                     (string/join ","
                                  (for [i (range 0 (inc *arc-approx-sectors*))]
                                    (str
                                     "" (+ cx (* r (js/Math.sin (* i step))))
                                     " " (- cy (* r (js/Math.cos (* i step)))))))
                     ")"))
    
    fp_poly (let [[[_pts pts]] args]
              (str "POLYGON ("
                   (for [[_xy x y] pts]
                     (str x ", " y))
                   ")"))
    fp_curve (throw (ex-info "not yet" {:kicad 'fp_curve}))
    
    nil))

(def simple  (quote #{layer tedit descr tags attr version generator}))
(def draw (quote #{fp_text fp_line fp_circle fp_arc fp_poly fp_curve}))
(def pad (quote #{pad}))

      
;; #js [ins-fp
;;      (into-array
;;       (cons fpname
;;             (for [[[_k v :as t]] (map (group-by first (attrs :simple)) simple)]
;;               v)))]

(defn footprint-insert*
  [fpname attrs]
  (let [s (into {}
                (attrs :simple))]
    #js [(str "insert into kicad_footprint"
              "        (name, layer, tedit, descr, tags_json, attr, version, generator)"
              "\nvalues(?,    ?,     ?,     ?,     ?,         ?,    ?,       ?)"
              "\nreturning id")
         #js [fpname
              (s 'layer)
              (s 'tedit)
              (s 'descr)
              (some-> (s 'tags) (.split " ") (.sort) js/JSON.stringify)
              (s 'attr)
              (s 'version)
              (s 'generator)]]))

(def ins-draw
  (str "insert into kicad_footprint_draw("
       (string/join "," (map name fp-draw-cols))
       ")\n values (" (string/join "," (repeat (count fp-draw-cols) "?"))
       ")\nreturning id"))

(defn draw-inserts*
  [footprint-id attrs]
  (for [del  (attrs :draw)
        :let [dcs (fp->draw-cols del)]]
    #js [ins-draw
         (into-array
          (for [d fp-draw-cols]
            (case d
              :d_mm               (fp->d del)
              :wkt                (fp->wkt del)
              :kicad_footprint_id footprint-id
              (aget dcs (name d)))))]))

(defn kicad->map
  [attrs]
  (into {}
        (for [[a & body] attrs]
          [a (vec body)])))

(def ^:dynamic *places* 5)
(defn n [x] (.toFixed x *places*))

(defn rect->d
  [x y w h]
  (str "M" (n x) "," (n y)
       " L" (+ x w) "," y
       " L" (+ x w) "," (+ y h)
       " L" x "," (+ y h)
       " z"))

(defn circle->d
  [x y r]
  (let [d (+ r r)]
    (str "M" (- x r) "," y
         " a" r "," r ",0,1,0," (+ d) "," 0
         " a" r "," r ",0,1,0," (- d) "," 0)))

(defn roundrect->d
  [x y w h rx ry]
  (str "M" (+ x rx ) "," y
       " h" (- w rx rx)
       " a" rx "," ry ",0,0,1," (+ rx) "," (+ ry)
       " v" (- h ry ry)
       " a" rx "," ry ",0,0,1," (- rx) "," (+ ry)
       " h" (- (- w rx rx))
       " a" rx "," ry ",0,0,1," (- rx) "," (- ry)
       " v" (- (- h ry ry))
       " a" rx "," ry ",0,0,1," (+ rx) "," (- ry)
       " z"))

(defn pad->d
  [[_ padname padtype padshape & attrs :as args]]
  #_(println "P->D" args )
  (let [[[_at x y] [_size sx sy] [_drill drill]] attrs
        halfw (* 0.5 sx)
        halfh (* 0.5 sy)
        prop (kicad->map attrs)]
    (println "Proppy" prop)
    (case padshape
      "rect"  (rect->d (- halfw) (- halfh) sx sy)
      "circle" (ellipse->d 0 0 halfw halfw)
      "oval"   (ellipse->d 0 0 halfw halfh)
      
      "roundrect"
      (let [minor (min sx sy)
            [rratio] (prop 'roundrect_rratio)
            rxy (* minor
                   (or rratio 0))]
        #_(roundrect->d (- halfw (- rxy)) (- halfh (- rxy)) (- sx rxy) (- sy rxy) rxy rxy)
        #_(roundrect->d (- halfw) (- halfh) (- sx rxy) (- sy rxy) rxy rxy)
        (roundrect->d (- halfw) (- halfh) sx sy rxy rxy)
        #_(roundrect->d (- halfw) (- halfh)
                        sx
                        sy
                        rxy
                        rxy))
      nil)))

#_(defn find-or-create-pad!
  [db [_pad _name type shape [_at x y] [_size width_mm height_mm ] [a1 :as av1] [a2 :as av2] :as pad]]
  (let [d_mm (pad->d pad)
        drill_mm (case a1 drill (nth av1 1) nil)
        layers_json (-> (case a1 drill av2 av1)
                        (next)
                        (into-array)
                        (.sort)
                        js/JSON.stringify)
        fpq (str "select id from kicad_pad where"
                 " d_mm = ? and type = ? and shape = ? and drill_mm = ? and layers_json = ?")
        fpp (do #js [d_mm  type shape drill_mm layers_json])
        
        ipq (str "insert into kicad_pad"
                 "\n       (type, shape, width_mm, height_mm, d_mm, drill_mm, layers_json)"
                 "\nvalues (?,    ?,     ?,        ?,         ?,    ?,        ?)"
                 "\n returning id")
        ipp (do #_____ #js [type  shape  width_mm  height_mm  d_mm  drill_mm  layers_json])]
    
    (a/let [_ (println "Exec" fpq)
            fr (-> (.exec db fpq fpp)
                   (.-get))
            [[found-id]] (.-rows fr)]
      (.free fr)
      (or found-id
          (a/let [_ (println "Exec" ipq)
                  ir (-> (.exec db ipq ipp)
                         (.-get))
                  [[inserted-id]] (.-rows ir)]
            (.free ir)
            inserted-id)))))


(defn find-or-create-pad!
  [db [_pad _name type shape [_at x y] [_size width_mm height_mm] :as pad]]
  (let [d_mm (pad->d pad)
        ipq (str "insert into kicad_pad"
                 "\n           (type, shape, width_mm, height_mm, d_mm)"
                 "\nvalues     (?,    ?,     ?,        ?,         ?)"
                 "\non conflict(type, shape, width_mm, height_mm, d_mm)"
                 "\ndo update set refcount = 1+refcount"
                 "\nreturning id")
        ipp (do #_____ #js [type  shape  width_mm  height_mm  d_mm])]
    
    
    (a/let [ir (-> (.exec db ipq ipp)
                   (.-get))
            [[inserted-id]] (.-rows ir)]
      (.free ir)
      inserted-id)))

(defn footprint-pad-insert
  [kicad-footprint-id kicad-pad-id
   [_pad padname _t _s [_at x y] _size [a1 :as av1] [a2 :as av2]]]
  (let [name        (case padname "" nil padname)
        drill_mm    (case a1 drill (nth av1 1) nil)
        layers_json (-> (case a1 drill av2 av1)
                        (next)
                        (into-array)
                        (.sort)
                        js/JSON.stringify)]
    (do #js [(str "insert into kicad_footprint_pad"
                  "\n       (x_mm, y_mm, name, drill_mm, layers_json, kicad_pad_id, kicad_footprint_id)"
                  "\nvalues (?,    ?,    ?,    ?,        ?,           ?,            ?)")
             #js [           x     y     name  drill_mm  layers_json  kicad-pad-id  kicad-footprint-id]])))


(defn classify-attrs*
  [body]
  (->> body
       (group-by (fn classify [[a]]
                   (condp contains? a
                     draw   :draw
                     simple :simple
                     pad    :pad
                     nil)))))

(defn insert-footprint!
  [db [_footprint fpname & body]]
  (let [attrs (classify-attrs* body)
        [insq insp] (footprint-insert* fpname attrs)]
    (a/let [result (-> (.exec db insq insp)
                       (.-get))
            [[fpid] :as rows] (.-rows result)
            _ (.free result)
            idraws (js/Promise.all (for [[q p] (draw-inserts* fpid attrs)]
                                     (.exec db q p)))
            ipads (reduce
                   (fn [pr pad]
                     (a/let [rp pr
                             pad-id (find-or-create-pad! db pad)]
                       (if-not pad-id
                         (println "NO PAD ID111" pad))
                       (let [[q p] (footprint-pad-insert fpid pad-id pad)]
                         (.exec db q p))))
                   nil
                   (attrs :pad))]
      (println "Ok"))))


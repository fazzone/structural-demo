(ns sted.eda.kicad-footprint
  (:require
   [clojure.string :as string]
   [goog.string :as gstring]
   [sted.df.async :as a]))

(def fp-draw-cols [:kicad_footprint_id :layer :d_mm :width_mm :text_type :text :font_size_w_mm :font_size_h_mm
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
                   :font_size_h_mm h})
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
(defn fp->d
  [[f & args]]
  (case f
    fp_line (let [[[_start sx sy] [_end ex ey]] args]
              (str "M" sx "," sy " L" ex "," ey))
    
    fp_arc (let [[[_start sx sy] [_end ex ey] [_angle angle]] args
                 r (js/Math.hypot (- ex sx) (- ey sy))
                 radians (* angle js/Math.PI (/ 1 180.0))
                 dx (* r (js/Math.sin radians))
                 dy (* -1 r (js/Math.cos radians))
                 end-radians (js/Math.atan2 (- ex sx) (- sy ey))]
             (str "M" (+ sx dx) "," (+ sy dy)
                    " A" r "," r ",0,0,1," ex "," ey ""))
    fp_circle (let [[[_center cx cy] [_end ex ey]] args
                    r (js/Math.hypot (- ex cx) (- ey cy))]
                (str "M" cx "," cy
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

(def ins-fp (str "insert into kicad_footprint(name,"
                  (string/join "," simple)
                  ")\n values (" (string/join "," (repeat (inc (count simple)) "?"))
                  ")\nreturning id"))

(defn footprint-insert*
  [fpname attrs]
  #js [ins-fp
       (into-array
        (cons fpname
              (for [[[_k v :as t]] (map (group-by first (attrs :simple)) simple)]
                v)))])

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

(defn fp->pad-cols
  [[f & args]]
  (reduce
   (fn [acc [f & args]]
     (case f
       at (let [[_ x y] args]
            (aset acc "at_x_mm" x)
            (aset acc "at_y_mm" y))
       size (let [[_ w h] args]
              (aset acc "width_mm" w)
              (aset acc "height_mm" h))
       drill (let [[_ ds] args]
               (aset acc "drill_mm" ds))
       layers (let [[_ & ls] args]
                (aset acc "layers_json" (js/JSON.stringify ls)))
       nil))
   #js {}))

(defn pad-inserts*
  [footprint-id attrs]
  (println "A T T R S")
  (run! prn attrs)
  (for [[_pad padname padtype padshape [_at atx aty] [_size w h] [a1 :as av1] [a2 :as av2] :as dink] (attrs :pad)]
    (do (prn "Dink:" dink)
        (prn "A1" a1)
        (prn "A2" a2)
        (run! (comp prn (juxt type identity) first) (drop 4 dink))
        (println "Type of a1??" (type a1) (pr-str a1) ) 
        #js [(str "insert into kicad_footprint_pad"
                  "\n       (kicad_footprint_id, name, type, shape, x_mm, y_mm, width_mm, height_mm, d_mm, drill_mm, layers_json)"
                  "\nvalues (?,                  ?,    ?,    ?,     ?,    ?,    ?,        ?,         ?,    ?,         ?)")
             #js [footprint-id (case padname "" nil padname) padtype padshape atx aty w h
                  "no"
                  (case a1 drill (nth av1 1) nil)
                  (-> (case a1 drill av2 av1)
                      (next)
                      (into-array)
                      (.sort)
                      js/JSON.stringify) ]])))

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
            ipads (js/Promise.all (for [[q p] (pad-inserts* fpid attrs)]
                                    (do
                                      (println "PI" q p)
                                      (.exec db q p))))]
      (println "Ok"))))


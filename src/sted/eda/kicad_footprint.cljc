(ns sted.eda.kicad-footprint
  (:require
   [sted.eda.dsn :as dsn]
   [clojure.string :as string]
   
   [sted.df.async :as a]))

(def fp-draw-cols [:kicad_footprint_id :layer :d_mm :width_mm :text_type :text :font_size_w_mm :font_size_h_mm
                   :text_x_mm
                   :text_y_mm
                   :wkt
                   ])

(defn rect->d
  [x y w h]
  (str "M" x "," y
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
    
    (fp_rect fp_line)
    (let [[[_start] [_end] [_layer l] [_width w]] args]
      #js {:layer l :width_mm w})
    
    (do (println "No draw cols" f)
        nil)))

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
                     (str "M " x1 "," y1
                          " A" r "," r "," xrot "," laf "," sweep "," x3 "," y3))))
    
    fp_circle (let [[[_center cx cy] [_end ex ey]] args
                    r (js/Math.hypot (- ex cx) (- ey cy))]
                (ellipse->d cx cy r r))
    
    fp_poly (let [[[_pts & pts]] args]
              (pts->d (for [p pts] (subvec p 1))))
    
    fp_curve (throw (ex-info "not yet" {:kicad 'fp_curve}))
    
    fp_rect (let [[[_ sx sy] [_ ex ey] [_layer l] [_width w]] args
                  xmin (min sx ex)
                  ymin (min sy ey)
                  xmax (max sx ex)
                  ymax (max sy ey)
                  w (- xmax xmin)
                  h (- ymax ymin)
                  halfw (* 0.5 w)
                  halfh (* 0.5 h)]
              (rect->d sx sy (- ex sx) (- ey sy)))
    
    fp_text nil
    
    (do
      (println "No fp d" f)
      nil)))

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
    
    fp_poly (let [[_pts pts] args]
              (str "POLYGON ("
                   #_(for [[_xy x y] pts]
                       (str x ", " y))
                   ")"))
    fp_curve (throw (ex-info "not yet" {:kicad 'fp_curve}))
    
    nil))

(def simple  (quote #{layer tedit descr tags attr version generator}))
(def draw (quote #{fp_text fp_line fp_circle fp_arc fp_poly fp_curve fp_rect}))
(def pad (quote #{pad}))

(defn footprint-insert*
  [fpname attrs]
  (let [s (into {} (attrs :simple))]
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
  [body]
  (into {}
        (for [b body]
          (if-not (coll? b)
            [b true]
            [(first b) (next b)]))))


(def ^:dynamic *places* 5)
(defn n [x] (.toFixed x *places*))


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

(defn oval->d
  [x y w h]
  (let [d (min w h)
        r (* 0.5 d)]
   (str "M" (+ x r) "," y
        " h" (- w d)
        " a" r "," r ",0,0,1," 0 "," d
        " h" (- d w)
        " a" r "," r ",0,0,1," 0 "," (- d))))

(defn pad->d
  [[_ padname padtype padshape & attrs :as args]]
  #_(println "P->D" args )
  (let [[[_at x y angle] [_size sx sy] [_drill drill]] attrs
        halfw (* 0.5 sx)
        halfh (* 0.5 sy)
        prop (kicad->map attrs)]
    #_(println "Proppy" prop)
    (case padshape
      "rect" (rect->d (- halfw) (- halfh) sx sy)
      "circle" (ellipse->d 0 0 halfw halfw)
      "oval" (cond
               (= sx sy)
               #_(str "M" 0 "," 0 "z")
               (circle->d 0 0 (* 0.5 sx))
                   
               :else 
               (oval->d (- halfw) (- halfh) sx sy)
               #_(str "M" 0 "," 0 "z"
                      )
                   
               :else (do (println "Bad oval" x y halfw halfh)
                         (str "M" 0 "," 0 "z")
                         )
                   
               )
      
      
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
      "custom"
      (let [[[pt & body] & more] (prop 'primitives)]
        (cond
          (and (nil? more) (= pt 'gr_poly))
          (let [[[_pts & pts ]] body]
            (pts->d (for [p pts] (subvec p 1))))
          
          :else (println "Your footprint is too fancy")))
      
      (do (println "Don't understand pad shape" padshape)
          nil))))

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
   [_pad padname _t _s [_at x y r] _size [a1 :as av1] [a2 :as av2]]]
  (let [name        (case padname "" nil padname)
        drill_mm    (case a1 drill (nth av1 1) nil)
        layers_json (-> (case a1 drill av2 av1)
                        (next)
                        (into-array)
                        (.sort)
                        js/JSON.stringify)
        transform (when r
                    (str "rotate(" r ")"))]
    (do #js [(str "insert into kicad_footprint_pad"
                  "\n       (x_mm, y_mm, name, drill_mm, layers_json, transform, kicad_pad_id, kicad_footprint_id)"
                  "\nvalues (?,    ?,    ?,    ?,        ?,           ?,         ?,            ?)")
             #js [           x     y     name  drill_mm  layers_json  transform  kicad-pad-id  kicad-footprint-id]])))


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








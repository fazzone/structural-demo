(ns comp.hex
  (:require [rum.core :as rum]
            [clojure.string :as string]
            [goog.string :as gstring]))

(def tau (* 2 js/Math.PI))


(def hex-size-x 30)
(def hex-size-y 32)

(def hex-angles
  (vec
   (for [i (range 6)]
     (/ (*  js/Math.PI (+ 30 (* 60 i)))
        180))))



(defn hex-angles-rotated
  [r]
  (vec
   (for [i (range 6)]
     (/ (*  js/Math.PI (+ r 30 (* 60 i)))
        180))))

(defn hex-group
  [x y r]
  [:polygon
   {:points
    (->> (for [a (hex-angles-rotated r)]
           [(+ x (* hex-size-x (Math/cos a)))
            (+ y (* hex-size-y (Math/sin a)))])
         (map (fn [[x y]] (str x "," y)))
         (string/join " "))}])



#_(defn axial-to-screen
  [q r]
  (let [x (+ (* q (js/Math.sqrt 3))
             (* r 0.5 (js/Math.sqrt 3)))
        y (* -3 r 0.5)]
    [(* x hex-size-x) (* y hex-size-y)]))

(def transform-axial->screen
  (atom [(js/Math.sqrt 3)
         0
         
         (* 0.5 (js/Math.sqrt 3))
         -1.5]))

(defn axial-to-screen
  [q r]
  (let [[a b c d] @transform-axial->screen
        x (+ (* q a) (* r c))
        y (+ (* q b) (* r d))]
    [(* x hex-size-x)
     (* y hex-size-y)]))

(defn axial-to-oddr
  [q r]
  (let [x (+ q r)
        z (- 0 r)]
    [(+ x (/ (- z (bit-and z 1)) 2))
     z]))

(defn axial-to-cube
  [q r]
  (let [x (+ q r)
        y (- 0 q)
        z (- 0 r)]
    [x y z]))

(defn oddr-to-axial
  [c r]
  [(- (+ r c)
      (quot (- r (bit-and r 1))
            2))
   (- r)])


(def scale #_["A" "A♯" "B" "C" "C♯" "D" "D♯" "E" "F" "F♯" "G" "G♯"]
  [ "C♯" "D" "D♯" "E" "F" "F♯" "G" "G♯" "A" "A♯" "B" "C" ])

(defn hex-key
  [i j & {:keys [text fill]}]
  (let [[x y] (axial-to-screen i j)
        index (+ (* 2 i) j)
        note (nth scale (mod index 12))
        accidental (< 1 (count note))
        text (str note " " text)
        fs 30]
    [:g {:fill (or fill (if accidental "#000" "#fff"))}
     (hex-group x y 0) 
     [:text {:fill (if accidental "#fff"  "#000")
             :stroke "none"
             :font-size (str fs "px")
             :text-anchor "middle"
             :alignment-baseline "central"
             :x  x #_(- x (* fs 0.25 (count text)))
             :y (+ y #_(* 0.2 fs))}
      text]]))



(def lumatone-row-sizes
  {0 2
   1 5
   2 8
   3 11
   4 14
   5 17
   6 20
   7 23
   8 26
   9 28
   10 26
   11 23
   12 20
   13 17
   14 14
   15 11
   16 8
   17 5
   18 2})


(rum/defc edit-transform < rum/reactive
  []
  [:div
   (map-indexed
    (fn [i v]
      [:input {:value v
               :type "range"
               :min -2
               :max 2
               :step "0.01"
               :on-change #(swap! transform-axial->screen assoc i (js/parseFloat (.-value (.-target %))))}])
    (rum/react transform-axial->screen))])



(comment
  #_[:img {:src "/lumatone12tet-janko.png"}]
     #_(let [m 4]
       [:svg {:style {:width "500px" :border "1px solid aliceblue"}
              ;; rotate(14.2deg) skewY(4deg)
              :viewBox (gstring/format "%f %f %f %f"  (- m) (- m) (+ m m hex-cols) (+ m m hex-rows))}
      
        #_(hex-group 0 0 "oka")
        [:g
         {:transform "skewY(-4)  rotate(-14.2)"}
         (for [i (range 0 hex-cols)
               j (range 0 hex-rows)
               :when (< i (get lumatone-row-sizes j ))]
           [:circle 
            {:key (+ j (* i hex-cols)) 
             :cx (cond-> i
                   (odd? j) inc)
             :cy j
             :r 0.1
             :fill "tomato"}])]]))

(defn thing
  []
  (let [longest-row 28
        rows-each-side 9]
   [:svg {:viewBox  "-200 -460 1800 515" 
          :style {:border "1px solid tomato"
                  :background "transparent"
                  :width "900px"
                  :opacity 0.9}}
    [:g {:stroke "#888"
         :fill "none"
         :stroke-width 2
         :transform "skewY(-15) skewX(15) rotate(-1.5)"}
     (for [row (range 0 (inc rows-each-side))
           refl [true nil]
           :when (not (and refl (zero? row)))
           :let [width (condp = row
                         0 longest-row
                         1 (- longest-row 2)
                         (- longest-row
                            2
                            (* 3 (dec row))))]
           col (range 0 width)]
       (let [q #_(if refl
                 (+ (- longest-row col)
                    (quot row 2))
                 (- col
                    (quot row 2)))
             (cond
               refl  (+ (- longest-row col)
                        (quot row 2))
               (= 0 row) col
               :else (- col  (quot row 2) (mod row 2)) 
               )
             r (cond-> row refl -)]
         (hex-key q r
                  ;; :fill (cond refl "tomato"
                  ;;             (= 0 row) "#ae81ff")
                  )))]]))

(defn half-cosine-curve
  [xa ya xb yb]
  (let [k 0.3642
        dx (- xb xa)]
    (str "C " (+ xa (* k dx)) " " ya
         " "  (- xb (* k dx)) " " yb
         " "  xb              " " yb)))

(defn half-cosine-scurve
  [xa xb yb]
  (let [k 0.3642
        dx (- xb xa)]
    (str "S "(- xb (* k dx)) " " yb
         " "  xb              " " yb)))

(defn cosine-curve
  [turns]
  (let [n (* 2 turns)
        k 0.3642]
    (str
     (half-cosine-curve 0 0 1 1)
     (loop [i 1
            s ""]
       (if (= i n)
         s
         (let [y (if (even? i) 1 0)
               next (inc i)]
           (recur next
                  (str s
                       "S" (- next k) " " y
                       " " next       " " y))))))))

(rum/defc wave []
  [:div
   (let [sw 0.01
         k 0.3642]
     [:div {:style {:width "1000px" 
                    :border "1px solid aliceblue"}}
      [:svg {:viewBox
             (str 0  " " (* -0.5 sw) " " 4 " " (+ 1 sw) )}
       [:path {:stroke "tomato"
               :fill "none"
               :stroke-width sw
               :d (str "M 0 0"
                       (cosine-curve 2))}]]])])

(rum/defc piano []
  (let [wkw    840
        bkw    490
        octave (* 7 wkw)
        aspect 0.75
        wkh    (* aspect octave)
        bkh (* (/ 2 3) wkh)
        
        ;; top widths of each white key
        twC 525
        twD bkw
        twE twC
        twF 455
        twG bkw
        twA twG
        twB twF]
    [:div {:style {:width  "300px"}}
     [:svg {:viewBox (str " 0 0 "  octave " " wkh)}
      ;; white keys
      [:g {:fill       "#eee"
           :stroke "none"}
       ;; C
       [:g
        [:path {:fill "#bbb"
                :d (str "m 0 " wkh
                        "v" (- wkh)
                        "h" twC
                        "v" bkh
                        "h" (- wkw twC)
                        "v" (- wkh bkh)
                        "h" (- wkw))}]
        [:text {:fill "#000"
                :stroke "none"
                :font-size (str 800 "px")
                :text-anchor "middle"
                :alignment-baseline "central"
                :x (* 0.5 wkw)
                :y (- wkh (* 0.5 (- wkh bkh)))
                }
         "C4"]]
       [:text {:fill "#fff"
               :stroke "none"
               :font-size (str 400 "px")
               :text-anchor "middle"
               :alignment-baseline "central"
               :x (+ twC (* 0.5 bkw))
               :y (* 0.5 bkh)}
        "C♯"]
       ;; D
       [:path {:d (let [cutout (* 0.5 (- wkw bkw))]
                    (str "m "  wkw " " wkh
                         "v" (- bkh wkh)
                         "h" cutout
                         "v" (- bkh)
                         "h" twD
                         "v" bkh
                         "h" cutout
                         "v" (- wkh bkh)
                         "h" (- wkw)))}]
       ;; E
       [:path {:d (let [cutout (* 0.5 (- wkw bkw))]
                    (str "m "  (* 2 wkw) " " wkh
                         "v" (- bkh wkh)
                         "h" (- wkw twE)
                         "v" (- bkh)
                         "h" twE
                         "v" wkh
                         "h" (- wkw)))}]
       ;; F
       [:path {:d (str "m " (* 3 wkw) " " wkh
                       "v" (- wkh)
                       "h" twF
                       "v" bkh
                       "h" (- wkw twF) 
                       "v" (- wkh bkh)
                       "h" (- wkw))}]
       ;; G
       [:path {:d (let [cutout 105]
                    (str "m " (* 4 wkw) " " wkh
                         "v" (- bkh wkh)
                         "h" cutout
                         "v" (- bkh )
                         "h" twG
                         "v" bkh
                         "h" (- wkw twG cutout)
                         "v" (- wkh bkh)
                         "h" (- wkw)))}]
       ;; A
       [:path {:d (let [cutout 105]
                    (str "m " (* 5 wkw) " " wkh
                         "v" (- bkh wkh)
                         "h" (- wkw twF cutout)
                         "v" (- bkh )
                         "h" twF
                         "v" bkh
                         "h" cutout
                         "v" (- wkh bkh)
                         "h" (- wkw)))}]
       ;; B
       [:path {:d (str "m " (* 6 wkw) " " wkh
                       "v" (- bkh wkh)
                       "h" (- wkw twB )
                       "v" (- bkh)
                       "h" twB
                       "v" wkh
                       "h" (- wkw))}]]]]))

(rum/defcs main < rum/reactive (rum/local 0.85 ::slider) [{::keys [slider]}]
  [:div
   (piano)
   #_(wave)
   (thing)])


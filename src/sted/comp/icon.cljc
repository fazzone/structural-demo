(ns comp.icon
  (:require [rum.core :as rum]))

(rum/defc favicon
  [size]
  (let [half    (* 0.5 size)
        quarter (* 0.5 half)
        eighth  (* 0.5 quarter)
        fr16    (* 0.5 eighth)
        st      (* 0.045 size)
        inset   (* 0.05 size)
        rh (- half eighth fr16)
        rw (- #_#_size quarter
              (* size 0.78)
              inset inset)
        ds        (* 1.5 st)
        [rdx rdy] [(+ inset (* 0.75 rw))
                   (+ inset (* 0.5 rh))]
        [ldx ldy] [(+ inset (* 0.25 rw))
                   (+ inset (* 0.5 rh))]
        alen        (* 0.43 size)
        [lapx lapy] [ldx (+ ldy alen quarter)]
        [rapx rapy] [(- size inset) rdy]
        color       "#fff" #_"#ae81ff"
        fs          (* size 0.65)
        lsp         (* 0.6 eighth)]
    [:svg {:viewBox (str "0 0 " size " " size)
           :style   {:width "16px"
                     :border "1px solid aliceblue"}}
     [:g {:stroke color :stroke-width st}
      [:rect {:x inset :y inset :width rw :height rh}]
      [:line {:x1 (+ inset (* 0.5 rw))
              :y1 inset
              :x2 (+ inset (* 0.5 rw))
              :y2 (+ rh inset)}]
      [:circle {:r      ds
                :fill   color
                :stroke "none"
                :cx     ldx
                :cy     ldy}]
      [:circle {:r      ds
                :fill   color
                :stroke "none"
                :cx     rdx
                :cy     rdy}]
      [:line {:x1 rdx :y1 rdy :x2 rapx :y2 rapy}]
      [:line {:x1 ldx :y1 ldy :x2 lapx :y2 lapy}]
      [:path {:fill "none"
              :d    (str "M" (- lapx eighth) " " (- lapy eighth)
                         "L" lapx " " lapy
                         "L" (+ lapx eighth) " " (- lapy eighth))}]
      [:path {:fill "none"
              :d    (str "M" (- rapx eighth) " " (- rapy eighth)
                         "L" rapx " " rapy
                         "L" (- rapx eighth) " " (+ rapy eighth))}]
      [:g {:transform (str "translate("
                             (+ inset (* 0.5 rw)) ","
                             (* size 0.65) ") "
                             "scale(0.5,0.5)")}
         [:text {:font-size      fs
                 :fill           color
                 :letter-spacing (+ lsp)
                 :font-family    "Iosevka Term SS03"
                 :stroke-width   (* st 0.5)} "st"]
         [:text {:y              (+ (* 0.8 fs) lsp)
                 :font-size      fs
                 :letter-spacing lsp
                 :font-family    "Iosevka Term SS03"
                 :fill           color
                 :stroke-width   (* st 0.5)} "ed"]]]]))
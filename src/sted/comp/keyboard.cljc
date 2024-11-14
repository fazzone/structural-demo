(ns sted.comp.keyboard
  (:require
   [rum.core :as rum]
   [datascript.core :as d]
   [sted.comp.icon :as icon]))

(rum/defc kkc-next
  [keycap {:keys [label symbol comp]}]
  [:div {:class ["key"
                 (when (= 1 (count keycap))
                   " single-width")]}
   (comment
     ;; alignment
     [:div {:style {:position :relative :top "0.5ex" :left "1.5ex"}}
      [:div {:style {:position :absolute :width "4ex" :height "2.5ex" :border "1px solid #333"}}]
      [:div {:style {:position :absolute :margin-top "1.25ex" :width "4ex" :border-top "1px dotted #333"}}]
      [:div {:style {:position :absolute :margin-left "2ex" :height "2.5ex" :border-left "1px dotted #333"}}]])
     
   [:div.keycap-parent {} [:div.keycap {} keycap]]
   (when symbol [:div.key-symbol-parent {} [:div.key-symbol {} symbol]])
   (when comp [:div.key-symbol-parent {} [:div.key-symbol {} ^:inline comp]])
   (when label [:div.key-label {} label])])

(rum/defc show-modifiers < rum/reactive
  [db]
  (let [mods (some-> (d/entity db :sted.page/state)
                     :sted.sys.keyboard/mods
                     (rum/react))]
    [:span "Modifiers: " [:code (pr-str mods)]]))

(def shift-map
  (zipmap (seq (str " 1234567890-=" "qwertyuiop[]\\" "asdfghjkl;'"  "zxcvbnm,./"))
          (seq (str " !@#$%^&*()_+" "QWERTYUIOP{}|"  "ASDFGHJKL:\"" "ZXCVBNM<>?"))))

(rum/defc keyboard-diagram
  [kme bus classes]
  (let [k->l (into {}
                   (for [{:key/keys [kbd mutation]} (:keymap/bindings kme)]
                     #_[kbd (get mutation->label mutation)]
                     [kbd (get icon/icon-map mutation)]))]
    (letfn [(key
              ([ch] (key ch nil))
              ([ch lch] (kkc-next ch (k->l (or lch ch)))))
            (skey
               ([ch] (skey ch nil))
               ([ch lch]
                (kkc-next (shift-map ch)
                          (k->l (str "S-" (shift-map (or lch ch)))))))]
     
     [:div {:class classes}
      (show-modifiers (d/entity-db kme))
      
      [:div.keyboard-container 
       [:div.keyboard-row.number
        (key "Esc" "Escape")
        (for [ch (vec "1234567890-=")]
          (rum/with-key (key ch) ch))
        (key "Backspace")]
       [:div.keyboard-row.qwer
        (key "Tab")
        (for [ch "qwertyuiop[]\\"]
          (rum/with-key (key ch) ch))]
       [:div.keyboard-row.asdf
        (key "Control")
        (for [ch "asdfghjkl;'"]
          (rum/with-key (key ch) ch))
        (key "Enter")]
       [:div.keyboard-row.zxcv
        (key "Shift")
        (for [ch "zxcvbnm,./"]
          (rum/with-key (key ch) ch))
        (key "Shift")]
       [:div.keyboard-row.space
        (key "Hyper")
        (key "Super")
        (key "Meta")
        (key " ")
        (key "Meta")
        (key "Super")
        (key "Hyper")
        (key "Control")]]
     
      [:br]
     
      ;; SHIFT
      [:div.keyboard-container 
       [:div.keyboard-row.number
        (key "Esc" "Escape")
        (for [ch (vec "1234567890-=")]
          (rum/with-key (skey ch) ch))
        (key "Backspace")]
       [:div.keyboard-row.qwer
        (key "Tab")
        (for [ch "qwertyuiop[]\\"]
          (rum/with-key (skey ch) ch))]
       [:div.keyboard-row.asdf
        (key "Control")
        (for [ch "asdfghjkl;'"]
          (rum/with-key (skey ch) ch))
        (key "Enter")]
       [:div.keyboard-row.zxcv
        (key "Shift")
        (for [ch "zxcvbnm,./"]
          (rum/with-key (skey ch) ch))
        (key "Shift")]
       [:div.keyboard-row.space
        (key "Hyper")
        (key "Super")
        (key "Meta")
        (key " ")
        (key "Meta")
        (key "Super")
        (key "Hyper")
        (key "Control")]]])))



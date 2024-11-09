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

(def mutation->label
  {:flow-right {:label "flow->" :symbol
                [:div {:style {:margin-top "0.65ex"
                               :margin-left "0.15ex"}}
                 "==>"]}
   :flow-left  {:label "<-flow" :symbol
                [:div {:style {:transform "scale(-1,1)"
                               :margin-top "0.65ex"
                               :margin-left "0.15ex"}}
                 "==>"]}
   :raise      {:label  "raise"
                :symbol [:div {:style {:transform   "rotate(225deg)"
                                       :margin-top  "0.5ex"}} "->>"]}
   :float      {:label  "float"
                :symbol [:div {:style {:margin-top  "0.5ex"
                                       ;; :margin-left "0.15ex"
                                       :transform   "rotate(-90deg)"}}
                         "->>"]}
   :sink       {:label  "sink"
                :symbol [:div {:style {:margin-top  "0.5ex"
                                       ;; :margin-left "0.15ex"
                                       :margin-left "-0.5ex"
                                       :transform   "rotate(90deg)"}}
                         "->>"]}
   :eval   {:label  "eval"
            :symbol [:div {:style {:font-size   "135%"
                                   :margin-left "0.5ex"
                                   :margin-top "0.175ex"}}
                     "λ"]}
   :hide {:label [:span {:style {:color "tomato"}} "hide"]}
   :compose {:label  "comp"
             :symbol [:div {:style {:font-size "150%" :margin-left "0.55ex"}}
                      "◯"]}
   
   :offer {:label  "offer"
           :symbol [:div {:style {:font-size "120%"
                                  :margin-left "0.8ex"
                                  :margin-top "0.3ex"}}
                    "a\u0302"]}

   :select-chain {:label "chain"}
   :m1 {:label "top"
        :symbol [:div {:style {:transform "rotate(225deg)"
                               :margin-top "0.5ex"}}
                 
                 "==>"]}
   :slurp-right {:label "push)→"
                 :symbol [:div {:style {:margin-top "0.65ex"
                                        :margin-left "0.7ex"}}
                          ")→"]
                 }
   :barf-right  {:label [:span {:style {:color "tomato"}} "←)pull"]}
   :gobble {:label "gobble"}
   :new-comment {:label "doc"}

   :pop-deleted-chain {:label "yank"}
   :new-quote {:label "quote"}
   :tear {:label "-tear-"
          :symbol [:div {:style {:font-size "130%"
                                 :margin-top "0.15ex"
                                 :margin-left "-0.15ex"}}
                   "«⋯»"]}
   :hop-left  {:label "<-hop"
               :symbol [:div {:style {:font-size "160%"
                                      :margin-left "0.25ex"}}
                        "\u25c1"]}
   :hop-right {:label "hop->"
               :symbol [:div {:style {:font-size "160%"
                                      :margin-left "0.25ex"}}
                        "\u25b7"]}
   :find-next {:label "look"}
   :undo {:label "undo"
          :symbol [:div {:style {:font-size "160%"
                                 :margin-left "0.4ex"}}
                   "\u21b6"]}
   :insert-left  {:label "insert"
                  :symbol [:div {:style {:font-size "120%"}}
                           "\u24d0<-"
                           ]}
   :insert-right {:label "->insert"}
   :wrap         {:label "(wrap)"
                  :symbol [:div {:style {:margin-top "0.65ex"
                                         :margin-left "0.15ex"}}
                           "(+)"]}
   :new-vec      {:label "new []" :symbol
                  [:div {:style {:margin-top "0.65ex"
                                 :margin-left "0.15ex"}}
                   "+[]"]}


   :delete-right {:label  "del->"
                  :symbol [:div {:style {:font-size   "120%"
                                         :margin-left "0.15ex"
                                         :margin-top  "0.25ex"}}
                           "⌦"]}
   :delete-left  {:label "<-delete"}
   
   :scroll {:label "view"
            :symbol (let [halfw 16
                          w (* 2 halfw)
                          halfh 10
                          h (* 2 halfh)
                          inset 2
                          ;; control vertical extent
                          ch 12
                          ;; control pointiness
                          cw 7]
                      [:svg {:width "1.6em"
                             :viewBox (str (- (+ halfw inset)) " " (- halfh) " " (+ w inset inset) " " h)
                             :stroke "#fff"
                             :fill "none"
                             :stroke-width "2"
                             :style {:margin-top "1ex"}}
                       [:circle {:x 0 :y 0 :r (* 0.3 halfw)}]
                       [:path {:d (str "M" (- halfw) " " 0
                                       " C" (- cw) " " (- ch) " " cw " " (- ch) " " halfw " " 0
                                       " C" cw " " ch " " (- cw) " " ch " " (- halfw) " " 0
                                       " Z")}]])}
   :search/dom {:label "goto"
                :symbol [:div {:style {:font-size "140%"
                                       :margin-left "0.4ex"}}
                         "\u00b6"]}
   
   :back-to-edit {:label "back"
                  :symbol [:div {:style {:font-size "160%"
                                         :margin-left "0.4ex"}}
                           "\u21a9"]}
   
   :parent {:label  "parent"
            :symbol [:div {:style {:transform "rotate(225deg)"
                                   :font-size "130%"
                                   ;; :margin-top "0.25ex"
                                   ;; :margin-left "0.15ex"
                                   }} "->"]}
   :next   {:label  "next"
            :symbol [:div {:style {:font-size "130%"}} "->"]}
   :prev   {:label  "prev"
            :symbol [:div {:style {:font-size "130%"}} "<-"]}
   
   ;; :next   {:label  "next"
   ;;          :symbol [:div {:style {:transform "rotate(90deg)"
   ;;                                 :font-size "130%"}} "->"]}
   ;; :prev   {:label  "prev"
   ;;          :symbol [:div {:style {:transform "rotate(-90deg)"
   ;;                                 :font-size "130%"}} "->"]}
   :tail   {:label "tail"
            :symbol [:div {:style {:transform "rotate(45deg)"
                                   :margin-top "0.5ex"
                                   :margin-left "-0.3ex"
                                   }} "==>"]}
   :clone     {:label "clone"
               :symbol [:div {:style {:margin-top "0.25ex"
                                      :margin-left "0.25ex"
                                      :font-size "130%"}}
                        "++"]}

   :new-do {:label "cmd"
            :symbol [:div {:style {:font-size "130%"
                                   :margin-top "0.4ex"
                                   :margin-left "0.4ex"
                                   }} "$"]
            
            }
   :linebreak {:label "linebreak"}
   
   :oneline {:label "squish"}
   :alias {:label "alias"}
   :hoist {:label "hoist"}
   :multiline {:label "splay"}
   :stringify {:label "pr-str"}
   :plus {:label "plus"}
   :new-bar {:label "bar"}
   :zp {:label "format"}
   :find-first {:label "first"}
   :unraise {:label "unraise"}
   :split {:label "split"}
   :save {:label "write"}
   :drag-right {:label "drag->"}
   :drag-left {:label "<-drag"}
   :cycle-deleted-chain {:label "cycle"}
   :uneval {:label "uneval"}
   :new-meta {:label "meta"}
   :new-deref {:label "deref"}
   :new-list {:label "new ()"}
   })

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



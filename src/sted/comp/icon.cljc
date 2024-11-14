(ns sted.comp.icon
  (:require [rum.core :as rum])
  #?(:cljs (:require-macros [sted.comp.icon :refer [deficons]])))

(rum/defc eye
  []
  (let [halfw 16
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
                     " Z")}]]))

#?(:clj
   (defmacro deficons
     [& body]
     `(do
        ;; make defc for each icon with hiccup def
        ~@(for [[mut _ icon-comp] (partition 3 body)
                :when (vector? icon-comp)]
            `(rum/defc ~(symbol (str "icon2" mut)) ; ~'< rum/static
               []
               ~icon-comp))

        (def icon-map
          (hash-map
           ~@(apply concat
                    (for [[mut label icon-comp] (partition 3 body)]
                      (list mut {:label label
                                 ;; invoke ourselves
                                 :comp (if-not (vector? icon-comp)
                                         icon-comp
                                         (list (symbol (str "icon2" mut))))}))))))))

(rum/defc t
  [txt mtop mleft xform fsize]
  [:div {:style {:margin-top mtop
                 :margin-left mleft
                 :transform xform
                 :font-size fsize}}
   (str txt)])

#_(deficons
  
  :flow-right "flow->" (t "==>" "0.65ex" "0.15ex")
  :flow-left  "<-flow" (t "<==" "0.65ex" "0.15ex")
  :raise "raise" (t "->>" "0.5ex" 0 "rotate(225deg)")
  :float "float" (t "->>" "0.5ex" 0 "rotate(-90deg)")
  :sink "sink" (t "->>" "0.5ex" "-0.5ex" 0 "rotate(90deg)")
  :compose "comp" (t "\u25ef" nil "0.55ex" nil "150%")
  
  )



(deficons
  :flow-right          "flow->"       (t "==>" "0.65ex" "0.15ex" nil nil)
  :flow-left           "<-flow"       (t "<==" "0.65ex" "0.15ex" nil nil)
  :raise               "raise"        (t "->>" "0.5ex" "0" "rotate(225deg)" nil)
  :float               "float"        (t "->>" "0.5ex" "0" "rotate(-90deg)" nil)
  :sink                "sink"         (t "->>" "0.5ex" "-0.5ex" "rotate(90deg)" nil)
  :compose             "comp"         (t "\u25ef" nil "0.55ex" nil "150%")
  :eval                "eval"         (t "λ" "0.175ex" "0.5ex" nil "135%")
  :hide                "hide"         nil
  :offer               "offer"        (t "a\u0302" "0.3ex" "0.8ex" nil "120%")
  :select-chain        "chain"        nil
  :m1                  "top"          (t "==>" "0.5ex" "0" "rotate(225deg)" nil)
  :slurp-right         "push)→"       (t ")→" "0.65ex" "0.7ex" nil nil)
  :barf-right          "←)pull"       nil
  :gobble              "gobble"       (t "gobble" nil nil nil nil)
  :new-comment         "doc"          nil
  :pop-deleted-chain   "yank"         nil
  :new-quote           "quote"        nil
  :tear                "-tear-"       (t "\u00ab\u22ef\u00bb" "0.15ex" "-0.15ex" nil "130%")
  :hop-left            "<-hop"        (t "\u25c1" nil "0.25ex" nil "160%")
  :hop-right           "hop->"        (t "\u25b7" nil "0.25ex" nil "160%")
  :find-next           "look"         nil
  :undo                "undo"         (t "\u21b6" nil "0.4ex" nil "160%")
  :insert-left         "insert"       (t "\u24d0<-" nil nil nil "120%")
  :insert-right        "->insert"     nil
  :wrap                "(wrap)"       (t "(+)" "0.65ex" "0.15ex" nil nil)
  :new-vec             "new []"       (t "+[]" "0.65ex" "0.15ex" nil nil)
  :delete-right        "del->"        (t "x>>" #_"\u2326" "0.25ex" "0.15ex" nil "120%")
  :delete-left         "<-delete"     (t "<-delete" nil nil nil nil)
  :scroll              "view"         nil
  :search/dom          "goto"         (t "\u00b6" nil "0.4ex" nil "140%")
  :back-to-edit        "back"         (t "\u21a9" nil "0.4ex" nil "160%")
  :parent              "parent"       (t "->" nil nil "rotate(225deg)" "130%")
  :next                "next"         (t "->" nil nil nil "130%")
  :prev                "prev"         (t "<-" nil nil nil "130%")
  :tail                "tail"         (t "==>" "0.5ex" "-0.3ex" "rotate(45deg)" nil)
  :clone               "clone"        (t "++" "0.25ex" "0.25ex" nil "130%")
  :new-do              "cmd"          (t "$" "0.4ex" "0.4ex" nil "130%")
  :linebreak           "linebreak"    nil
  :oneline             "squish"       nil
  :alias               "alias"        nil
  :hoist               "hoist"        nil
  :multiline           "splay"        nil
  :stringify           "pr-str"       nil
  :plus                "plus"         nil
  :new-bar             "bar"          nil
  :zp                  "format"       nil
  :find-first          "first"        nil
  :unraise             "enclose"      nil
  :split               "split"        nil
  :save                "write"        nil
  :drag-right          "drag->"       nil
  :drag-left           "<-drag"       nil
  :cycle-deleted-chain "cycle"        nil
  :uneval              "uneval"       nil
  :new-meta            "meta"         nil
  :new-deref           "deref"        nil
  :new-list            "new ()"       nil

  )


(comment
  
  :eval   {:label  "eval"
           :symbol [:div {:style {:font-size   "135%"
                                  :margin-left "0.5ex"
                                  :margin-top "0.175ex"}}
                    "λ"]}
  :hide {:label [:span {:style {:color "tomato"}} "hide"]}
  
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
  
  :scroll {:label "view" :symbol (eye)}
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
  )

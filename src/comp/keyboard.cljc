(ns comp.keyboard
  (:require [rum.core :as rum]))

(rum/defc keyboard-key-component
  [k->l keycap]
  [:div {:class ["key"
                 (when (= 1 (count keycap))
                     " single-width")]} 
   [:div.keycap keycap]
   (when-let [legend (k->l keycap)]
     (if-not (string? legend)
       legend
       [:div.key-legend legend]))])

(rum/defc kkc
  [lookup keycap]
  [:div {:class ["key"
                 (when (= 1 (count keycap))
                   " single-width")]} 

   [:div.keycap-parent [:div.keycap keycap]]
   
   (when-let [symbol (:symbol (lookup keycap )) ]
     [:div.key-symbol-parent [:div.key-symbol symbol]])

   (when-let [label (:label (lookup keycap )) ]
     [:div.key-label label])])

(defn thread-first
  [sel]
  (loop [[func arg & more] sel
         acc               []]
    (println 'func func 'arg arg 'more more 'acc acc)
    (cond
      (nil? func) (list* '-> acc)
      (list? arg) (recur arg (-> func
                                 (cond-> more (list* more))
                                 (cons acc)))
      :else       (list* '-> arg func acc))))

(def defaultkl
  {
   "f" {:label "flow->" :symbol [:span.symbol1 "==>"] }
   "a"{:label "<-flow" :symbol [:span.symbol1 "<=="] }

   "r" {:label "raise"
        :symbol [:div {:style {:transform "rotate(225deg)"
                               :margin-top "0.5ex"
                               :margin-left "-0.5ex"
                               }} "=>>"]}
   "w" {:label "float"
        :symbol [:div {:style {:margin-top "0.5ex"
                               :margin-left "-0.5ex"
                               :transform "rotate(-90deg)"}}
                 "=>>"]}
   "s" {:label "sink"
        :symbol [:div {:style {:margin-top "0.5ex"
                               :margin-left "-0.5ex"
                               :transform "rotate(90deg)"}} "=>>"]}
   "e" {:label [:span {:style {:color "tomato"}} "eval"]
        :symbol [:div {:style {:font-size "220%"
                               :margin-top "-1.175ex"
                               :margin-left "0ex"
                               }}
                 "\u2026"]}
   "y" {:label [:span {:style {:color "tomato"}} ""]
        :symbol [:div {:style {:font-size "140%" :margin-left "0.3ex"}} "λ"]}
   "u" {:label [:span {:style {:color "tomato"}} "undo"]
        :symbol [:div {:style {:font-size "160%"
                               :margin-top "-0.4ex"
                               :margin-left "-0.0ex"
                               }}
                 "\u238c"]
        }
   ;; append is "n Space"? - kinda, last vs. tail pos
   ;; "o" {:label [:span {:style {:color "tomato"}} "append"]}
   "t" {:label [:span {:style {:color "tomato"}} "thread"]}
   "=" {:label [:span {:style {:color "tomato"}} "rename"]}
   "-" {:label [:span {:style {:color "tomato"}} "hide"]}
   
   ;; chainable, call more functions on it
   ;; needs to specify movement/up after  edit complete
   "q" {:label [:span {:style {:color "tomato"}} "comp"]
        :symbol [:div {:style {:font-size "120%"
                               :margin-left "0.2ex"
                               }}
                 "\u25ef"]}
   
   "m" {:label [:span {:style {:color "tomato"}} "modify"]}
   "n" {:label [:span {:style {:color "tomato"}} "next"]}
   "b" {:label [:span {:style {:color "tomato"}} "bind"]}
   "p" {:label [:span {:style {:color "tomato"}} "push)→"]}
   
   
   "z" {:label "<-hop"}
   "x" {:label "hop->"}
   
   
   "/" {:label [:span {:style {:color "tomato"}} "search"]}
   "," {:label [:span {:style {:color "tomato"}} "prefix"]}

   
   "g" {:label [:span {:style {:color "tomato"}} "goto"]}
   ";" {:label [:span {:style {:color "tomato"}} "doc"]}


   " " {:label "insert"}
   "0" {:label "parent"}
   "9" {:label "(wrap)"}
   "[" {:label "new []"}
   "]" {:label "parent"}

   "d" {:label "delete"
        :symbol [:div {:style {:font-size "120%"
                               ;; :font-size "210%"
                               :margin-left "0.0ex"
                               :margin-top "-0.2ex"
                               }}
                 "\u2326"
                 #_
                 "×"]}
   "v" {:label "view"}
   
   "h" {:label "parent"
        :symbol [:div.symbol2 {:style {:transform "rotate(225deg)"}} "->"]}
   "j" {:label "next"
        :symbol [:div.symbol2 {:style {:transform "rotate(90deg)"}} "->"]}
   "k" {:label "prev"
        :symbol [:div.symbol2 {:style {:transform "rotate(-90deg)"}} "->"] }
   "l" { :label "tail"
        :symbol [:div.symbol2 {:style {:transform "rotate(45deg)"}} "->"]}

   
   
   "i"         {:label "indent"}
   "c"         {:label "clone" :symbol [:div.symbol3 "++"]}
   "Enter"     {:label "linebreak"}
   "Backspace" {:label "←delete"}})

(rum/defc keyboard-diagram
  [ ]
  (let [k->l defaultkl
        key (partial kkc k->l)]
    [:div.keyboard-container
     [:div.number-row
      (key "Esc")
      (for [ch (vec "1234567890-=")]
        (rum/with-key (key ch) ch))
      (key "Backspace")]
     [:div.qwer-row
      (key "Tab")
      (for [ch "qwertyuiop[]\\"]
        (rum/with-key (key ch) ch))]
     [:div.asdf-row
      (key "Caps")
      (for [ch "asdfghjkl;'"]
        (rum/with-key (key ch) ch))
      (key "Enter")]
     [:div.zxcv-row
      (key "Shift")
      (for [ch "zxcvbnm,./"]
        (rum/with-key (key ch) ch))
      (key "Shift")]
     [:div.space-row
      (key "Ctrl")
      (key "Mod")
      (key "Alt")
      (key " ")
      (key "Alt")
      (key "Mod")
      (key "Menu")
      (key "Ctrl")]]))

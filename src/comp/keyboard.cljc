(ns comp.keyboard
  (:require
   [rum.core :as rum]
   [datascript.core :as d]))

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
  [lookup keycap lkey]
  (let [{:keys [label symbol]} (lookup (or lkey keycap))]
    [:div {:class ["key"
                   (when (= 1 (count keycap))
                     " single-width")]}
     [:div.keycap-parent [:div.keycap keycap]]
     (when symbol [:div.key-symbol-parent [:div.key-symbol symbol]])
     (when label [:div.key-label label])]))

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

(def mutation->label
  {
   :flow-right {:label "flow->" :symbol [:span.symbol1 "==>"]}
   :flow-left  {:label "<-flow" :symbol [:span.symbol1 "<=="]}
   :raise      {:label  "raise"
                :symbol [:div {:style {:transform   "rotate(225deg)"
                                       :margin-top  "0.5ex"
                                       :margin-left "-0.5ex"}} "=>>"]}
   :float      {:label  "float"
                :symbol [:div {:style {:margin-top  "0.5ex"
                                       :margin-left "-0.5ex"
                                       :transform   "rotate(-90deg)"}}
                         "=>>"]}
   :sink       {:label  "sink"
                :symbol [:div {:style {:margin-top  "0.5ex"
                                       :margin-left "-0.5ex"
                                       :transform   "rotate(90deg)"}} "=>>"]}
   :eval-sci   {:label  [:span {:style {:color "tomato"}} "eval"]
                :symbol [:div {:style {:font-size   "220%"
                                       :margin-top  "-1.175ex"
                                       :margin-left "0ex"}}
                         "…"]}
   :hide {:label [:span {:style {:color "tomato"}} "hide"]}
   :compose {:label  [:span {:style {:color "tomato"}} "comp"]
             :symbol [:div {:style {:font-size   "120%"
                                    :margin-left "0.2ex"}}
                      "◯"]}
   :select-chain {:label "chain"}
   :m1 {:label "top"}
   
   :slurp-right {:label "push)→"}
   :barf-right  {:label [:span {:style {:color "tomato"}} "←)pull"]}
   :gobble {:label "gobble"}
   :offer {:label "offer"}
   :new-comment {:label "doc"}
   :tear {:label "-tear-"
          #_#_:symbol
          [:div {:style {:font-size   "130%"
                         :margin-left "0.0ex"
                         :margin-top  "-0.2ex"}}
           "✂"]}
   :hop-left  {:label "<-hop"}
   :hop-right {:label "hop->"}
   :find-next {:label "next"}
   :undo {:label "undo"}
   :insert-left  {:label "ins<-"}
   :insert-right {:label "->ins"}
   :wrap         {:label "(wrap)"}
   :new-vec      {:label "new []"}
   :delete-right {:label  "del->"
                  :symbol [:div {:style {:font-size   "120%"
                                         :margin-left "0.0ex"
                                         :margin-top  "-0.2ex"}}
                           "⌦"]}
   :delete-left  {:label "<-del"}
   :scroll {:label "view"}
   :parent {:label  "parent"
            :symbol [:div.symbol2 {:style {:transform "rotate(225deg)"}} "->"]}
   :next   {:label  "next"
            :symbol [:div.symbol2 {:style {:transform "rotate(90deg)"}} "->"]}
   :prev   {:label  "prev"
            :symbol [:div.symbol2 {:style {:transform "rotate(-90deg)"}} "->"]}
   :tail   {:label "tail"
            :symbol [:div.symbol2 {:style {:transform "rotate(45deg)"}} "->"]}
   :clone     {:label "clone" :symbol [:div.symbol3 "++"]}
   :linebreak {:label "linebreak"}})

(def defaultkl
  {
   "f" {:label "flow->" :symbol [:span.symbol1 "==>"]}
   "a" {:label "<-flow" :symbol [:span.symbol1 "<=="]}
   "r" {:label "raise"
        :symbol [:div {:style {:transform "rotate(225deg)"
                               :margin-top "0.5ex"
                               :margin-left "-0.5ex"}} "=>>"]}
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
                               :margin-left "0ex"}}
                 "…"]}
   "y" {:label [:span {:style {:color "tomato"}} ""]
        :symbol [:div {:style {:font-size "140%" :margin-left "0.3ex"}} "λ"]}
   "u" {:label [:span {:style {:color "tomato"}} "undo"]
        :symbol [:div {:style {:font-size "160%"
                               :margin-top "-0.4ex"
                               :margin-left "-0.0ex"}}
                 "⎌"]}
   ;; append is "n Space"? - kinda, last vs. tail pos
   ;; "o" {:label [:span {:style {:color "tomato"}} "append"]}
   "t" {:label [:span {:style {:color "tomato"}} "thread"]}
   "=" {:label [:span {:style {:color "tomato"}} "rename"]}
   "-" {:label [:span {:style {:color "tomato"}} "hide"]}
   ;; chainable, call more functions on it
   ;; needs to specify movement/up after  edit complete
   "q" {:label [:span {:style {:color "tomato"}} "comp"]
        :symbol [:div {:style {:font-size "120%"
                               :margin-left "0.2ex"}}
                 "◯"]}
   "m" {:label [:span {:style {:color "tomato"}} "modify"]}
   "n" {:label [:span {:style {:color "tomato"}} "next"]}
   "b" {:label [:span {:style {:color "tomato"}} "bind"]}
   "p" {:label [:span {:style {:color "tomato"}} "push)→"]}
   "o" {:label [:span {:style {:color "tomato"}} "←)pull"]}
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
                               :margin-top "-0.2ex"}}
                 "⌦"
                 #_
                 "×"]}
   "v" {:label "view"}
   "h" {:label "parent"
        :symbol [:div.symbol2 {:style {:transform "rotate(225deg)"}} "->"]}
   "j" {:label "next"
        :symbol [:div.symbol2 {:style {:transform "rotate(90deg)"}} "->"]}
   "k" {:label "prev"
        :symbol [:div.symbol2 {:style {:transform "rotate(-90deg)"}} "->"]}
   "l" {:label "tail"
        :symbol [:div.symbol2 {:style {:transform "rotate(45deg)"}} "->"]}
   "i"         {:label "indent"}
   "c"         {:label "clone" :symbol [:div.symbol3 "++"]}
   "Enter"     {:label "linebreak"}
   "Backspace" {:label "←delete"}})

(rum/defc keyboard-diagram
  [kme]
  (let [k->l #_defaultkl
        (into {}
              (for [{:key/keys [kbd mutation]} (:keymap/bindings kme)]
                [kbd (get mutation->label mutation)]))
        key (partial kkc k->l)]
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
      (key "Caps")
      (for [ch "asdfghjkl;'"]
        (rum/with-key (key ch) ch))
      (key "Enter")]
     [:div.keyboard-row.zxcv
      (key "Shift")
      (for [ch "zxcvbnm,./"]
        (rum/with-key (key ch) ch))
      (key "Shift")]
     [:div.keyboard-row.space
      (key "Ctrl")
      (key "Mod")
      (key "Alt")
      (key " ")
      (key "Alt")
      (key "Mod")
      (key "Menu")
      (key "Ctrl")]]))

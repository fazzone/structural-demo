(ns comp.keyboard
  (:require [rum.core :as rum]))

(rum/defc keyboard-key-component
  [k->l keycap]
  [:div {:class ["key"
                 (when (= 1 (count keycap)) " single-width")]} 
   [:div.keycap keycap]
   (when-let [legend (k->l keycap)]
     (if-not (string? legend)
       legend
       [:div.key-legend legend]))])

(rum/defc kkc
  [lookup keycap]
  [:div {:class ["key"
                 (when (= 1 (count keycap)) " single-width")]} 
   [:div.keycap keycap]
   
   (when-let [symbol (:symbol (lookup keycap )) ]
     [:div.key-symbol symbol])
   (when-let [label (:label (lookup keycap )) ]
     [:div.key-label label])
   
   #_(when-let [legend (k->l keycap)]
       (if-not (string? legend)
         legend
         [:div.key-legend legend]))])

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

(run! (comp prn (juxt identity meta))
      (tree-seq
       seq? identity
       (thread-first '(a ^BC (b c) ^DE (d e)))))
;; => (-> c b (a (d e)))


(def defaultkl
  { ;; "f" "flow→"
   ;; "a" "←flow"
   "f" {:label "flow→" :symbol "↫" }
   "a" {:label "←flow" :symbol "↬"}
   "r" {:label "raise"}
   "w" {:label "float"}
   "s" {:label "sink"}
   "e" {:label [:span {:style {:color "tomato"}} "eval"]}
   "y" {:label [:span {:style {:color "tomato"}} "paste"]}
   "u" {:label [:span {:style {:color "tomato"}} "undo"]}
   ;; append is "n Space"? - kinda, last vs. tail pos
   ;; "o" {:label [:span {:style {:color "tomato"}} "append"]}
   "t" {:label [:span {:style {:color "tomato"}} "thread"]}
   "=" {:label [:span {:style {:color "tomato"}} "rename"]}
   "-" {:label [:span {:style {:color "tomato"}} "hide"]}
   
   ;; chainable, call more functions on it
   ;; needs to specify movement/up after  edit complete
   "q" {:label [:span {:style {:color "tomato"}} "comp"]}
   
   "m" {:label [:span {:style {:color "tomato"}} "modify"]}
   "n" {:label [:span {:style {:color "tomato"}} "next"]}
   "b" {:label [:span {:style {:color "tomato"}} "bind"]}
   "p" {:label [:span {:style {:color "tomato"}} "push)→"]}
   
   
   "/" {:label [:span {:style {:color "tomato"}} "search"]}
   "," {:label [:span {:style {:color "tomato"}} "prefix"]}

   
   "g" {:label [:span {:style {:color "tomato"}} "goto"]}
   ";" {:label [:span {:style {:color "tomato"}} "doc"]}
   "1" {:label [ :span {:style {:font-size "120%"}} "⤣1"]}

   " " {:label "insert"}
   "0" {:label "up"}
   "9" {:label "(wrap)"}
   "[" {:label "new []"}
   ;; "q" {:label "(wrap‸)"}
   ;; "p" {:label "push)→"}
   "]" {:label "up"}

   "d" {:label "delete"}
   "v" {:label "view"}
   
   ;; "h" "parent"
   ;; "j" "next"
   ;; "k" "prev"
   ;; "l" "tail"
   "h" {:label "parent" :symbol "🡬"}
   "j" {:label "next" :symbol "🡫"}
   "k" {:label "prev" :symbol "🡩"}
   "l" { :label "tail" :symbol "🡮"}
   
   
   "i"         {:label "indent" :symbol "⭾"}
   "c"         {:label "clone"}
   "Enter"     {:label "linebreak"}
   "Backspace" {:label "←delete"}})

(rum/defc keyboard-diagram
  [ ]
  (let [k->l defaultkl
        key (partial kkc k->l)]
    [:div.whatever
     [:div.keyboard-container
      [:div.number-row
       (for [ch (vec "`1234567890-=")]
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
       (key "Ctrl")]]]))

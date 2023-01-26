(ns sted.comp.keyboard
  (:require
   [rum.core :as rum]
   [datascript.core :as d]))

#_(rum/defc keyboard-key-component
  [k->l keycap]
  [:div {:class ["key"
                 (when (= 1 (count keycap))
                   " single-width")]}
   [:div {:style {:position :absolute
                  :height "100%"
                  :border-left "1px solid #fff"}}
    "What"
    ]
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
     (comment ;; alignment
       [:div {:style {:position :relative :top "0.5ex" :left "1.5ex"}}
        [:div {:style {:position :absolute :width "4ex" :height "2.5ex" :border "1px solid #333"}}]
        [:div {:style {:position :absolute :margin-top "1.25ex" :width "4ex" :border-top "1px dotted #333"}}]
        [:div {:style {:position :absolute :margin-left "2ex" :height "2.5ex" :border-left "1px dotted #333"}}]])
     
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
   :flow-right {:label "flow->" :symbol
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
   :eval-sci   {:label  "eval"
                :symbol [:div {:style {:font-size   "135%"
                                       :margin-left "0.25ex"
                                       :margin-top "0.175ex"}}
                         "λ>"]}
   :hide {:label [:span {:style {:color "tomato"}} "hide"]}
   :compose {:label  "comp"
             :symbol [:div {:style {:font-size "150%" :margin-left "0.55ex"}}
                      "◯"]}
   :select-chain {:label "chain"}
   :m1 {:label "top"
        :symbol [:div {:style {:transform "rotate(225deg)"
                               :margin-top "0.5ex"}}
                 
                 "==>"]}
   :slurp-right {:label "push)→"}
   :barf-right  {:label [:span {:style {:color "tomato"}} "←)pull"]}
   :gobble {:label "gobble"}
   :offer {:label "offer"}
   :new-comment {:label "doc"}
   :tear {:label "-tear-"
          :symbol [:div {:style {:font-size "130%"
                                 :margin-top "0.15ex"
                                 :margin-left "-0.15ex"}}
                   "«⋯»"]}
   :hop-left  {:label "<-hop"
               :symbol [:div {:style {:font-size "130%"
                                      :margin-top "0.15ex"
                                      :margin-left "-0.15ex"}}
                        "<|"]}
   :hop-right {:label "hop->"
               :symbol [:div {:style {:font-size "130%"
                                      :margin-top "0.15ex"
                                      :margin-left "-0.15ex"}}
                        "|>"]}
   :find-next {:label "look"}
   :undo {:label "undo"}
   :insert-left  {:label "insert"}
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
   :scroll {:label "view"}
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
   :linebreak {:label "linebreak"}})

(rum/defc binker < rum/reactive
  [db]
  (let [mods (some-> (d/entity db :sted.page/state)
                     :sted.sys.keyboard/mods
                     (rum/react))]
    (println "Binker")
    [:code [:pre (pr-str mods)]]))

(rum/defc keyboard-diagram
  [kme bus classes]
  (let [k->l #_defaultkl
        (into {}
              (for [{:key/keys [kbd mutation]} (:keymap/bindings kme)]
                [kbd (get mutation->label mutation)]))
        key (partial kkc k->l)]
    (println "New keyboard diagram?")
    [:div
     #_[:code [:pre (pr-str mods)]]
     (binker (d/entity-db kme))
     [:div.keyboard-container {:class classes}
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
       (key "Control")]]]))

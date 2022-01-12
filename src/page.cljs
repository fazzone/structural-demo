(ns page
  (:require
   [clojure.edn :as edn]
   [embed :as e]
   [schema :as s]
   [debug :as debug]
   [tx-history :as h]
   [goog.string :as gstring]
   [goog.functions :as gf]
   [datascript.core :as d]
   [clojure.datafy :as datafy]
   [clojure.string :as string]
   [rum.core :as rum]
   [cljs.core.async :as async]
   [sci.core :as sci]
   
   [db-reactive :as dbrx]
   [comp.cons :as cc]
   [comp.edit-box :as eb]
   [comp.keyboard :as ck]

   [cmd.move :as move]
   [cmd.nav :as nav]
   [cmd.insert :as insert]
   [cmd.edit :as edit]
   [cmd.mut :as mut]
   [cmd.invar :as invar]
   [df.github :as dfg]
   [df.async :as a]
   
   [core :as core
    :refer [get-selected-form
            move-selection-tx]])
  (:import
   [goog.net XhrIo]
   [goog.net EventType])
  
  (:require-macros
   [cljs.core.async.macros :refer [go
                                   go-loop]]
   [macros :as m]))

(def load-time (js/Date.now))

(def test-form-data-bar
  '[["Chain 1"
     (def thing
       [1 (+ 2 3 ^:form/highlight foo  ) [:a :c] "ok"])
     
     (defn hn-test
       []
       (ingest (then (nav stories :topstories (:topstories stories))
                     (fn [x] (nav x x (first x))))))]])

(def default-keymap
  {"f"   :flow-right
   "S-F" :flow-right-coll
   "u"   :undo
   "S-A" :alias
   
   "S-Z" :drag-left
   "S-X" :drag-right
   
   "n"   :find-next
   "S-N" :find-first
   
   "C-/" :undo
   "S-R" :reify-undo
   
   "S-_" :uneval
   ;; "e"   :save
   "t"   :tear
   
   "S-@" :new-deref
   
   "a"         :flow-left
   "w"         :float
   "s"         :sink
   ;; "S-H"       :toplevel
   "h"         :parent
   "j"         :next
   "k"         :prev
   "l"         :tail
   "r"         :raise
   " "         :insert-right
   "S- "       :insert-left
   "d"         :delete-right
   "S-H"       :hoist
   "Backspace" :move-to-deleted-chain
   "Enter"     :linebreak
   "C-Enter"   :insert-right-newline
   "Escape"    :select-chain
   "c"         :clone
   "z"         :hop-left
   "x"         :hop-right
   "q"         :compose
   "9"         :wrap
   ;; "9"         :new-list
   "0"         :parent
   "]"         :parent
   "p"         :slurp-right
   "S-P"       :barf-right
   "Tab"       :indent
   "S-Tab"     :dedent
   "e"         :eval-sci
   "S-("       :new-list
   "["         :new-vec
   "S-C"       :new-chain
   "S-B"       :new-bar
   "1"         :m1
   "2"         :m2
   "3"         :m3
   "4"         :m4
   "5"         :m5
   "6"         :m6
   "7"         :m7
   "8"         :m8
   "v"         :scroll
   "-"         :hide
   "i"         :insert-left
   "S-Q"       :stringify
   "S-+"       :plus})

(def init-tx-data
  (let [chains (concat

                [(e/string->tx-all (m/macro-slurp  "src/core.cljc"))]
                (map e/->tx test-form-data-bar)
                
                #_[(e/string->tx-all (m/macro-slurp  "src/core.cljc"))]
                
                #_[(e/string->tx-all (m/macro-resource "clojure/core.clj"))]
                
                #_[(assoc (e/string->tx-all (m/macro-slurp  "src/embed.cljc"))
                          :chain/filename "zz-embed.cljc")]
                
                #_[(e/string->tx-all (m/macro-slurp  "subtree/clojure.core.clj"))]
                
                #_[(e/string->tx-all (m/macro-slurp  "src/cmd/edit.cljc"))]
                #_[(e/string->tx-all (m/macro-slurp  "src/cmd/mut.cljc"))]
                #_[(e/string->tx-all (m/macro-slurp  "src/page.cljs"))]
                
                #_[(e/string->tx-all (m/macro-slurp  "src/cmd/mut.cljc"))]
                )]
    [{:db/ident ::state
      :state/bar "bar"}
     {:db/ident ::evalchain
      :db/id "evalchain"
      :coll/type :vec
      :seq/first {:string/value "No more evals" :coll/_contains "evalchain"}}

     {:db/ident ::command-chain
      :db/id "command-chain"
      :coll/type :vec}
     {:db/ident ::inspect
      
      :db/id "inspect"
      :coll/type :inspect}
     {:db/ident ::default-keymap
      :db/id "defaultkeymap"
      :coll/type :keyboard
      :keymap/bindings (for [[k m] default-keymap]
                         {:key/kbd k :key/mutation m})}
     (assoc
      (e/seq-tx
       (concat
        (for [ch chains]
          (assoc ch
                 :coll/type :chain
                 :coll/_contains "bar"))
        
        [{:db/ident ::meta-chain
          :coll/type :chain
          :coll/_contains "bar"
          :coll/contains #{"label"
                           "defaultkeymap"
                           "inspect"
                           "evalchain"
                           "command-chain"}
          :seq/first {:db/id "label"
                      :string/value "Keyboard"}
          :seq/next {:seq/first "defaultkeymap"
                     :seq/next {:seq/first "evalchain"
                                :seq/next {:seq/first "command-chain"
                                           :seq/next {:seq/first "inspect"}}}}}]))
      :db/id "bar"
      :coll/type :bar)]))



;; replace with non-breaking hyphen, lmao
#_(-> text (gstring/replaceAll "-" "‑"))

(declare fcc)

(defn breadcrumbs-portal-id [eid] (str eid "bp"))
(defn modeline-portal-id [eid] (str eid "mp"))

(declare el-bfs)
(rum/defc top-level-form-component < dbrx/ereactive 
  [e bus p]
  [:div.form-card {:ref "top-level"}
   #_[:div.form-title.code-font {:style {:margin-bottom "4ex"}} (str "#" (:db/id e) " T+" (- (js/Date.now) load-time) "ms")]
   
   #_[:div.bp {:id (breadcrumbs-portal-id (:db/id e))}]
   
   [:div.top-level-form.code-font
    (fcc e bus 0 p)]
   [:div.modeline-outer {:id (modeline-portal-id (:db/id e))}]])

(defn computed-indent
  [e indent-prop]
  (+ indent-prop
     (or (:form/indent e)
         0)))

#_(defn do-indent
  [child linebreak? indent-level]
  (if-not linebreak?
    child
    (rum/fragment
     [:span.indent-chars "\n"
      [:span.indenter {:style {:margin-left (str indent-level "ch")}}]]
     child)))

(defn do-indent*
  [child ip fi linebreak?]
  #_(println "DI" fi linebreak?)
  (if (and (or (nil? fi) (zero? fi)) (not linebreak?))
    child
    (rum/fragment
       [:span.indent-chars (when linebreak? {:class "nl"})
        (when linebreak? "\n")
        #_[:span.indenter {:style {:padding-left (str ip "ch")
                                   :background-color "tomato"}}]
        (when fi
          #_[:span.indenter
           {:style {:color "cadetblue"} }
           (apply str (repeat fi "-"))]
          [:span.indenter {:style {:margin-left (str fi "ch")
                                   ;; :background-color "cadetblue"
                                   }}])]
       child)))

(defn coll-fragment
  [e indent-prop]
  (rum/fragment
   (case (:coll/type e) :list "(" :vec "[" :map "{")
   (for [x (e/seq->vec e)]
     (rum/with-key (fcc x (computed-indent e indent-prop)) (:db/id x)))
   (case (:coll/type e) :list ")" :vec "]" :map "}")))

(defmulti display-coll (fn [c bus indent classes]
                         (:coll/type c)))

(defmethod display-coll :default [c _ _ _]
  [:code (pr-str c)])

(def render-counter (atom 0))

(defn delimited-coll
  [open close e bus indent classes proply]
  #_(when proply (println "DC" e proply))
  [:span.c.dl
   (when classes {:class classes :ref "selected"})
   [:span.d
    
    #_[:span.inline-tag-outer [:span.inline-tag-inner (subs (str (:db/id e)) 0 1)]]
    
    #_(when-let [p (get proply (:db/id e))]
      [:span.inline-tag-outer
       [:span.inline-tag-inner
        (str p)]])
    
    #_[:span.inline-tag
     (str (swap! render-counter inc))
     #_(str (:db/id e))]
    
    open]
   (for [x (e/seq->vec e)]
     (-> (fcc x bus (computed-indent e indent) proply)
         (rum/with-key (:db/id x))))
   [:span.d.cl close]])




(defmethod display-coll :list [c b i s p] (delimited-coll  "(" ")" c b i s p))
(defmethod display-coll :vec  [c b i s p] (delimited-coll  "[" "]" c b i s p))
(defmethod display-coll :map  [c b i s p] (delimited-coll  "{" "}" c b i s p))
(defmethod display-coll :set  [c b i s p] (delimited-coll "#{" "}" c b i s p))
(defmethod display-coll :fn   [c b i s p] (delimited-coll "#(" ")" c b i s p))
(defmethod display-coll :tear [c b i s p] (delimited-coll  "«" "»" c b i s p))

(defmethod display-coll :hidden  [c b i s p]
  [:div {:style {:width "800px"}}
   (delimited-coll "SVG{ " " }SVG" c b i s p)
   (cc/svg-viewbox c b)
   #_(cc/svg-viewbox c core/blackhole)]
  #_[:span
   (cond-> {:class (str "c " s)}
     s (assoc :ref "selected"))
   (case (:hidden/coll-type c)
     :list "(\u00b7\u00b7\u00b7)"
     :vec "[...]"
     :map "{...}"
     :set "#{...}"
     "<...>")])



(defmethod display-coll :uneval  [c b i s p]
  [:span.c.unev
   (when s {:class s :ref "selected"})
   "#_"
   (for [x (e/seq->vec c)]
     (-> (fcc x b (computed-indent c i) p)
         (rum/with-key (:db/id x))))])

(defmethod display-coll :deref  [c b i s p]
  [:span.c.pf
   (when s {:class s :ref "selected"})
   [:span.pfc "@"]
   (for [x (e/seq->vec c)]
     (-> (fcc x b (computed-indent c i) p)
         (rum/with-key (:db/id x))))])

(defmethod display-coll :quote  [c b i s p]
  [:span.c.pf
   (when s {:class s :ref "selected"})
   [:span.pfc "'"]
   (for [x (e/seq->vec c)]
     (-> (fcc x b (computed-indent c i) p)
         (rum/with-key (:db/id x))))])

(defmethod display-coll :syntax-quote  [c b i s p]
  [:span.c.pf
   (when s {:class s :ref "selected"})
   [:span.pfc "`"]
   (for [x (e/seq->vec c)]
     (-> (fcc x b (computed-indent c i) p)
         (rum/with-key (:db/id x))))])

(defmethod display-coll :unquote  [c b i s p]
  [:span.c.pf
   (when s {:class s :ref "selected"})
   [:span.pfc "~"]
   (for [x (e/seq->vec c)]
     (-> (fcc x b (computed-indent c i) p)
         (rum/with-key (:db/id x))))])

(defmethod display-coll :unquote-splicing  [c b i s p]
  [:span.c.pf
   (when s {:class s :ref "selected"})
   [:span.pfc "~@"]
   (for [x (e/seq->vec c)]
     (-> (fcc x b (computed-indent c i) p)
         (rum/with-key (:db/id x))))])

(defmethod display-coll :reader-macro  [{:reader-macro/keys [dispatch] :as c} b i s p]
  [:span.c.pf
   (when s {:class s :ref "selected"})
   [:span.pfc (str "#" dispatch)]
   (for [x (e/seq->vec c)]
     (-> (fcc x b (computed-indent c i) p)
         (rum/with-key (:db/id x))))])

(declare snapshot)

(defn display-undo-preview
  [c b s top?]
  [:ul.undo-preview
   (when s {:class s :ref "selected"})
   (when top?
     [:span.prose-font "History"])
   (for [e (cond-> (e/seq->vec c) (not top?) reverse)]
     (rum/with-key
       (snapshot e b)
       (:db/id e)))])

(rum/defc snapshot < dbrx/ereactive
  [e bus]
  (let [tx (:number/value e)
        r  (core/get-history bus tx)]
    (if-not tx
      (display-undo-preview e bus (when (:form/highlight e) "selected") nil)
      [:li.undo-preview-entry
       (when (:form/highlight e) {:class "selected" :ref "selected"})
       (str tx " " (some-> r :mut pr-str))
       #_(apply str (interpose " " (map pr-str (:mut r))))
       (if-not (some (fn [[e a v t]] (not= a :form/highlight))
                     (:tx-data r))
         " movement only"
         (when r
           [:div.alternate-reality
            (-> (peek (nav/parents-vec (get-selected-form (:db-after r))))
                (fcc core/blackhole 0 nil))]))])))

(defmethod display-coll :undo-preview  [c b i s p]
  (display-undo-preview c b s true))

(defmethod display-coll :chain [chain bus i classes proply]
  [:div.chain.hide-scrollbar
   {:key (:db/id chain)
    :ref "chain"
    :id (str "c" (:db/id chain))
    :class classes}
   #_(str "Chain" (:db/id chain))
   (for [f (e/seq->vec chain)]
     (-> (top-level-form-component f bus proply)
         (rum/with-key (:db/id f))))])

(defmethod display-coll :bar [bar bus i c p]
  [:div.bar.hide-scrollbar
   (when c {:ref "selected"})
   #_(cond-> {:class c}
       c (assoc :ref "selected"))
   (for [chain-head (e/seq->vec bar)]
     (-> (fcc chain-head bus i p)
         (rum/with-key (:db/id chain-head))))])

(defmethod display-coll :keyboard [k bus i]
  "No keyboardn"
  [:div.display-keyboard
   (ck/keyboard-diagram
    k
    #_(d/entity (d/entity-db k) )
    )
   
   [:div {:style {:width "6ex"
                  :font-size "9pt"}}
    (ck/kkc {} "F")]])


(defmethod display-coll :alias [{:alias/keys [of] :as c} b i s]
  (println "DCA" c)
  [:div.alias
   (when s {:class s :ref "selected"})
   [:div.prose-font "Alias " (:db/id c) " of " (:db/id of)]
   (fcc of b i s)])

(defmethod display-coll :eval-result [c b i s]
  [:div.eval-result-outer
   [:span.eval-result
    (str "[" (:db/id c) "] " (:db/id (:eval/of c)) "=>")
    (fcc (:eval/result c) b 0)]])

(rum/defc inspector < (dbrx/areactive :form/highlight :form/edited-tx)
  [db bus]
  (let [sel (get-selected-form db)]
    [:div.inspector
     [:span.prose-font (str "Inspect #" (:db/id sel))]
     [:div
      (debug/datoms-table-eavt*
       (d/datoms (d/entity-db sel) :eavt  (:db/id sel))
       #_(concat
        (d/datoms (d/entity-db sel) :eavt  (:db/id sel))
        (->> (d/datoms (d/entity-db sel) :avet  )
             (filter (fn [[e a v t]]
                       (= v (:db/id sel)))))))]]))

(defmethod display-coll :inspect [k bus i]
  "No inspect"
  #_(inspector (d/entity-db k) bus))

(defn token-class
  [e]
  (or (when-let [s (:symbol/value e)]
        (case s
          ("defn" "let" "when" "and" "or" "if" "do" "for" "some->"
           "when-not" "if-not" "def" "cond" "case" "->" "->>" "some->>"
           "if-let" "when-let" "recur" "try" "catch" "nil") "m"
          ("first" "map" "filter" "apply" "reset!" "swap!"
           "get" "assoc" "update" "cons" "conj" "seq" "next"
           "prn" "println" "into" "set" "vector"
           "take" "drop" "take-while" "dorp-while" "reduce"
           "concat") "s"
          "v"))
      (when (:keyword/value e) "k")
      (when (:string/value e) "l")
      (when (:number/value e) "n")))

(defn token-text
  [e]
  (or
   (some-> e :symbol/value str)
   (when-let [k (:keyword/value e)]
     (let [is (string/index-of k "/")]
         (if (> 0 is)
           k
           (rum/fragment
            [:span.kn (subs k 0 is)]
            (subs k is)))))
   (some-> e :string/value pr-str)
   (some-> e :number/value str)))

(rum/defc fcc < dbrx/ereactive
  [e bus indent-prop proply]
  #_(when proply (prn "FccProply" (:db/id e) proply))
  (cond-> (or (when (:form/editing e)
                (eb/edit-box e bus))
              (when-let [tc (token-class e)]
                [:span (cond-> {:key (:db/id e)
                                :class (if (:form/highlight e)
                                         (str tc " tk selected")
                                         (str tc " tk"))
                                :on-click (fn [ev]
                                            (.stopPropagation ev)
                                            (core/send! bus [:select (:db/id e)]))}
                         (:form/highlight e) (assoc :ref "selected"))
                 (token-text e)])
              (when (:coll/type e)
                (display-coll e
                              bus
                              (+ 2 indent-prop)
                              (when (:form/highlight e) "selected")
                              proply
                              #_(if-not (:form/highlight e)
                                  proply
                                  (zipmap
                                   (map :db/id (next (mut/get-numeric-movement-vec e)))
                                   (range 2 9)))))
              (comment "Probably a retracted entity, do nothing"))
    
    (not (= :noindent proply))
    (do-indent*
     indent-prop
     (:form/indent e)
     (:form/linebreak e))))

#_(register-sub ::extract-to-new-top-level
              (->mutation
               (fn [db]
                 (let [sel (get-selected-form db)
                       top-level (first (reverse-parents-array sel))
                       chain (some-> top-level :coll/_contains first)
                       new-node (-> (form-duplicate-tx sel)
                                    (update :db/id #(or % "dup-leaf")))]
                   (into [new-node]
                         (concat (edit/insert-before-tx top-level new-node)
                                 (move-selection-tx (:db/id sel) (:db/id new-node))))))))

(defn el-bfs
  [top limit]
  (loop [out   []
         front [top]]
    (cond
      (empty? front) out
      (= limit (count out)) out
      :else (let [e (first front)]
              (recur (cond-> out
                       (:coll/type e) (conj e))
                     (cond-> (subvec front 1)
                       (:seq/next e) (conj (:seq/next e))
                       (:seq/first e) (conj (:seq/first e))))))))

(def breadcrumbs-max-numeric-label 8)
(rum/defc parent-path-item
  [bus i parent]
  [:li
   [:a {:href "#"
        :on-click #(do (.preventDefault %)
                       (core/send! bus [:select (:db/id parent)]))}
    [:span.code-font
     (when (< i breadcrumbs-max-numeric-label)
       [:span.inline-tag (str (inc i))])
     (if (= :list (:coll/type parent))
       (or (some-> parent :seq/first :symbol/value) "()")
       (case (:coll/type parent) :vec "[]" :map "{}" "??"))]]])

(rum/defc parent-path
  [rpa bus]
  [:ul.parent-path
   (for [i (range (count rpa))]
     (-> (parent-path-item bus i (nth rpa i))
         (rum/with-key i)))])

(rum/defc breadcrumbs-always < (dbrx/areactive :form/highlight :form/edited-tx)
  [db bus]
  (when-let [sel (get-selected-form db)]
    (let [rpa (some-> sel nav/parents-vec reverse vec)
          top-level (first rpa)]
      (when-let [n (->> (:db/id top-level)
                        (breadcrumbs-portal-id)
                        (.getElementById js/document))]
        (-> (if-not (= (:db/id sel) (:db/id top-level))
              (butlast rpa)
              (el-bfs sel 9))
            (parent-path bus)
            (rum/portal n))))))

(def special-key-map
  (merge
   {" "      [::insert-editing :after]
    "S-\""   [::insert-editing :after "\""]
    "S-:"    [::insert-editing :after ":"]
    "S- "    [::insert-editing :before]
    "'"      [::insert-editing :before "'"]
    "S-("    [::edit-new-wrapped :list]
    "M-S-("  [::wrap-and-edit-first :list]
    "q"      [::wrap-and-edit-first :list]
    "9"      [::wrap-selected-form :list]
    "["      [::edit-new-wrapped :vec ]
    "S-{"    [::edit-new-wrapped :map ]
    "r"      [::raise-selected-form]
    "S-X"    [::extract-to-new-top-level]
    "Escape" [::select-chain]
    "m"      [::edit-selected]
    "]"      [::move :move/up]
    "w"      [::exchange-with-previous]
    "s"      [::exchange-with-next]
    "0"      [::move :move/up]
    "v"      [::scroll-into-view]
    "h"      [::move :move/up]
    "j"      [::move :move/next-sibling]
    "k"      [::move :move/prev-sibling]
    "l"      [::move :move/most-nested]
    "f"      [::move :move/flow]
    "a"      [::move :move/back-flow]
    "n"      [::move :move/most-nested]
    "d"      [::delete-with-movement :move/forward-up]
    "e"      [::eval]
    
    "z"   [::hop-left]
    "x"   [::hop-right]
    "S-Z" [::drag-left] 
    
    "Backspace" [::delete-with-movement :move/backward-up]
    "c"         [::duplicate-selected-form]
    ;; "i"         [::indent-form 1]
    ;; "S-I"       [::indent-form -1]
    "Tab"       [::indent-form 1]
    "S-Tab"     [::indent-form -1]
    "i"         [::check-invariants] #_ [::reset-indent]
    "Enter"     [::linebreak-form]
    "M-p"       ["placeholder"]
    "M-n"       ["placeholder"]
    "-"         ["placeholder"]
    "S-Q"         ["placeholder"]

    "S-M"       [::recursively-set-indent true]
    "S-O"       [::recursively-set-indent false]
    ;; "M-x"       [::execute-selected-as-mutation]
    "S-A"       {"a" [::linebreak-form]}
    "S-R"       {"f" [::reify-extract-selected]
                 "m" [::reify-last-mutation]
                 "p" [::reify-parse-selected]}
    "C-z"       [::revert-last]}
   (into {}
         (for [i (range  breadcrumbs-max-numeric-label)]
           [(str (inc i)) [::select-1based-nth-reverse-parent (inc i)]]))))

(comment
  (let [a (f 1 2 3)]
    (when thing (q a)))
  
  (when thing
    (let [a (f 1 2 3)]
      (q a)))
  )

(defn search*
  [db sa text]
  ;; U+10FFFF
  (d/index-range db sa text (str text "\udbff\udfff")))

(defn stupid-symbol-search
  [db sa text]
  (println "Text" (count text) (pr-str text))
  (when (pos? (count text))
    [:div.search
     (for [[v [[e] :as ds]] (->> (search* db sa text)
                                 (group-by #(nth % 2))
                                 (sort-by (comp - count second))
                                 (take 5))
           k (range 3)]
       (case k
         0 (let [ent (d/entity db e)]
             [:span.tk {:key e :class (token-class ent)}
              (token-text ent)])
         1 [:span {:key (str "sr" e)} "x" (count ds)]
         2 [:span {:key (str "id" e)} (pr-str (take 5 (map first ds ))) ]))]))

(declare command-compose-feedback)
(declare save-status)
(rum/defc modeline-inner < dbrx/ereactive rum/reactive
  [sel bus {:keys [text valid] :as edn-parse-state}]
  [:span {:class (str "modeline code-font"
                      (if text " editing modeline-search" " modeline-fixed")
                      (when (and (not (empty? text)) (not valid)) " invalid"))}
   ""
   [:span.modeline-echo
    {}
    (let [{:keys [on at status file]} (rum/react save-status)]
      (when (= on (:db/id sel))
        (case status
          :saving "Saving"
          :ok (str file "@" at)
          :error "Error"
          "")))]
   
   (if text
     (stupid-symbol-search (d/entity-db sel) :symbol/value text)
     [:span.modeline-content
      (if-not sel
        "(no selection)"
        (str "#" (:db/id sel)
             " "
             #_(pr-str
                (apply max
                       (for [[_ a _ t] (d/datoms (d/entity-db sel) :eavt (:db/id sel))
                             :when (not= :form/highlight a)]
                         t)))
             (:coll/type sel)))
      (command-compose-feedback)])])

(rum/defc modeline-portal  < rum/reactive (dbrx/areactive :form/highlight :form/editing)
  [db bus]
  (let [sel (get-selected-form db)
        rpa (reverse (nav/parents-vec sel))
        nn (.getElementById js/document (modeline-portal-id (:db/id (first rpa))))]
    (when nn
      (-> (modeline-inner
           sel
           bus
           (when (:form/editing sel)
             (rum/react eb/editbox-ednparse-state)))
          (rum/portal nn)))))

(declare setup-app)
(rum/defc root-component
  [db bus]
  (let [state (d/entity db ::state)]
    [:div.bar-container
     #_(breadcrumbs-always db bus)
     (fcc (:state/bar state) bus 0 nil)
     (modeline-portal db bus)
     #_(cc/svg-viewbox (:state/bar state) core/blackhole)]))

(defn event->kbd
  [^KeyboardEvent ev]
  (str (when (.-altKey ev) "M-")
       (when (.-ctrlKey ev )
         "C-")
       (when (.-shiftKey ev) "S-")
       (.-key ev)))

(def initial-compose-state {:bindings special-key-map :compose nil})
(def key-compose-state (atom initial-compose-state))

(def keyboard-bus (atom nil))

(defn global-keydown*
  [ev]
  (let [tkd (js/performance.now)]
    (when-not @eb/global-editing-flag
      (let [kbd (event->kbd ev)
            {:keys [bindings compose]} @key-compose-state
            
            mut (get bindings kbd)
            next-kbd (conj (or compose []) kbd)]
        #_(prn "Key" kbd mut)
        (core/send! @keyboard-bus [:kbd kbd tkd])
        (when (or (some? mut)
                  (and compose (nil? mut)))
          (.preventDefault ev)
          (.stopPropagation ev))
        
        #_(if-not mut
            (reset! key-compose-state initial-compose-state)
            (cond
              (vector? mut) (do (reset! key-compose-state initial-compose-state)
                                (core/send! @keyboard-bus
                                            (with-meta mut {:kbd (string/join " " next-kbd)})))
              (map? mut) (reset! key-compose-state {:bindings mut :compose next-kbd})
              :else (do (println "Bad key entry" mut)
                        (reset! key-compose-state initial-compose-state))))))))


(defonce global-keydown
  (fn [ev] 
    (global-keydown* ev)))

(defn global-keyup*
  [ev]
  #_(prn "Keyup" (event->kbd ev)))

(defonce global-keyup
  (fn [ev]
    (global-keyup* ev)))

(rum/defc command-compose-feedback < rum/reactive
  []
  (let [{:keys [bindings compose]} (rum/react key-compose-state)]
    (when compose
      [:span.code-font.key-compose-echo
       (pr-str compose)
       " "
       (pr-str bindings)])))

(defn scroll-1d
  [size h pos off]
  (println "S1D" size h pos off)
  (let [align-bottom (- off (- size h))
        top-closer?  (< (js/Math.abs (- pos off))
                        (js/Math.abs (- pos align-bottom)))
        ;; _ (println "Top closer?")
        [best other] (if top-closer?
                       [off align-bottom]
                       [align-bottom off])]
    #_(println "Delta" (- pos best) "Hysteresis" (< -3 (- pos best) 3))
    (when-not (< -3 (- pos best) 3)
      (int best))
    #_(if (< -1 (- pos best) 1)
        other
        best)))

(defn scroll-to-selected!
  ([] (scroll-to-selected! true))
  ([always]
   (let [#_ #_el (js/document.querySelector ".selected")
         [el & more] (js/document.querySelectorAll ".selected")
         _ (prn "More" more)
         
         tl    (some-> el (.closest ".form-card"))
         chain (some-> el (.closest ".chain"))
         bar   (some-> chain (.closest ".bar"))
         
         chain-height (some-> chain (.-clientHeight))
         bar-width    (some-> bar (.-clientWidth))
         
         h    (some-> tl (.getBoundingClientRect) (.-height) (js/Math.ceil))
         vpos (some-> chain (.-scrollTop))
         voff (some-> tl (.-offsetTop))
         
         w    (some-> chain (.getBoundingClientRect) (.-width) (js/Math.ceil))
         hpos (some-> bar (.-scrollLeft))
         hoff (some-> chain (.-offsetLeft))]
     
     #_(js/console.log "Tl" tl "Chain" chain "Bar" bar)
     #_(println "================================Scroll"
              "\nChain-height" chain-height
              "\nBar-width" bar-width
              "\nh" h
              "vpos" vpos
              "voff" voff
              "\nw" w
              "hpos" hpos
              "hoff" hoff
              "\nCan fit?" (< h chain-height)
              "\nAlready visible?" (not (< vpos voff (+ h voff)
                                           (+ vpos chain-height))))
     (when (and chain 
                (< h chain-height)
                (or always (not (< vpos voff (+ h voff)
                                   (+ vpos chain-height)))))
       (.scrollTo chain #js{:top (int (scroll-1d chain-height h vpos voff))}))
     (when (and bar 
                (or always (not (< hpos hoff (+ w hoff)
                                   (+ hpos bar-width)))))
       (.scrollTo bar #js{:left (int (scroll-1d bar-width w hpos hoff))})))))

(defn ensure-selected-in-view! []
  (println "ESIV")
  (scroll-to-selected! false))

(def save-status (atom nil))

(defn save*
  [file contents]
  (let [spit (some-> js/window
                     (aget "my_electron_bridge")
                     (aget "spit"))]
    (when spit
      (-> (spit file contents)
          (.then  (fn [] (swap! save-status assoc :status :ok :file file)))
          (.catch (fn [] (swap! save-status assoc :status :error)))))))

(defn setup-app
  ([]
   (setup-app
    (doto (d/create-conn s/schema)
      (d/transact! init-tx-data))))
  ([conn]
   (let [a  (core/app conn)
         scivar-sel (sci/new-var "zsel")
         scivar-ingest (sci/new-var "ingest")
         s  (sci/init {:namespaces {'d {'datoms d/datoms
                                        'touch  d/touch}}
                       :bindings   {'jcl    (fn [z] (js/console.log z))
                                    'datafy datafy/datafy
                                    'nav    datafy/nav
                                    'fetch  (fn [z f]
                                              (-> z (js/fetch) (.then f)))
                                    'then (fn [p f] (.then (js/Promise.resolve p) f))
                                    'stories dfg/stories
                                    'ingest scivar-ingest
                                    'sel scivar-sel
                                    '->seq e/seq->seq}})
         ]
     (doseq [[m f] mut/dispatch-table]
       (core/register-simple! a m f))
     (doto a
       (core/register-mutation! :kbd
                                (fn [[_ kbd tkd] db bus]
                                  (when-let [mut (:key/mutation (d/entity db [:key/kbd kbd]))]
                                    (core/send! bus [mut]))))
       (core/register-mutation! :eval-sci
                                (fn [_ db bus]
                                  (let [et (->> (get-selected-form db)
                                                (move/move :move/most-upward))
                                        c   (e/->string et)
                                        #_(->> (e/->form et)
                                               (pr-str))]
                                    (println "Eval sci" c scivar-sel)
                                    
                                    (try
                                      (let [_ (sci/alter-var-root scivar-sel (constantly (get-selected-form db)))
                                            _ (sci/alter-var-root scivar-ingest
                                                                  (constantly (fn [z]
                                                                                (.then (js/Promise.resolve z)
                                                                                       (fn [ar]
                                                                                         (js/console.log "PARse" ar)
                                                                                         (core/send! bus [:ingest-result (:db/id et) ar])
                                                                                         ::ok)))))
                                            ans (sci/eval-string* s c)
                                            
                                            #_ (sci/eval-string c
                                                                )]
                                        (.then (js/Promise.resolve ans)
                                               (fn [ar]
                                                 (when (not= ::ok ar)
                                                   (core/send! bus [:eval-result (:db/id et) ar])))))
                                      (catch :default e
                                        (js/console.log "SCI exception" e))))))
       (core/register-mutation! :form/highlight (fn [_ _ _]
                                                  (js/window.setTimeout
                                                   (fn [] (ensure-selected-in-view!))
                                                   1)))
       (core/register-mutation! :scroll  (fn [_ _ _]
                                           (scroll-to-selected!)))
       (core/register-mutation! :save
                                (fn [_ db bus]
                                  (let [xhr   (XhrIo.)
                                        sel   (get-selected-form db)
                                        chain (-> sel
                                                  (nav/parents-vec)
                                                  (peek)
                                                  :coll/_contains
                                                  first)
                                        file  (or (:chain/filename chain) "noname.clj")]
                                    (println "Do save" chain)
                                    
                                    (when chain
                                      (reset! save-status {:at (:max-tx db) :on (:db/id sel) :status :saving})
                                      (save* file (e/->string chain))
                                      #_(do
                                        (.listen xhr EventType/COMPLETE
                                                 (fn [ev]
                                                   (reset! save-status {:on     (:db/id sel)
                                                                        :at     (:max-tx db)
                                                                        :status :ok
                                                                        :file   file})))
                                        (.listen xhr EventType/ERROR
                                                 (fn [_] (println "Save error!")
                                                   (reset! save-status {:on (:db/id sel) :status :error})))
                                        (.send xhr (str "/save?file=" (or (:chain/filename chain) "noname.clj"))
                                               "POST"
                                               (e/->string chain)
                                               #js {"Content-Type" "application/json;charset=UTF-8"})))
                                    nil)))
       #_(core/register-mutation!
          :execute
          (fn [_ db bus]
            (let [sel (get-selected-form db)]
              (d/transact! conn )
            
              )))))))

(rum/defc example < rum/static
  [init-form muts] 
  (let [conn (d/create-conn s/schema)
        form-txdata (e/->tx init-form)
        {:keys [db-after tempids] :as init-report}
        (d/transact! conn
                     [{:db/ident ::state
                       :state/bar {:db/id "bar"
                                   :coll/type :bar
                                   :seq/first {:coll/type :chain
                                               :coll/_contains "bar"
                                               :coll/contains #{(:db/id form-txdata)}
                                               :seq/first form-txdata}}}])
        
        toplevel-eid (get tempids (:db/id form-txdata))
        
        reports (reductions
                 (fn [{:keys [db-after]} [m & args]]
                   (if-let [mut-fn (get mut/dispatch-table m)]
                     (try
                       (let [tx (apply mut-fn db-after args)]
                         (assoc (d/with db-after tx) :input-tx tx))
                       (catch :default e
                         (reduced
                          {:failure
                           [:div [:p.prose-font [:span {:style {:color "tomato"}}
                                                 "Exception"]
                                  " "
                                  (ex-message e)]
                            [:p (with-out-str (cljs.pprint/pprint (ex-data e)))]
                            "Mutation"
                            [:p (pr-str (into [m] args) )]
                            "At selected form"
                            [:p (with-out-str (cljs.pprint/pprint (d/touch (get-selected-form db-after))))]]})))
                     (throw (ex-info "No mutation" {:m m}))))
                 init-report
                 muts)]
    [:div {:style {:display "flex" :flex-direction "row" :width "1200px"}}
     [:div {:style {:display "flex" :flex-direction "column" :width "100%"}}
      (for [[i m {:keys [failure input-tx db-after tx-data]} other] (map vector (range) (cons "initial" muts) reports (cons nil reports))]
        (if failure
          [:div failure]
          [:div {:key i :style {:display :flex
                                :flex-direction :column
                                :margin-top "2ex"
                                :border "1px solid #ae81ff"}}
           [:span (str "#" i " " (pr-str m))]
           [:div {:style {:border "1px solid #777"}}
            (when other (root-component (:db-after other) core/blackhole))]
           [:div {:style {:border "1px solid #777"}}
            (root-component db-after core/blackhole)]
           (when (< 0 i)
             [:div
              #_(pr-str (e/->form (get-selected-form db-after)))
              #_(pr-str (invar/check-all (:state/bar (d/entity db-after ::state))))
              
              #_[:div ;; :details [:summary "SVG"]
                 [:div {:style {:display :flex :flex-direction :row}}
                  (cc/svg-viewbox (:state/bar (d/entity (:db-after other) ::state)) core/blackhole)
                  (cc/svg-viewbox (:state/bar (d/entity db-after ::state)) core/blackhole)]]
              #_(pr-str input-tx)
              [:div   ;; :details[:summary "txdata"]
               (debug/datoms-table-eavt* tx-data)]
              #_[:div
               (debug/datoms-table-eavt* (d/datoms db-after :eavt))]])]))]]))

(defn some-random-mutations
  [n]
  (->> #_[[[:insert-right] [:edit/finish "x"]]
        [[:insert-right] [:edit/finish "y"]]
        [[:flow-right]]
        [[:flow-right]]
        [[:flow-left]]
        [[:flow-left]]
        [[:raise]]
        #_[[:clone]]
        [[:next]]
        [[:prev]]
        [[:float]]
        [[:float]]
        [[:sink]]
        [[:sink]]
        #_[[:parent]]
        [[:tail]]
        [[:tail]]
        [[:wrap]]
        [[:delete-left]]
        [[:delete-left]]
        [[:wrap]]
        [[:wrap]]
        [[:delete-right]]
        [[:delete-right]]
        [[:insert-left] [:edit/finish "b"]]
        [[:insert-left] [:edit/finish "a"]]
        [[:slurp-right]] [[:slurp-right]] [[:slurp-right]] [[:slurp-right]] [[:slurp-right]] [[:slurp-right]]
        #_[[:delete-right]]
        #_[[:delete-left]]]
       [
        [[:slurp-right]] [[:slurp-right]] [[:slurp-right]] [[:slurp-right]] [[:slurp-right]] [[:slurp-right]]
        [[:barf-right]] [[:barf-right]] [[:barf-right]] [[:barf-right]] [[:barf-right]] [[:barf-right]]]
       cycle
       (take n)
       vec
       (shuffle)
       (apply concat)
       (vec)))


(rum/defc player-mutation-view < rum/reactive
  [a]
  (when-let [v (rum/react a)]
    [:pre v]))

(rum/defc player
  [init-form muts]
  (let [form-txdata (e/->tx init-form)
        {:keys [conn bus]} (setup-app (doto (d/create-conn s/schema)
                                        (d/transact! [{:db/ident ::state
                                                       :state/bar {:db/id "bar"
                                                                   :coll/type :bar
                                                                   :seq/first {:coll/type :chain
                                                                               :coll/_contains "bar"
                                                                               :coll/contains #{(:db/id form-txdata)}
                                                                               :seq/first form-txdata}}}])))
        ;; se (d/entity @conn ::state)
        hz 44
        mv (atom nil)
        zch (core/zchan bus)]
    (rum/use-effect!
     (fn setup []
       (println "Setup")
       (async/go-loop [i 0
                       [m & more] muts
                       t nil]
         (if (and t )
           (swap! mv #(str "#" i " "
                           (/ (inc i)
                              (- (js/performance.now) t)
                              (/ 1 1000))
                           " "
                           (pr-str m)
                           "\n"
                           %)))
         (case m
           ::random-insert (do (core/send! bus [:insert-right])
                               (core/send! bus [:edit/finish (str "m" i)]))
           (core/send! bus m))
         #_(async/<! (async/timeout (/ 1000.0 hz)))
         (async/<! (async/timeout 0))
         (if-not more
           (let [dms (- (js/performance.now) t)]
            (println "Finished" (inc i) "iter "
                     "Total ms" dms
                     " Hertz"
                     (* 1000 (/ (inc i) dms))))
           (recur (inc i) more (or t (js/performance.now)))))
       (fn cleanup []
         (println "Cleanup")))
     [])
    [:div
     (root-component @conn bus)
     #_(player-mutation-view mv)]))

(rum/defc debug-component
  []
  [:div {:style {:margin-top "2ex"}  }
   #_(player
    '[a b c ^:form/highlight [a s d f] [l e l] [O] d]
    (some-random-mutations 10000))
   
   (example
    '[a ^:form/highlight [] b c]
    #_[[:slurp-right] [:slurp-right]]
    [[:delete-right] [:barf-right] [:barf-right] [:delete-right] [:flow-left] [:barf-right]])
   
   #_(example
      '[a ^:form/highlight b c [a a a]]
      [[:float] [:delete-left] [:flow-right] [:float]]
      #_[[:last]])])

#_(defonce the-singleton-db
  (doto (d/create-conn s/schema)
    (d/transact! init-tx-data)))

(letfn [(pubsub []
          (let [subs (js/Set.)]
            {:sub (fn [f]
                    (.add subs f)
                    (fn [] (.delete subs f)))
             :pub (fn [m]
                    (.forEach subs (fn [f] (f m))))}))]
  (let [{:keys [pub sub]} (pubsub)
        unsub (sub (fn [m] (println "Messg" m)))
        _ (pub "Message")
        _ (unsub)
        _ (pub "M@")]))

#_(defn stupid-github-crap
  []
  (-> (js/fetch "https://api.github.com/repos/babashka/sci/git/ref/heads/master")
      (.then #(.json %))
      (.then #(do (js/console.log %) %))
      (.then (fn [ref]
               (println (js->clj ref))
               (-> (js/fetch (-> ref js->clj (get "object") (get "url")))
                   (.then #(.json %))
                   (.then (fn [commit]
                            (js/console.log commit)
                            (-> (js/fetch (-> commit js->clj (get "tree") (get "url")))
                                (.then #(.json %))
                                (.then (fn [tree]
                                         (js/console.log tree)
                                         ))
                                ))))))))

(defn fetch-json
  [u]
  (-> (js/fetch u)
      (.then #(.json %))
      (.then #(js->clj % :keywordize-keys true))))


#_(defn stupid-github-crap
  []
  (-> (js/fetch "https://api.github.com/repos/babashka/sci/git/ref/heads/master")
      (.then #(.json %))
      (.then #(do (js/console.log %) %))
      (.then (fn [ref]
               (println (js->clj ref))
               (-> (js/fetch (-> ref js->clj (get "object") (get "url")))
                   (.then #(.json %))
                   (.then (fn [commit]
                            (js/console.log commit)
                            (-> (js/fetch (-> commit js->clj (get "tree") (get "url")))
                                (.then #(.json %))
                                (.then (fn [tree]
                                         (js/console.log tree)
                                         ))
                                ))))))))


(defn stupid-github-crap
  []
  (a/let [ref    (fetch-json "https://api.github.com/repos/babashka/sci/git/ref/heads/master")
          commit (fetch-json (-> ref :object :url)) 
          tree   (fetch-json (-> commit :tree :url))]
    
    #_(cljs.pprint/pprint tree))) 

(defn  ^:dev/after-load init []
  (js/document.removeEventListener "keydown" global-keydown true)
  (js/document.removeEventListener "keyup" global-keyup true)
  
  
  (js/document.addEventListener "keydown" global-keydown true)
  (js/document.addEventListener "keyup" global-keyup true)
  (h/clear!)
  
  #_(stupid-github-crap)
  
  (let [{:keys [conn bus]} (setup-app #_the-singleton-db)] 
    
      (println "Reset keyhboard bus" bus)
      (reset! keyboard-bus bus)
      #_(stupid-github-crap)
    
      #_(go
          (async/<!
           (async/onto-chan!
            (core/zchan bus)
            [[:clone]
             [:delete-right] [:delete-right] [:delete-right]
             [:undo] [:undo] [:undo]
             [:insert-right]
             [:edit/finish "nice"]
             [:reify-undo]]
            false))
          (println "We did it")
      
          (rum/mount
           #_(debug-component)
           (root-component @conn bus)
           (.getElementById js/document "root")))
    
      ;; document.write(process.versions['electron'])
      (rum/mount
       #_(debug-component)
       (root-component @conn bus)
       (.getElementById js/document "root"))))

;; Single highlight, in DB
;; - Mutations find it? With a lookup ref [highlight true]
;; - Mutations set it? In the tx-data
;; - fcc renders it?  From the entity
;; Single highlight outside
;; - Find? Protocol method
;; - Set?
;; -- Actions either:
;; -- -- Pure movement
;; -- -- Pure mutation, leaves selection the same
;; -- -- Create something and select it
;; -- -- Do something then select something which existed already (deletion)
;; -- -- What about doing a modification and selecting something which didn't already exist?
;; -- When you want to move the selection from a mutation:
;; -- Either the target already exists so you can find it ahead of time
;; -- Or it does not in which case you must be creating it and can supply a tempid
;; - Render? Equality check with protocol method
;; Multi highlight in DB
;; - Find? Lookup ref [highlight me]
;; - Set? In the tx-data
;; - Render? From the entity - but what if cursors overlap?
;; Multi highlight outside


;; Fix the modeline changing size and reflowing
;; Implement deletion-chain / reparenting
;; Implement comma
;; 

;; Make the mutations all take the selection as the parameter
;; Make the mutations set the selection with a special tempid
;; Move all mention of form/highlight to core api methods
;; Reimplement it as atom holding an entity and see what happens

;; Scroll is not cleanly unmounted on reload?


;; Dogfooding
;; Round trip:
;; text file -> rewrite clj parser -> db -> string

;; Scroll on settimeout so we don't query the dom and mutate it again immediately
;; Buffer all of the comments you write and use them as a commit msg

;; keyword/value, symbol/value etc...
;; REALLY stupid compared to token/type because of multiple queries

;; Derefs, syntax quotes, etc - give them seq/first but make them not collections?
;; Have to change stuff to check seq/first before coll/type...
;; But if we check coll/type don't we always check seq/first anyway so it doesn't matter?

;; Insert within chain should automatically give ()s

;; Fix scrolling

;; If you want to re-use the exact same db state for undos and inverse mutations:
;; -- You need to increment max-tx and use that as a unique identifier for a state
;; -- You cannot store historical tx-reports because they might be backwards
;; --

;; Highlight the inactive chain/selections...
;; COW clone?????

;; Aliases are a huge problem for scrolling because how do you know what to center?
;; Suppose I alias the same form and put one copy at each corner of the screen, then what?
;; The best it can really do is pick the one under the alias with the highest db/id
;; Even if it does then, then how do you implement go-to-next-alias?
;; That's not even to mention the fact that it FORCES queryselectorall?

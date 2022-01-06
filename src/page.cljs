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
   
   [core :as core
    :refer [get-selected-form
            move-selection-tx]])
  (:require-macros
   [cljs.core.async.macros :refer [go
                                   go-loop]]
   [macros :refer [macro-slurp]]))

(def load-time (js/Date.now))

(def test-form-data-bar
  '[["Chain 1"
     (def thing
       [[1 2 3 ^:form/highlight (+ 1 2 9) [:a :b :c] "ok"] :x.y/z])]])

(defn test-form-data-tx
  [chain-txdatas]
  (assoc
   (e/seq-tx
    (concat
     (for [ch chain-txdatas]
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
                        "history"
                        }
       :seq/first {:db/id "label"
                   :string/value "Keyboard"}
       :seq/next {:seq/first "defaultkeymap"
                  ;; :seq/next {:seq/first "evalchain"}
                  :seq/next {:seq/first "history"
                             :seq/next {:seq/first "evalchain"
                                        :seq/next {:seq/first "inspect"}}}
                  }}]))
   :db/id "bar"
   :coll/type :bar))


(def default-keymap
  {"f"         :flow-right
   "a"         :flow-left
   "w"         :float
   "s"         :sink
   "S-H"       :toplevel
   "h"         :parent
   "j"         :next
   "k"         :prev
   "l"         :tail
   "r"         :raise
   " "         :insert-right
   "S- "       :insert-left
   "d"         :delete-right
   "Backspace" :delete-left
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
   "1"         :m1
   "2"         :m2
   "3"         :m3
   "4"         :m4
   "5"         :m5
   "6"         :m6
   "7"         :m7
   "8"         :m8
   "v"         :scroll
   "-"         :minus
   "i"         :insert-left
   "S-Q"       :stringify
   "S-+"       :plus
   
   })

(def init-tx-data
  (let [txe (test-form-data-tx (concat
                                (map e/->tx test-form-data-bar)
                                
                                #_[(e/string->tx-all (macro-slurp "src/page.cljs"))]
                                #_[(e/string->tx-all (macro-slurp "src/cmd/edit.cljc"))]
                                #_[(e/string->tx-all (macro-slurp "src/cmd/mut.cljc"))]
                                #_[(e/string->tx-all (macro-slurp "src/cmd/nav.cljc"))]))]
    (concat
     [{:db/ident ::state
       :state/bar (:db/id txe)}
      {:db/ident ::history
       :db/id "history"
       :coll/type :vec
       :seq/first {:string/value "end of history"
                   :coll/_contains "history"}}
      {:db/ident ::evalchain
       :db/id "evalchain"
       :coll/type :vec
       :seq/first {:string/value "No more evals" :coll/_contains "evalchain"}}
      {:db/ident ::inspect
       :db/id "inspect"
       :coll/type :inspect
       :seq/first {:string/value "No inspect" :coll/_contains "history"}}
      {:db/ident ::default-keymap
       :db/id "defaultkeymap"
       :coll/type :keyboard
       :keymap/bindings (for [[k m] default-keymap]
                          {:key/kbd k :key/mutation m})}
      txe])))

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
   [:div.bp {:id (breadcrumbs-portal-id (:db/id e))}]
   [:div.top-level-form.code-font
    (fcc e bus 0 p)]
   [:div.modeline-size {:id (modeline-portal-id (:db/id e))}]])

(defn do-indent
  [child linebreak? indent-level]
  (if-not linebreak?
    child
    (rum/fragment
     [:span.indent-chars "\n"
      [:span.indenter {:style {:margin-left (str indent-level "ch")}}]]
     child)))

(defn computed-indent
  [e indent-prop]
  (+ indent-prop
     (or (:form/indent e)
         0)))

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
    
    #_[:span.inline-tag (str (:db/id e))]
    
    open]
   (for [x (e/seq->vec e)]
     (-> (fcc x bus (computed-indent e indent) proply)
         (rum/with-key (:db/id x))))
   [:span.d.cl close]])

(defmethod display-coll :list [c b i s p] (delimited-coll "(" ")" c b i s p))
(defmethod display-coll :vec  [c b i s p] (delimited-coll "[" "]" c b i s p))
(defmethod display-coll :map  [c b i s p] (delimited-coll "{" "}" c b i s p))
(defmethod display-coll :set  [c b i s p] (delimited-coll "#{" "}" c b i s p))

(defmethod display-coll :hidden  [c b i s p]
  [:div {:style {:width "800px"}}
   (cc/svg-viewbox c core/blackhole)]
  #_[:span
   (cond-> {:class (str "c " s)}
     s (assoc :ref "selected"))
   (case (:hidden/coll-type c)
     :list "(\u00b7\u00b7\u00b7)"
     :vec "[...]"
     :map "{...}"
     :set "#{...}"
     "<...>")])

(defmethod display-coll :chain [chain bus i classes proply]
  [:div.chain
   {:key (:db/id chain)
    :ref "chain"
    :id (str "c" (:db/id chain))
    :class classes}
   #_(str "Chain" (:db/id chain))
   (for [f (e/seq->vec chain)]
     (-> (top-level-form-component f bus proply)
         (rum/with-key (:db/id f))))])

(defmethod display-coll :bar [bar bus i c p]
  [:div
   (cond-> {:class (str "bar"  (when c " ") c)}
     c (assoc :ref "selected"))
   (for [chain-head (e/seq->vec bar)]
     (-> (fcc chain-head bus i p)
         (rum/with-key (:db/id chain-head))))])

(defmethod display-coll :keyboard [k bus i]
  [:div.display-keyboard
   (ck/keyboard-diagram
    k
    #_(d/entity (d/entity-db k) )
    )
   
   [:div {:style {:width "6ex"
                  :font-size "9pt"}}
    (ck/kkc {} "F")]])


(defmethod display-coll :alias [{:alias/keys [of]} b i s]
  (fcc of core/blackhole i))

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
       (concat
        (d/datoms (d/entity-db sel) :eavt  (:db/id sel))
        (->> (d/datoms (d/entity-db sel) :avet  )
             (filter (fn [[e a v t]]
                       (= v (:db/id sel)))))))]]))

(defmethod display-coll :inspect [k bus i]
  (inspector (d/entity-db k) bus))

(defmethod display-coll :timetravel [tt bus i]
  [:div [:span.prose-font "Time travel"]
   (for [e (e/seq->vec (:seq/first tt))]
     [:div {:key (:db/id e)}
      "Item # " (pr-str e)])])

(defn token-class
  [e]
  (or (when-let [s (:symbol/value e)]
        (case s
          ("defn" "let" "when" "and" "or" "if"
           "when-not" "if-not" "def" "cond" "case") "m"
          ("first") "s"
          "v"))
      (when (:keyword/value e) "k")
      (when (:string/value e) "l")
      (when (:number/value e) "n")))

(defn token-text
  [e]
  (or
   (some-> e :symbol/value str)
   (when-let [k (:keyword/value e)]
     (if (string? k)
       k
       (let [kns (namespace k)]
         (if-not kns
           (str k)
           (rum/fragment ":" [:span.kn kns] "/" (name k))))))
   (:string/value e)
   (some-> e :number/value str)))

(rum/defc fcc < dbrx/ereactive
  [e bus indent-prop proply]
  #_(when proply (prn "FccProply" (:db/id e) proply))
  (-> (or (when (:form/editing e)
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
                          proply))
          (comment "Probably a retracted entity, do nothing"))
      (do-indent (:form/linebreak e)
                 (computed-indent e indent-prop))))

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
(rum/defc parent-path
  [rpa bus]
  [:ul.parent-path
   (for [i (range (count rpa))]
     (let [parent (nth rpa i)]
       [:li {:key i}
        [:a {:href "#"
             :on-click #(do (.preventDefault %)
                            (core/send! bus [:select (:db/id parent)]))}
         [:span.code-font
          (when (< i breadcrumbs-max-numeric-label)
            [:span.inline-tag (str (inc i))])
          (if (= :list (:coll/type parent))
            (or (some-> parent :seq/first :symbol/value) "()")
            (case (:coll/type parent) :vec "[]" :map "{}" "??"))]]]))])

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

(declare command-compose-feedback)
(rum/defc modeline-inner < dbrx/ereactive
  [sel bus {:keys [text valid] :as edn-parse-state}]
  [:span {:class (str "modeline modeline-size code-font"
                      (when text " editing")
                      (when (and (not (empty? text)) (not valid)) " invalid"))}
   [:span.modeline-echo.modeline-size
    (when text
      (pr-str text))]
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
    (command-compose-feedback)]])

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
     (breadcrumbs-always db bus)
     (fcc (:state/bar state) bus 0 nil)
     #_(modeline-portal db bus)
     #_(cc/svg-viewbox (:state/bar state) core/blackhole)]))

(defn event->kbd
  [^KeyboardEvent ev]
  (str (when (.-altKey ev) "M-")
       (when (.-ctrlKey ev ) "C-")
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
        (if-not mut
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
  (let [align-bottom (- off (- size h))
        top-closer?  (< (js/Math.abs (- pos off))
                        (js/Math.abs (- pos align-bottom)))
        [best other] (if top-closer?
                       [off align-bottom]
                       [align-bottom off])]
    (if (< -1 (- pos best) 1)
      other
      best)))

(defn scroll-to-selected!
  ([] (scroll-to-selected! true))
  ([always]
   (let [el (js/document.querySelector ".selected")
         tl    (some-> el (.closest ".form-card"))
         chain (some-> el (.closest ".chain"))
         bar   (some-> chain (.closest ".bar"))
         
         chain-height (some-> chain (.-clientHeight))
         bar-width    (some-> bar (.-clientWidth))
         
         h    (some-> tl (.getBoundingClientRect) (.-height))
         vpos (some-> chain (.-scrollTop))
         voff (some-> tl (.-offsetTop))
         
         w    (some-> chain (.getBoundingClientRect) (.-width))
         hpos (some-> bar (.-scrollLeft))
         hoff (some-> chain (.-offsetLeft))]
     (when (and chain 
                (or always (not (< vpos voff (+ h voff)
                                   (+ vpos chain-height)))))
       (.scrollTo chain #js{:top (scroll-1d chain-height h vpos voff)}))
     (when (and bar 
                (or always (not (< hpos hoff (+ w hoff)
                                   (+ hpos bar-width)))))
       (.scrollTo bar #js{:left (scroll-1d bar-width w hpos hoff)})))))

#_(defn scroll-to-selected!
  ([] (scroll-to-selected! true))
  ([always]
   (let [tl    (some-> (js/document.querySelector ".selected")
                       (.closest ".form-card"))
         h     (some-> tl (.getBoundingClientRect) (.-height))
         chain (some-> tl (.closest ".chain"))
         pos   (some-> chain (.-scrollTop))
         off   (some-> tl (.-offsetTop))]
     (when (and chain
                (or always (not (< pos off (+ off h)
                                   (+ pos (.-clientHeight chain))))))
       (let [align-bottom (- off
                             (- (.-clientHeight chain)
                                h))
             top-closer?  (< (js/Math.abs (- pos off))
                             (js/Math.abs (- pos align-bottom)))
             [best other] (if top-closer?
                            [off align-bottom]
                            [align-bottom off])]
         (.scrollTo chain #js{:top
                              (if (< -1 (- pos best) 1)
                                other
                                best)}))))))

(defn ensure-selected-in-view! [] (scroll-to-selected! false))

(defn setup-app
  ([]
   (setup-app
    (doto (d/create-conn s/schema)
      (d/transact! init-tx-data))))
  ([conn]
   (let [a (core/app conn)
         s (sci/init {})
         
         chain->io (atom {})
         
         io (js/IntersectionObserver.
             (fn [a b c d e]
               (js/console.log "IO" a))
             #js {:root       js/document.body
                  :rootMargin "0px"
                  :threshold  (array 0 0.2 0.4 0.6 0.8 1)})]
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
                                        c  (->> (e/->form et)
                                                (pr-str))]
                                    (println "Eval sciNioce" c)
                                    (try
                                      (let [ans (sci/eval-string* s c)
                                            
                                            #_(sci/eval-string c
                                                               {:namespaces {'d {'datoms d/datoms
                                                                                 'touch d/touch}}
                                                                :bindings {'db db
                                                                           'sel (get-selected-form db)}})]
                                        (core/send! bus [:eval-result (:db/id et) ans]))
                                      (catch :default e
                                        (js/console.log "SCI exception" e))))))
       (core/register-mutation! :form/highlight (fn [_ _ _] (ensure-selected-in-view!)))
       (core/register-mutation! :scroll  (fn [_ _ _] (scroll-to-selected!)))))))

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
              
              [:div ;; :details [:summary "SVG"]
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
   (player
    '[a b c ^:form/highlight [a s d f] [l e l] [O] d]
    (some-random-mutations 10000))
   
   #_(example
      '[a ^:form/highlight [] b c]
      [[:slurp-right] [:slurp-right]]
      #_[[:delete-right] [:barf-right] [:barf-right] [:delete-right] [:flow-left] [:barf-right]])
   
   #_(example
      '[a ^:form/highlight b c [a a a]]
      [[:float] [:delete-left] [:flow-right] [:float]]
      #_[[:last]])])

#_(def the-singleton-db
  (doto (d/create-conn s/schema)
    (d/transact! init-tx-data)))

(defn  ^:dev/after-load init []
  (js/document.removeEventListener "keydown" global-keydown true)
  (js/document.removeEventListener "keyup" global-keyup true)
  
  
  (js/document.addEventListener "keydown" global-keydown true)
  (js/document.addEventListener "keyup" global-keyup true)
  (h/clear!)
  (let [{:keys [conn bus]} (setup-app #_the-singleton-db)] 
    
    (println "Reset keyhboard bus" bus)
    (reset! keyboard-bus bus)
    (println "DKL" (d/entity @conn ::default-keymap))
    (run! prn (d/touch (d/entity @conn ::default-keymap)))
    
    (rum/mount
     
     #_(debug-component)
     (root-component @conn bus)
     
     (.getElementById js/document "root"))))


;; create a new toplevel function above the current
;; write next list/symbol with/without newline with/without going up first
;; 

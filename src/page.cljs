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
   [comp.hex :as chex]

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
     (defn thing
       [a b c ^:form/highlight [a s d f] [l e l] [O] d] blah)]])

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
       :coll/contains #{"label" "keyboard"
                        "inspect"
                        "evalchain"
                        ;; "history"
                        ;; "timetravel"
                        }
       :seq/first {:db/id "label"
                   :string/value "Keyboard"}
       :seq/next {:seq/first {:db/id "keyboard"
                              :coll/type :keyboard}
                  :seq/next {:seq/first "inspect"
                             :seq/next {:seq/first "evalchain"}
                             ;; :seq/next {:seq/first "history"}
                             
                             }
                  
                  ;; :seq/next {:seq/first "timetravel"}
                  #_:seq/next #_{:seq/first {:db/id "hexes"
                                             :coll/type :hexes}}}}]))
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
   "c"         :clone
   "z"         :hop-left
   "x"         :hop-right
   "q"         :compose
   "9"         :wrap
   "0"         :parent
   "p"         :slurp-right
   "Tab"       :indent
   "S-Tab"     :dedent
   "e"         :eval-sci
   "S-("       :new-list
   "["         :new-vec


   "1" :m1
   "2" :m2
   "3" :m3
   "4" :m4
   "5" :m5
   "6" :m6
   "7" :m7
   "8" :m8
   
   })

(def init-tx-data
  (let [txe (test-form-data-tx (concat
                                (map e/->tx test-form-data-bar)
                                [(e/string->tx-all (macro-slurp "src/embed.cljc"))]
                                #_[(e/string->tx-all (macro-slurp "src/cmd/edit.cljc"))]))
        timetravel-placeholders (e/->tx (vec (range 9)))]
    (concat
     
     [{:db/ident ::state
       :state/bar (:db/id txe)}
      {:db/ident ::history
       :db/id "history"
       :coll/type :vec
       :seq/first {:string/value "Eod of history" :coll/_contains "history"}
       :seq/next {:seq/first {:string/value "Really end " :coll/_contains "history"}}}
      {:db/ident ::evalchain
       :db/id "evalchain"
       :coll/type :vec
       :seq/first {:string/value "No more evals" :coll/_contains "evalchain"}}
      {:db/ident ::inspect
       :db/id "inspect"
       :coll/type :inspect
       :seq/first {:string/value "No inspect" :coll/_contains "history"}}
      {:db/ident ::timetravel
       :db/id "timetravel"
       :coll/type :timetravel
       :seq/first (:db/id timetravel-placeholders)}
      timetravel-placeholders
      txe]
     (for [[k m] default-keymap]
       {:key/kbd k :key/mutation m}))))

(defn ->mutation
  [tx-fn]
  (fn [[_ & args :as mutation]]
    #_(let [db @conn]
     (when-let [tx-data (apply tx-fn db args)]
       ;; transact metadata on db/current-tx?  run out of memory?
       #_(d/transact! conn tx-data (merge {:mutation mutation :input-tx-data tx-data} (meta mutation)))
       (let [he (d/entity db ::history)
             ;; tmr (js/performance.now)
             ;; tkd (get (meta mutation) :tkd)
             mnode (assoc (e/->tx mutation)
                          :form/linebreak true
                          ;; :tmp (- tmr tkd)
                          )]
         #_(println "Insert before" (d/touch (:seq/first he)))
         (println mutation)
         (run! prn tx-data)
         (d/transact! conn
                      tx-data
                      #_(concat tx-data
                                [{:db/id (:db/id he)
                                  :seq/first mnode
                                  :seq/next {:db/id "mcons"
                                             :seq/first (:db/id (:seq/first he))}}
                                 [:db/add (:db/id he) :coll/contains (:db/id mnode)]
                                 (when-let [nn (:seq/next he)]
                                   [:db/add "mcons" :seq/next (:db/id nn)])])
                      (merge {:mutation mutation
                              :input-tx-data tx-data}
                             (meta mutation))))))))

;; hacks
(def auto-scroll-hysteresis-px 400)
(defn scroll-element-to-window-center!
  [^js/HTMLElement el]
  (let [top (.-offsetTop el)
        wh (.-innerHeight js/window)
        new-scroll-top  (- top (/ wh 2))
        delta (- new-scroll-top (.-scrollY js/window))]
    #_(println "NST" new-scroll-top "Delta" delta)
    (when (< auto-scroll-hysteresis-px (js/Math.abs delta))
      (js/window.scrollTo #js {:left 0 :top new-scroll-top}))))

#_(defn scroll-element-in-div!
  [^js/HTMLElement el]
  (let [top (.-offsetTop el)
        parent (.-parentNode el )]
    (js/console.log  el "Parent" parent  )
    (set! (.-scrollTop parent ) top)))



#_(defn scroll-within-chain*
  [sel]
  (let [top-level (first (reverse-parents-array sel))
        chain (some-> top-level :coll/_contains first)
        ss (aget dbrx/ereactive-components-by-eid (:db/id sel))
        c (first (aget dbrx/ereactive-components-by-eid (:db/id chain)))
        el-chain (some-> c (.-refs) (aget "chain"))
        tl (first (aget dbrx/ereactive-components-by-eid (:db/id top-level)))
        el-top (some-> tl (.-refs) (aget "top-level"))]
    (doseq [s ss]
      (when-let [el-sel (some-> s (.-refs) (aget "selected"))]
        #_(js/console.log "Offset" (.-offsetTop el-sel)
                          "Scrolltop" (.-scrollTop el-chain)
                          "C-Clientheight" (.-clientHeight el-chain)
                          "C-Scrollheight" (.-scrollHeight el-chain)
                          (.getBoundingClientRect el-sel))
        (if-not el-top
          (println "No top")
          (let [br (.getBoundingClientRect el-top)
                outer-height (.-clientHeight el-chain)
                inner-height (.-height br)
                offset (.-offsetTop el-top)
                space-left (- outer-height inner-height)
                pos (.-scrollTop el-chain)]
            (.scrollTo el-chain
                       #js{:top offset
                           :behavior "smooth"})))))))

#_(def scroll-within-chain! (gf/throttle scroll-within-chain* 222))

#_(defn scroll-ref-into-view-after-render 
  [ref-name]
  {:did-update
   (fn [{:keys [subsequent] :as state}]
     (when-let [sel (some-> state :rum/react-component (.-refs) (aget ref-name))]
       (scroll-within-chain! (first (:rum/args state))))
     state)})

;; replace with non-breaking hyphen, lmao
#_(-> text (gstring/replaceAll "-" "‑"))

(declare fcc)

(defn breadcrumbs-portal-id [eid] (str eid "bp"))
(defn modeline-portal-id [eid] (str eid "mp"))

(rum/defc top-level-form-component < dbrx/ereactive 
  [e bus]
  [:div.form-card {:ref "top-level"}
   
   #_[:span.form-title.code-font (str "#" (:db/id e)
                                      " T+" (- (js/Date.now) load-time) "ms")]
   
   [:div.bp {:id (breadcrumbs-portal-id (:db/id e))}]
   [:div.top-level-form.code-font (fcc e bus 0)]
   [:div.modeline-size {:id (modeline-portal-id (:db/id e))}]
   
   
   #_(if (= :list (:coll/type e))
       [:div {:style {:width "1800px"}}
        (cc/svg-viewbox e bus)])])

(defn do-indent
  [child linebreak? indent-level]
  (if-not linebreak?
    child
    (rum/fragment
     [:span.indent-chars "\n"
      [:span.indenter {:style {:margin-left (str indent-level "ch")}}
       #_(apply str (repeat indent-level "X"))
       ]]
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
  [e bus indent classes open close]
  [:span (cond-> {:class (str "c " classes)}
           classes (assoc :ref "selected"))
   [:span.d
    #_[:span.inline-tag-outer
     [:span.inline-tag-inner (subs (str (:db/id e)) 0 1)]]
    #_[:span.inline-tag (str (:db/id e))]
    open]
   (for [x (e/seq->vec e)]
     (-> (fcc x bus (computed-indent e indent))
         (rum/with-key (:db/id x))))
   [:span.d close]])

(defmethod display-coll :list [c b i s] (delimited-coll c b i s "(" ")"))
(defmethod display-coll :vec  [c b i s] (delimited-coll c b i s "[" "]"))
(defmethod display-coll :map  [c b i s] (delimited-coll c b i s "{" "}"))
(defmethod display-coll :set  [c b i s] (delimited-coll c b i s "#{" "}"))

(defmethod display-coll :chain [chain bus i classes]
  [:div.chain
   {:key (:db/id chain)
    :ref "chain"
    :class classes}
   (for [f (e/seq->vec chain)]
     (-> (top-level-form-component f bus)
         (rum/with-key (:db/id f))))])

(defmethod display-coll :bar [bar bus i]
  [:div.bar
    (for [chain-head (e/seq->vec bar)]
      (-> (fcc chain-head bus i)
          (rum/with-key (:db/id chain-head))))])

(defmethod display-coll :keyboard [k bus i]
  [:div.display-keyboard (ck/keyboard-diagram)])

(defmethod display-coll :hexes [k bus i]
  [:div
   (chex/main)])

(defmethod display-coll :alias [{:alias/keys [of]} b i s]
  (fcc of core/blackhole i))

(rum/defc inspector < (dbrx/areactive :form/highlight :form/edited-tx)
  [db bus]
  (let [sel (get-selected-form db)]
    [:div.inspector
     [:span.prose-font (str "Inspect #" (:db/id sel))]
     (when-let [parent (first (:coll/_contains sel))]
       (case (:coll/type parent)
         (:bar :chain)
         [:div "Cannot inspect this"]
         
         [:div
          #_(fcc parent 0)
          [:div
           (debug/datoms-table-eavt*
            (concat
             (d/datoms (d/entity-db sel) :eavt  (:db/id sel))
             (->> (d/datoms (d/entity-db sel) :avet  )
                  (filter (fn [[e a v t]]
                            (= v (:db/id sel)))))))]]))]))

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
          ("defn" "let" "when" "and") "m"
          ("first") "s"
          "v"))
      (when (:keyword/value e) "k")
      (when (:string/value e) "l")
      (when (:number/value e) "n")))

(defn token-text
  [e]
  [:span
   #_[:span.inline-tag (str (:db/id e))]
   (or
    (some-> e :symbol/value str)
    (when-let [k (:keyword/value e)]
      (let [kns (namespace k)]
        (if-not kns
          (str k)
          (rum/fragment ":" [:span.kn kns] "/" (name k)))))
    (:string/value e)
    (some-> e :number/value str))])

(rum/defc fcc < dbrx/ereactive
  [e bus indent-prop]
  (-> (or (when (:form/editing e)
            (eb/edit-box e bus))
          (when-let [tc (token-class e)]
            [:span (cond-> {:key (:db/id e)
                            :class (if (:form/highlight e)
                                     (str tc " tk selected")
                                     (str tc " tk"))
                            :on-click (fn [ev]
                                        (.stopPropagation ev)
                                        (core/send! bus [:select (:db/id e)])
                                        #_(async/put! bus [::select-form (:db/id e)]))}
                     (:form/highlight e) (assoc :ref "selected"))
             (token-text e)])
          (when (:coll/type e)
            (display-coll e
                          bus
                          (+ 2 indent-prop)
                          (when (:form/highlight e) "selected")))
          (comment "Probably a retracted entity, do nothing"))
      (do-indent (:form/linebreak e)
                 (computed-indent e indent-prop))))

(defn select-1based-nth-reverse-parent-of-selected-tx
  [db n]
  (let [sel (get-selected-form db)
        rpa (reverse (nav/parents-vec sel))]
    (println "Rpa" rpa)
    (when (< 0 n (inc (count rpa)))
      (move-selection-tx (:db/id sel)
                         (:db/id (nth rpa (dec n)))))))

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

;; history

(defn form-eids-by-edit-tx-desc
  [db]
  (->> (d/rseek-datoms db :avet :form/edited-tx)
       (take-while (fn [[_ a]] (= a :form/edited-tx)))
       (map (fn [[e]] e))))

(defn update-tab-index
  [prev n delta]
  (let [i (+ prev delta)]
    (cond
      (= i -1) (dec n)
      (= i  n) 0
      :else    i)))

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
                            (async/put! bus [::select-form (:db/id parent)]))}
         [:span.code-font
          (when (< i breadcrumbs-max-numeric-label)
            [:span.parent-index (str (inc i))])
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
    
    "z"         [::hop-left]
    "x"         [::hop-right]
    "S-Z"       [::drag-left] 
    
    "Backspace" [::delete-with-movement :move/backward-up]
    "c"         [::duplicate-selected-form]
    ;; "i"         [::indent-form 1]
    ;; "S-I"       [::indent-form -1]
    "Tab"       [::indent-form 1]
    "S-Tab"     [::indent-form -1]
    "i"         [::check-invariants] #_[::reset-indent]
    "Enter"     [::linebreak-form]
    "M-p"       ["placeholder"]
    "M-n"       ["placeholder"]
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
(rum/defc modeline-inner
  [sel tx-report {:keys [text valid] :as edn-parse-state}]
  [:span {:class (str "modeline modeline-size code-font"
                      (when text " editing")
                      (when (and (not (empty? text)) (not valid)) " invalid"))}
   [:span.modeline-echo.modeline-size
    (if text
      (pr-str text)
      (let [r tx-report]
        (str (pr-str (:mutation (:tx-meta r)))
             (when-let [kbd (:kbd (:tx-meta r))]
               (str " " kbd))
             " "
             (:db/current-tx (:tempids r))
             " ("
             (count (:tx-data r))
             ")")))]
   [:span.modeline-content
    (if-not sel
      "(no selection)"
      (str "#" (:db/id sel)
           " "
           (pr-str
            (apply max
                   (for [[_ a _ t] (d/datoms (d/entity-db sel) :eavt (:db/id sel))
                         :when (not= :form/highlight a)]
                     t)))))
    (command-compose-feedback)]])

(rum/defc modeline-portal  < rum/reactive (dbrx/areactive :form/highlight :form/editing)
  [db bus]
  (let [sel (get-selected-form db)
        rpa (reverse (nav/parents-vec sel))
        nn (.getElementById js/document (modeline-portal-id (:db/id (first rpa))))]
    (when nn
      (-> (modeline-inner
           sel
           (rum/react h/last-tx-report)
           (when (:form/editing sel)
             (rum/react eb/editbox-ednparse-state)))
          (rum/portal nn)))))

(declare setup-app)
(rum/defc root-component ;; <  dbrx/ereactive
  [state bus]
  (let [db (d/entity-db state)]
    [:div
     #_(str " T+" (- (js/Date.now) load-time) "ms")
     (breadcrumbs-always db bus)
     #_(h/history-view conn bus)
     (fcc (:state/bar state) bus 0)
     (modeline-portal db bus)]))

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
  #_(prn eb/global-editing-flag)
  (let [tkd (js/performance.now)]
    (when-not @eb/global-editing-flag
      (let [kbd (event->kbd ev)
            {:keys [bindings compose]} @key-compose-state
            
            mut (get bindings kbd)
            next-kbd (conj (or compose []) kbd)]
        #_(prn "Key" kbd mut)
        #_(async/put! -secret-bus [::global-keydown kbd])
        (core/send! @keyboard-bus [:kbd kbd])
        (when (or (some? mut)
                  (and compose (nil? mut)))
          (.preventDefault ev)
          (.stopPropagation ev))
        (if-not mut
          (reset! key-compose-state initial-compose-state)
          (cond
            (vector? mut) (do (reset! key-compose-state initial-compose-state)
                              #_(println "Mut" mut)
                              (core/send! @keyboard-bus
                                          (with-meta mut {:kbd (string/join " " next-kbd)}))
                              #_(async/put! -secret-bus (with-meta mut {:kbd (string/join " " next-kbd)
                                                                        ;; :tkd tkd
                                                                        })))
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



(defn setup-app
  ([]
   (setup-app
    (doto (d/create-conn s/schema)
      (d/transact! init-tx-data))))
  ([conn]
   (let [a (core/app s/schema conn)
         s (sci/init {})]
     (doseq [[m f] mut/dispatch-table]
       (core/register-simple! a m f))
     (doto a
       (core/register-mutation! :kbd
                                (fn [[_ kbd] db bus]
                                  (when-let [mut (:key/mutation (d/entity db [:key/kbd kbd]))]
                                    (core/send! bus [mut]))))
       (core/register-mutation! :eval-sci
                                (fn [_ db bus]
                                  (let [et (->> (get-selected-form db)
                                                (move/move :move/most-upward))
                                        c (->> (e/->form et)
                                               (pr-str))]
                                    (println "Eval sci" c)
                                    (try
                                      (let [ans (sci/eval-string* s c)]
                                        (core/send! bus [:eval-result et ans]))
                                      (catch :default e
                                        (js/console.log "SCI exception" e))))))
       (core/register-mutation! :form/highlight
                                (fn [_ db bus]
                                  (println "Highlight changed" )))))))

(comment
  (register-sub ::select-form (->mutation select-form-tx ))
  (register-sub ::raise-selected-form (->mutation (fn [db] (edit/form-raise-tx (get-selected-form db)))))
  (register-sub ::exchange-with-next (->mutation (fn [db] (edit/exchange-with-next-tx (get-selected-form db)))))
  (register-sub ::exchange-with-previous (->mutation (fn [db] (edit/exchange-with-previous-tx (get-selected-form db)))))

  (register-sub ::duplicate-selected-form (->mutation insert-duplicate-tx))
  (register-sub ::wrap-selected-form (->mutation (fn [db ct] (form-wrap-tx (get-selected-form db) ct))))
  (register-sub ::wrap-and-edit-first (->mutation (fn [db ct] (wrap-and-edit-first-tx (get-selected-form db) ct))))
  (register-sub :edit/finish (->mutation (fn [db text] (eb/finish-edit-tx db (d/entid db [:form/editing true]) text))))
  (register-sub :edit/reject (->mutation (fn [db] (eb/reject-edit-tx db (d/entid db [:form/editing true])))))
  (register-sub :edit/wrap   (->mutation (fn [db ct value] (eb/wrap-edit-tx db (d/entid db [:form/editing true]) ct value))))
  (register-sub :edit/finish-and-move-up (->mutation (fn [db text] (eb/finish-edit-and-move-up-tx db (d/entid db [:form/editing true]) text))))
  (register-sub :edit/finish-and-edit-next-node (->mutation (fn [db text] (eb/finish-edit-and-edit-next-tx db (d/entid db [:form/editing true]) text))))


  (register-sub ::edit-selected (->mutation edit-selected-tx))
  (register-sub ::edit-new-wrapped (->mutation edit-new-wrapped-tx))
  (register-sub ::move (->mutation move/movement-tx))
  (register-sub ::repeat-move (->mutation move/repeat-movement-tx))
  (register-sub ::delete-with-movement (->mutation move-and-delete-tx))
  (register-sub ::indent-form (->mutation indent-selected-form-tx))
  (register-sub ::reset-indent (->mutation (fn [db] [{:db/id (:db/id (get-selected-form db)) :form/indent 0}])))
  (register-sub ::linebreak-form (->mutation linebreak-selected-form-tx))
  (register-sub ::recursively-set-indent (->mutation recursively-set-indent-tx))
  (register-sub ::check-invariants (->mutation (fn [db] (invar/check (get-selected-form db)))))
  (register-sub ::import-data-toplevel (->mutation import-formdata-tx))

  (register-sub ::revert-last (->mutation (fn [db] (h/undo-last-tx!))))

  (register-sub ::hop-left (->mutation (fn [db] (hop* seq-previous db))))
  (register-sub ::hop-right (->mutation (fn [db] (hop* seq-next db))))
  (register-sub ::drag-left (->mutation (fn [db] (drag* db seq-previous))))
  (register-sub ::select-1based-nth-reverse-parent (->mutation select-1based-nth-reverse-parent-of-selected-tx))
  
  #_ (register-sub ::insert-editing (->mutation edit/insert-editing-tx))
  (register-sub ::insert-editing (->mutation edit/insert-editing-tx))
  (register-sub ::scroll-into-view (fn [[_]] (scroll-within-chain! (d/entity @conn [:form/highlight true])))))


;; last/previous edit position switcher
#_(register-sub ::global-keydown
              (let [tab-index (atom 0)
                    tab-seq   (atom nil)]
                (fn [[_ k]]
                  (case k
                    "M-Alt"
                    nil
                    
                    (= "M-p" "M-n")
                    (let [ts (or @tab-seq
                                 (reset! tab-seq (vec (form-eids-by-edit-tx-desc @conn))))]
                      (when-not (= 0 (count ts))
                        (pub! [::select-form
                               (nth ts
                                    (swap! tab-index
                                           update-tab-index
                                           (count ts)
                                           (case k "M-p" 1 "M-n" -1)))])))
                    
                    (do (reset! tab-index 0)
                        (reset! tab-seq nil))))))

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
                            [:p (with-out-str (cljs.pprint/pprint (d/touch (get-selected-form db-after))))]
                            
                            ]})))
                     (throw (ex-info "No mutation" {:m m}))))
                 init-report
                 muts)]
    [:div {:style {:display "flex"
                   :flex-direction "row"
                   :width "1000px"
                   }}
     [:div {:style {:display "flex" :flex-direction "column"}}
      (for [[i m {:keys [failure input-tx db-after tx-data]} other] (map vector (range) (cons "initial" muts) reports (cons nil reports))]
        (if failure
          [:div failure]
          [:div {:key i :style {:display :flex
                                :flex-direction :column
                                :margin-top "2ex"
                                :border "1px solid #ae81ff"}}
           [:span (str "#" i " " (pr-str m))]
           [:div {:style {:border "1px solid #777"}}
            (when other (root-component (d/entity (:db-after other) ::state) core/blackhole))]
           [:div {:style {:border "1px solid #777"}}
            (root-component (d/entity db-after ::state) core/blackhole)]
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
  (->> [[[:insert-right] [:edit/finish "x"]]
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
        se (d/entity @conn ::state)
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
     (root-component se bus)
     #_(player-mutation-view mv)]))

(rum/defc debug-component
  []
  [:div {:style {:margin-top "2ex"}  }
   (player
      '[a b c ^:form/highlight [a s d f] [l e l] [O] d]
      (some-random-mutations 10000))
   
   #_(example
    '[a b c ^:form/highlight [a s d f] [l e l] [O] d]
    (some-random-mutations 199))
   #_(example
    '[a ^:form/highlight b c [a a a]]
    [[:float] [:delete-left] [:flow-right] [:float]]
    #_[[:last]])])

(defn  ^:dev/after-load init []
  (js/document.removeEventListener "keydown" global-keydown true)
  (js/document.removeEventListener "keyup" global-keyup true)
  
  
  (js/document.addEventListener "keydown" global-keydown true)
  (js/document.addEventListener "keyup" global-keyup true)
  (h/clear!)
  #_(dbrx/reset-for-reload!)
  (let [{:keys [conn bus]} (setup-app)
        se (d/entity @conn ::state)] 
    
    #_(run! (partial core/send! bus)
            [[:flow-left]
             [:raise]])
    
    #_(let [hz 4]
        (async/go-loop [n 0]
          (async/<! (async/timeout (/ 1000.0 hz)))
          (println "Waiter " n)
          (when (< n 99)
            (recur (inc n)))))
    
    (println "Reset keyhboard bus" bus)
    (reset! keyboard-bus bus)
    
    (rum/mount
     #_(debug-component)
     (root-component se bus)
     (.getElementById js/document "root"))))

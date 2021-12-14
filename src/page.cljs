(ns page
  (:require
   [clojure.edn :as edn]
   [embed :as e]
   [debug :as debug]
   [svg-helpers :as s]
   [tx-history :as h]
   [goog.string :as gstring]
   [goog.functions :as gf]
   [datascript.core :as d]
   [clojure.string :as string]
   [rum.core :as rum]
   [cljs.core.async :as async]
   [db-reactive :as dbrx]

   [comp.cons :as cc]
   [comp.edit-box :as eb]
   [comp.keyboard :as ck]
   [comp.hex :as chex]

   [cmd.move :as move]
   [cmd.edit :as edit]
   [cmd.invar :as invar]
   
   [core :refer [get-selected-form
                 move-selection-tx] ]
   
   )
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop]]
   [macros :refer [macro-slurp]]))

(def load-time (js/Date.now))

(def schema
  (merge
   e/form-schema
   {:state/undowhat {}
    :state/bar {:db/valueType :db.type/ref}
    
    :history/item {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
    :history-item/mutation {:db/valueType :db.type/ref}
    :history-item/tx {:db/valueType :db.type/ref}
    
    
    ;; :state/selected-form {:db/valueType :db.type/ref}
    :form/editing {:db/unique :db.unique/identity} 
    :form/edit-initial {}
    :form/highlight {:db/unique :db.unique/identity}
    :form/indent {}
    :form/linebreak {}
    :form/edited-tx {:db/valueType :db.type/ref}

    :edit/of {:db/valueType :db.type/ref}
    
    :chain/selection {:db/valueType :db.type/ref}
    }))

(def test-form-data-bar
  '[
    ["Chain 1"
            (defn thing
              [a b c ^{:form/highlight true} [l e l] [O] d] blah)]
    #_["Chain 2"
            (defn other-thing
              [a b c
               [d e ^{:meta-thing "Thningy"}  X]
               p
               #_[l e l]
               [O] d] blah)]
    #_["Chain 3"
     (defn register-sub
       [topic action]
I       (let [ch (async/chan)]
         (async/sub the-pub topic ch)
         (go-loop []
           #_(action (async/<! ch))
           (try
             (action (async/<! ch))  
             (catch js/Error e
               (println "Error in " topic)
               (prn e)
               (js/console.log e)))
           (recur))))]
    
    
    
    ])

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
                        ;; "history"
                        ;; "timetravel"
                        }
       :seq/first {:db/id "label"
                   :string/value "Keyboard"}
       :seq/next {:seq/first {:db/id "keyboard"
                              :coll/type :keyboard}
                  :seq/next {:seq/first "inspect"
                             ;; :seq/next {:seq/first "history"}
                             
                             }
                  
                  ;; :seq/next {:seq/first "timetravel"}
                  #_:seq/next #_{:seq/first {:db/id "hexes"
                                             :coll/type :hexes}}}}]))
   :db/id "bar"
   :coll/type :bar))


(def conn
  (doto (d/create-conn schema)
    (d/transact! (let [txe (test-form-data-tx (concat
                                               (map e/->tx test-form-data-bar)
                                               #_[(e/string->tx-all (macro-slurp "src/embed.cljc"))]
                                               
                                               #_[(e/string->tx-all (macro-slurp "src/cmd/edit.cljc"))]))
                       timetravel-placeholders (e/->tx (vec (range 9)))]
                   [{:db/ident ::state
                     :state/bar (:db/id txe)}
                    {:db/ident ::history
                     :db/id "history"
                     :coll/type :vec
                     :seq/first {:string/value "Eod of history" :coll/_contains "history"}
                     :seq/next {:seq/first {:string/value "Really end " :coll/_contains "history"}}}
                    {:db/ident ::inspect
                     :db/id "inspect"
                     :coll/type :inspect
                     :seq/first {:string/value "No inspect" :coll/_contains "history"}}
                    {:db/ident ::timetravel
                     :db/id "timetravel"
                     :coll/type :timetravel
                     :seq/first (:db/id timetravel-placeholders)}
                    timetravel-placeholders
                    txe
                    
                    ]))
    (d/listen!
     (fn [tx-report]
       (doto tx-report
         (dbrx/tx-listen-fn)
         (h/tx-listen-fn)
         (invar/tx-listen-fn))))))




(def bus (async/chan))
(def the-pub (async/pub bus first))

(defn register-sub
  [topic action]
  (let [ch (async/chan)]
    (async/sub the-pub topic ch)
    (go-loop []
      #_(action (async/<! ch))
      (try
        (action (async/<! ch))  
        (catch js/Error e
          (println "Error in " topic)
          (prn e)
          (js/console.log e)))
      (recur))))

(defn pub! [e]
  (async/put! bus e))

(defn ->mutation
  [tx-fn]
  (fn [[_ & args :as mutation]]
    (let [db @conn]
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

(defn parents-array
  [e]
  (->> e
       (iterate (partial move/move :move/up))
       (take-while some?)
       (clj->js)))

(defn reverse-parents-array
  [e]
  (.reverse (parents-array e)))

(defn scroll-within-chain*
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
                pos (.-scrollTop el-chain)
                ]
            ;; offset + 
            #_(println
             "Outer" outer-height
             "Inner" inner-height
             "Pos" pos
             "Offset" offset
             "Space left"
           
           
           
             )
          
          
            (.scrollTo el-chain
                       #js{:top offset
                           :behavior "smooth"})))
        
        #_(set! (.-scrollTop el-chain)
                (- (.-offsetTop el-sel)
                   (/ (.-clientHeight el-chain)
                      2)))))))

(def scroll-within-chain!
  (gf/throttle scroll-within-chain* 222))

(register-sub ::scroll-into-view
              (fn [[_]]
                (scroll-within-chain! (d/entity @conn [:form/highlight true]))))



(defn scroll-ref-into-view-after-render 
  [ref-name]
  {:did-update
   (fn [{:keys [subsequent] :as state}]
     (when-let [sel (some-> state :rum/react-component (.-refs) (aget ref-name))]
       #_(println "Srivar"
                (:db/id (first (:rum/args state))))
       #_(js/console.log sel)
       (scroll-within-chain! (first (:rum/args state)))
       )
     
     #_(if-not subsequent
         (assoc state :subsequent true)
         (do
           (println "Subsdeq")
           state))
     state
     )})



#_(declare edit-box)

#_(rum/defc token-component ;; < rum/static
  [classes eid child ref]
  (println eid  "Classes" classes "Ref" ref)
  [:span (cond-> {:key eid
                  :class (str "tk " classes)
                  :on-click (fn [ev]
                              (.stopPropagation ev)
                              (pub! [::select-form eid]))}
           ;; (some? ref) (assoc :ref ref)
           )
   child
   #_(-> text 
         ;; replace with non-breaking hyphen, lmao
         (gstring/replaceAll "-" "‑"))])

(defn token-component
  [classes eid child ref]
  [:span (cond-> {:key eid
                  :class (str "tk " classes)
                  :on-click (fn [ev]
                              (.stopPropagation ev)
                              (pub! [::select-form eid]))}
           (some? ref) (assoc :ref ref))
   child
   #_(-> text 
         ;; replace with non-breaking hyphen, lmao
         (gstring/replaceAll "-" "‑"))])



(declare fcc)

(defn breadcrumbs-portal-id [eid] (str eid "bp"))
(defn modeline-portal-id [eid] (str eid "mp"))

(rum/defc top-level-form-component < dbrx/ereactive
  [e]
  [:div.form-card {:ref "top-level"}
   #_[:span.form-title.code-font (str "#" (:db/id e)
                                      " T+" (- (js/Date.now) load-time) "ms")]
   [:div.bp {:id (breadcrumbs-portal-id (:db/id e))}]
   [:div.top-level-form.code-font (fcc e 0)]
   [:div {:id (modeline-portal-id (:db/id e))}]
   (if (= :list (:coll/type e))
     [:div {:style {:width "1800px"}}
      (cc/svg-viewbox e)])])

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

(defmulti display-coll (fn [c indent]
                         (:coll/type c)))

(defmethod display-coll :default [c _]
  [:code (pr-str c)])

(defn delimited-coll
  [e indent classes open close]
  [:span (cond-> {:class (str "c " classes)}
           classes (assoc :ref "selected"))
   [:span.d open]
   (for [x (e/seq->vec e)]
     (rum/with-key (fcc x (computed-indent e indent))
       (:db/id x)))
   [:span.d close]])

(defmethod display-coll :list [c i cs] (delimited-coll c i cs "(" ")"))
(defmethod display-coll :vec  [c i cs] (delimited-coll c i cs "[" "]"))
(defmethod display-coll :map  [c i cs] (delimited-coll c i cs "{" "}"))
(defmethod display-coll :set  [c i cs] (delimited-coll c i cs "#{" "}"))

(defmethod display-coll :chain [chain i classes]
  [:div.chain
   {:key (:db/id chain)
    :ref "chain"
    :class classes}
   (for [f (e/seq->vec chain)]
     (-> (top-level-form-component f)
         (rum/with-key (:db/id f))))])

(defmethod display-coll :bar [bar i]
  [:div.bar
    (for [chain-head (e/seq->vec bar)]
      (-> (fcc chain-head i)
          (rum/with-key (:db/id chain-head))))])

(defmethod display-coll :keyboard [k i]
  [:div.display-keyboard
   {:on-click #(pub! [::select-form (:db/id k)])}
   (ck/keyboard-diagram)])

(defmethod display-coll :hexes [k i]
  [:div
   (chex/main)])

(rum/defc inspector < (dbrx/areactive :form/highlight :form/edited-tx)
  [db]
  (let [sel (get-selected-form db)]
    [:div
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

(defmethod display-coll :inspect [k i]
  (inspector (d/entity-db k)))

(defmethod display-coll :timetravel [tt i]
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
  (or (some-> e :symbol/value str)
      (when-let [k (:keyword/value e)]
        (let [kns (namespace k)]
          (if-not kns
            (str k)
            (rum/fragment ":" [:span.kn kns] "/" (name k)))))
      (:string/value e)
      (some-> e :number/value str)))

(rum/defc fcc < dbrx/ereactive (scroll-ref-into-view-after-render "selected")
  [e indent-prop]
  (-> (or (when (:form/editing e)
            (eb/edit-box bus e))
          (when-let [tc (token-class e)]
            (token-component
             (cond-> tc (:form/highlight e) (str " selected"))
             (:db/id e)
             (token-text e)
             (when (:form/highlight e) "selected")))
          (when (:coll/type e)
            (display-coll e
                          (+ 2 indent-prop)
                          (when (:form/highlight e) "selected")))
          (comment "Probably a retracted entity, do nothing"))
      (do-indent (:form/linebreak e)
                 (computed-indent e indent-prop))))

(defn select-form-tx
  [db eid]
  (move-selection-tx (:db/id (get-selected-form db))
                     eid))

(register-sub ::select-form (->mutation select-form-tx ))

;; list modifications

(register-sub ::raise-selected-form (->mutation edit/raise-selected-form-tx))

(register-sub ::exchange-with-previous
              (->mutation
               (fn [db]
                 (let [sel    (get-selected-form db)
                       spine  (first (:seq/_first sel))
                       prev   (some-> spine :seq/_next first)
                       next   (some-> spine :seq/next)
                       parent (first (:coll/_contains sel))]
                   (println "Prev" prev 'PArent parent )
                   (when (and prev parent)
                     [[:db/add (:db/id prev) :seq/first (:db/id sel)]
                      [:db/add (:db/id spine) :seq/first (:db/id (:seq/first prev))]
                      [:db/add (:db/id parent) :form/edited-tx :db/current-tx]])))))

(register-sub ::exchange-with-next
              (->mutation
               (fn [db]
                 (let [sel    (get-selected-form db)
                       spine  (first (:seq/_first sel))
                       next   (some-> spine :seq/next)
                       parent (first (:coll/_contains sel))]
                   (when (and next parent)
                     [[:db/add (:db/id next) :seq/first (:db/id sel)]
                      [:db/add (:db/id spine) :seq/first (:db/id (:seq/first next))]
                      [:db/add (:db/id parent) :form/edited-tx :db/current-tx]])))))

(defn form-duplicate-tx
  {:this-ms-masdf "Okay"}
  [e]
  (letfn [(dup-spine [parent head] 
            (if-let [x (:seq/first head)]
              (cond-> {:seq/first (assoc (form-duplicate-tx x) :coll/_contains parent )}
                (:seq/next head) (assoc :seq/next (dup-spine parent (:seq/next head))))))]
    (cond 
      (:symbol/value e)  {:symbol/value (:symbol/value e)}
      (:keyword/value e) {:keyword/value (:keyword/value e)}
      (:string/value e)  {:string/value (:string/value e)}
      (:number/value e)  {:number/value (:number/value e)}
      (:coll/type e)     (let [us (e/new-tempid)]
                           (merge (cond-> {:db/id us :coll/type (:coll/type e)}
                                    (some? (:form/indent e)) (assoc :form/indent (:form/indent e))
                                    (some? (:form/linebreak e)) (assoc :form/linebreak (:form/linebreak e)))
                                  (dup-spine us e))))))

(defn insert-duplicate-tx
  [db]
  (let [sel (get-selected-form db)
        new-node (-> (form-duplicate-tx sel)
                     (update :db/id #(or % "dup-leaf")))]
    (into [new-node]
          (edit/insert-after-tx sel new-node))))

(register-sub ::duplicate-selected-form (->mutation insert-duplicate-tx))

(defn form-wrap-tx
  [e ct]
  (into [{:db/id "newnode"
          :coll/type ct
          :coll/contains (:db/id e)
          :seq/first (:db/id e)}]
        (edit/form-overwrite-tx e "newnode")))

(register-sub ::wrap-selected-form
              (->mutation
               (fn [db ct]
                 (form-wrap-tx (get-selected-form db) ct))))



(register-sub
 ::wrap-and-edit-first
 (->mutation
  (fn [db ct] 
    (let [sel (get-selected-form db)
          spine (first (:seq/_first sel))
          coll (first (:coll/_contains sel))
          new-node {:db/id "first"
                    :coll/type ct
                    :coll/_contains (:db/id coll)
                    :seq/first {:db/id "funcname"
                                :coll/_contains "first"
                                :form/editing true}
                    :seq/next {:seq/first (:db/id sel)}}]
      (into [new-node]
            (concat
             (edit/form-overwrite-tx sel "first")
             (move-selection-tx (:db/id sel) "first")))))))


;; editing


(register-sub :edit/finish (->mutation (fn [db text] (eb/finish-edit-tx db (d/entid db [:form/editing true]) text))))
(register-sub :edit/reject (->mutation (fn [db] (eb/reject-edit-tx db (d/entid db [:form/editing true])))))
(register-sub :edit/wrap   (->mutation (fn [db ct value] (eb/wrap-edit-tx db (d/entid db [:form/editing true]) ct value))))
(register-sub :edit/finish-and-move-up (->mutation (fn [db text] (eb/finish-edit-and-move-up-tx db (d/entid db [:form/editing true]) text))))
(register-sub :edit/finish-and-edit-next-node (->mutation (fn [db text] (eb/finish-edit-and-edit-next-tx db (d/entid db [:form/editing true]) text))))



(register-sub ::insert-editing (->mutation edit/insert-editing-tx))

(defn begin-edit-existing-node-tx
  [e]
  [[:db/add (:db/id e) :form/editing true]])

(defn edit-selected-tx
  [db]
  (let [sel (get-selected-form db)]
    (when-not (:coll/type sel)
     (begin-edit-existing-node-tx sel))))

(register-sub ::edit-selected (->mutation edit-selected-tx))

(defn edit-new-wrapped-tx
  [db coll-type init]
  (let [sel (get-selected-form db)
        new-node {:db/id "newnode"
                  :coll/type coll-type
                  :coll/contains "inner" 
                  :seq/first {:db/id "inner"
                              :coll/_contains "newnode"
                              :form/edit-initial (or init "")
                              :form/editing true}}]
    (into [new-node]
          (concat (edit/insert-after-tx sel new-node)
                  (move-selection-tx (:db/id sel) "inner")))))

(register-sub ::edit-new-wrapped (->mutation edit-new-wrapped-tx))



;; movements?


(register-sub ::move (->mutation move/movement-tx))

(register-sub ::repeat-move (->mutation move/repeat-movement-tx))

(defn move-and-delete-tx
  [db movement-type]
  (let [src (get-selected-form db)]
    (when-let [dst (move/move movement-type src)]
      (concat (edit/form-delete-tx src)
              [[:db/add (:db/id dst) :form/highlight true]]))))

(register-sub ::delete-with-movement (->mutation move-and-delete-tx))

(defn indent-selected-form-tx
  [db delta]
  (let [sel (get-selected-form db)]
    [[:db/add (:db/id sel)
      :form/indent (+ delta
                      (-> (:form/indent sel)
                          (or 0)))]]))

(register-sub ::indent-form (->mutation indent-selected-form-tx))

(register-sub ::reset-indent (->mutation
                              (fn [db]
                                [{:db/id (:db/id (get-selected-form db))
                                  :form/indent 0}])))

(defn linebreak-selected-form-tx
  [db]
  (let [sel           (get-selected-form db)
        parent-indent (:form/indent (move/move :move/up sel))]
    #_(println "PArentindent" parent-indent)
    [[:db/add (:db/id sel) :form/linebreak (not (:form/linebreak sel))]
     (when parent-indent
       [:db/add (:db/id sel) :form/indent parent-indent])]))

(register-sub ::linebreak-form (->mutation linebreak-selected-form-tx))



(defn recursively-set-indent-tx
  [db indent]
  (->> (get-selected-form db)
       (tree-seq :coll/type e/seq->vec)
       (next)
       (keep (fn [e]
               (when (:coll/type e)
                 [:db/add (:db/id e) :form/linebreak indent])))))

(register-sub ::recursively-set-indent (->mutation recursively-set-indent-tx))
(register-sub ::check-invariants
              (->mutation (fn [db]
                            (println "Cinvar" (get-selected-form db))
                            (invar/check (get-selected-form db)))))


(register-sub ::execute-selected-as-mutation
              (->mutation (fn [db]
                            (let [m (e/->form (get-selected-form db))]
                              (if (vector? (first m))
                                (run! pub! m)
                                (pub! m)))
                            nil)))



(defn import-formdata-tx
  [db data]
  (let [sel (get-selected-form db)
        top-level (first (reverse-parents-array sel))
        chain (some-> top-level :coll/_contains first)
        new-node (-> (e/->tx data)
                     (update :db/id #(or % "import-formdata-tx")))]

    (into [new-node]
          (concat (edit/insert-before-tx top-level new-node)
                  (move-selection-tx (:db/id sel) (:db/id new-node))))))

(register-sub ::import-data-toplevel (->mutation import-formdata-tx))

(register-sub ::reify-extract-selected
              (->mutation (fn [db]
                            (let [sel (get-selected-form db)]
                             (import-formdata-tx
                              db
                              (into {:db/id (:db/id sel)}
                                    (for [[a v] (d/touch sel)]
                                      (cond
                                        (= :db.cardinality/many (:db/cardinality (get schema a)))
                                        [a (into #{} (map :db/id) v)]
                                       
                                        (= :form/highlight a)
                                        nil
                                       
                                        :else
                                        [a v]))))))))

(register-sub
 ::reify-parse-selected
 (->mutation
  (fn [db]
    (let [sel (get-selected-form db)]
     (when-let [sv (:string/value sel)]
       (let [txe (e/string->tx sv)]
         (into [txe]
               (concat
                (move-selection-tx (:db/id sel) (:db/id txe))
                (edit/form-replace-tx sel txe)))))))))



(register-sub ::reify-last-mutation
              (->mutation (fn [db]
                            (let [r @h/last-tx-report]
                              (import-formdata-tx
                               db
                               {:tx-data (mapv vec (:tx-data r) )
                                :tx-meta (:tx-meta r)
                                :tempids (:tempids r)})))))

(register-sub ::revert-last
              (->mutation (fn [db]
                            (println "Revertlast")
                            (h/undo-last-tx!))))

(register-sub ::eval
              (->mutation (fn [db]
                            (let [pa (some-> (get-selected-form db) (parents-array))
                                  [mut & args] (first (keep :form/eval-action pa))]
                              (prn 'mut mut 'args args)
                              
                              ))))
(defn select-chain-tx
  [db sel]
  (let [top-level (first (reverse-parents-array sel))
        chain (some-> top-level :coll/_contains first)]
    (println "Selectchaintx" chain)
    (concat
     (select-form-tx db (:db/id chain))
     [{:db/id (:db/id chain)
       :chain/selection (:db/id sel)}])))

(defn restore-chain-selection-tx
  [db chain]
  (when-let [prev-sel (:chain/selection chain)]
    (select-form-tx db (:db/id prev-sel))))


(register-sub ::select-chain
              (->mutation (fn [db]
                            (let [sel (get-selected-form db)]
                              (if (= :chain (:coll/type sel))
                                (restore-chain-selection-tx db sel)
                                (select-chain-tx db sel))))))

(defn hop-target
  [chain]
  (or (:chain/selection chain) chain))

(defn hop*
  [db mover]
  (let [sel (get-selected-form db)]
    (if (= :chain (:coll/type sel))
      (when-let [hop-chain (mover sel)]
        (move-selection-tx (:db/id sel) (:db/id (hop-target hop-chain))))
      (let [chain (some-> sel reverse-parents-array first :coll/_contains first)
            hop-chain (mover chain)]
        (concat
         (move-selection-tx (:db/id sel) (:db/id (hop-target hop-chain)))
         [{:db/id (:db/id chain)
           :chain/selection (:db/id sel)}])))))

(defn seq-previous
  [e]
  (some-> e :seq/_first first :seq/_next first :seq/first))

(defn seq-next
  [e]
  (some-> e :seq/_first first :seq/next :seq/first))

(register-sub ::hop-left  (->mutation (fn [db] (hop* db seq-previous))))
(register-sub ::hop-right (->mutation (fn [db] (hop* db seq-next))))

(defn drag*
  [db mover]
  (let [sel (get-selected-form db)]
    (when-not (= :chain (:coll/type sel))
      (when-let [target (some-> sel reverse-parents-array first :coll/_contains first mover hop-target)]
        (let [newnode {:db/id "dragnew" :string/value "Drag new node"}]
          (concat
           [newnode]
           (edit/insert-before-tx target newnode)
           
           #_(edit/form-replace-tx newnode sel)))))))

(register-sub ::drag-left (->mutation (fn [db] (drag* db seq-previous))))



(defn select-1based-nth-reverse-parent-of-selected-tx
  [db n]
  (let [sel (get-selected-form db)
        rpa (reverse-parents-array sel)]
    (println "Rpa" rpa)
    (when (< 0 n (inc (count rpa)))
      (move-selection-tx (:db/id sel)
                         (:db/id (nth rpa (dec n)))))))

(register-sub ::select-1based-nth-reverse-parent (->mutation select-1based-nth-reverse-parent-of-selected-tx))

(register-sub ::extract-to-new-top-level
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


(defn link-to-form-by-id
  [eid children]
  [:a {:key eid
       :href "#"
       :on-click #(do (.preventDefault %)
                      (pub! [::select-form eid]))}
   children])

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
  [rpa]
  [:ul.parent-path
   (for [i (range (dec (count rpa)))]
     (let [parent (nth rpa i)]
       [:li {:key i}
        (link-to-form-by-id (:db/id parent)
                            [:span.code-font
                             (when (< i breadcrumbs-max-numeric-label)
                               [:span.parent-index (str (inc i))])
                             (if (= :list (:coll/type parent))
                               (or (some-> parent :seq/first :symbol/value) "()")
                               (case (:coll/type parent) :vec "[]" :map "{}" "??"))])]))])

(rum/defc breadcrumbs-always < (dbrx/areactive :form/highlight :form/edited-tx)
  [db]
  (when-let [sel (get-selected-form db)]
    (let [rpa (some-> sel (reverse-parents-array))
          top-level (first rpa)]
     (let [node-id (breadcrumbs-portal-id (:db/id top-level))]
       (when-let [n (.getElementById js/document node-id)]
         (rum/portal
          (parent-path
           (if-not (= (:db/id sel) (:db/id top-level))
             rpa
             (el-bfs sel 9)))
          n))))))


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
    "M-x"       [::execute-selected-as-mutation]
    "S-A"       {"a" [::linebreak-form]}
    "S-R"       {"f" [::reify-extract-selected]
                 "m" [::reify-last-mutation]
                 "p" [::reify-parse-selected]}
    "C-z"       [::revert-last]}
   (into {}
         (for [i (range  breadcrumbs-max-numeric-label)]
           [(str (inc i)) [::select-1based-nth-reverse-parent (inc i)]]))))





(rum/defc key-bindings-table []
  [:table
   [:thead
    [:tr
     [:td {:style {:width "8em"}} "Key"]
     [:td {:style {:width "30em"}} "Message (click to send)"]]]
   [:tbody
    {} 
    (for [[k msg] (sort-by (comp  pr-str val) (seq special-key-map))]
      [:tr {:key k}
       [:td [:code k]]
       [:td [:button {:style {:font-family "monospace"}
                      :on-click #(pub! msg) }
             (pr-str msg)]]])]])

(rum/defc all-datoms-table < rum/reactive []
  (debug/datoms-table-eavt* (d/datoms (rum/react conn) :eavt)))

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
  [db]
  (let [sel (get-selected-form db)
        rpa (reverse-parents-array sel)
        ;; _ (prn 'MP_RPA rpa)
        nn (.getElementById js/document (modeline-portal-id (:db/id (first rpa))))]
    #_(prn nn)
    (when nn
      (-> (modeline-inner
           sel
           (rum/react h/last-tx-report)
           (when (:form/editing sel)
             (rum/react eb/editbox-ednparse-state)))
          (rum/portal nn)))))

(rum/defc edit-history < (dbrx/areactive :form/edited-tx)
  [db]
  (let [sel-eid (d/entid db  [:form/highlight true])]
    [:ul
     (for [e (form-eids-by-edit-tx-desc db)]
       [:li {:key e}
        (link-to-form-by-id
         e
         (str "Edited #" e
              (when (= e sel-eid)
                " (Selected)")))])]))



(rum/defc root-component < dbrx/ereactive
  [state]
  (let [db (d/entity-db state)]
    [:div
     #_(str " T+" (- (js/Date.now) load-time) "ms")
     (breadcrumbs-always db)
     (h/history-view conn bus)
     (fcc (:state/bar state) 0)
     #_(modeline-portal db)]))

(defn event->kbd
  [^KeyboardEvent ev]
  (str (when (.-altKey ev) "M-")
       (when (.-ctrlKey ev ) "C-")
       (when (.-shiftKey ev) "S-")
       (.-key ev)))

(def initial-compose-state {:bindings special-key-map :compose nil})
(def key-compose-state (atom initial-compose-state))

(defn global-keydown*
  [ev]
  #_(prn eb/global-editing-flag)
  (let [tkd (js/performance.now)]
    (when-not @eb/global-editing-flag
      (let [kbd (event->kbd ev)
            {:keys [bindings compose]} @key-compose-state
            mut (get bindings kbd)
            next-kbd (conj (or compose []) kbd)]
        (prn "Key" kbd)
        (pub! [::global-keydown kbd])
        (when (or (some? mut)
                  (and compose (nil? mut)))
          (.preventDefault ev)
          (.stopPropagation ev))
        (if-not mut
          (reset! key-compose-state initial-compose-state)
          (cond
            (vector? mut) (do (reset! key-compose-state initial-compose-state)
                              (pub! (with-meta mut {:kbd (string/join " " next-kbd)
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



;; last/previous edit position switcher
(register-sub ::global-keydown
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

(defn debug-component
  [db]
  (debug/db-viewer db))

(defn  ^:dev/after-load init []
  (js/document.removeEventListener "keydown" global-keydown true)
  (js/document.removeEventListener "keyup" global-keyup true)
  
  
  (js/document.addEventListener "keydown" global-keydown true)
  (js/document.addEventListener "keyup" global-keyup true)
  (h/clear!)
  (dbrx/reset-for-reload!)
  (let [se (d/entity @conn ::state)] 
    (rum/mount
     #_(debug-component @conn)
     (root-component (d/entity @conn ::state))
     (.getElementById js/document "root"))))

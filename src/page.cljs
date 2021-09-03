(ns page
  (:require
   [clojure.edn :as edn]
   [embed :as e]
   [debug :as debug]
   [svg-helpers :as s]
   [tx-history :as h]
   [comp.cons :as cc]
   [goog.string :as gstring]
   [comp.keyboard :as ck]
   [datascript.core :as d]
   [clojure.string :as string]
   [db-reactive :as dbrx]
   [rum.core :as rum]
   [cljs.core.async :as async])
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop]]))

(def load-time (js/Date.now))

(def schema
  (merge
   e/form-schema
   {:state/display-form {:db/valueType :db.type/ref
                         :db/cardinality :db.cardinality/many}
    :state/bar {:db/valueType :db.type/ref}
    
    
    ;; :state/selected-form {:db/valueType :db.type/ref}
    :form/editing {:db/unique :db.unique/identity} 
    :form/edit-initial {}
    :form/highlight {:db/unique :db.unique/identity}
    :form/indent {}
    :form/linebreak {}
    :form/edited-tx {:db/valueType :db.type/ref}

    :edit/of {:db/valueType :db.type/ref}

    }))

(def test-form-data-bar
  '[
    ["Chain 1"
            (defn thing
              [a b c [l e l] [O] d] blah)]
    ["Chain 2"
            (defn other-thing
              [a b c
               [d e ^{:meta-thing "Thningy"}  X]
               p
               #_[l e l]
               [O] d] blah)]
    ["Chain 3"
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
           (recur))))]
    
    
    
    ])

(defn test-form-data-tx
  [chains]
  (assoc
   (e/seq-tx (for [ch chains]
               (assoc (e/->tx ch)
                      :coll/type :chain
                      :coll/_contains "bar")))
   :db/id "bar"
   :coll/type :bar))

(def conn
  (doto (d/create-conn schema)
    (d/transact! (let [txe (test-form-data-tx test-form-data-bar)]
                   [{:db/ident ::state
                     ;; :state/display-form (:db/id txe)
                     :state/bar (:db/id txe)}
                    txe]))))




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
    (when-let [tx-data (apply tx-fn @conn args)]
      ;; transact metadata on db/current-tx?  run out of memory?
      (d/transact! conn tx-data (merge {:mutation mutation
                                        :input-tx-data tx-data}
                                       (meta mutation))))))

(d/listen! conn
           (fn [tx-report]
             (doto tx-report
               (dbrx/tx-listen-fn)
               (h/tx-listen-fn))))

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

(register-sub ::scroll-into-view
              (fn [[_]]
                (let [sel (d/entid @conn  [:form/highlight true])]
                  (doseq [c (aget dbrx/ereactive-components-by-eid sel)]
                    (some-> (.-refs c)
                            (aget "selected")
                            (scroll-element-to-window-center!))))))

(defn focus-ref-on-mount
  [ref-name]
  {:did-mount (fn [state]
                (some-> state :rum/react-component (.-refs) (aget ref-name) (.focus))
                state)})

(defn scroll-ref-into-view-after-render
  [ref-name]
  {:after-render (fn [state]
                   #_(some-> state :rum/react-component (.-refs) (aget ref-name) (.scrollIntoView false))
                   (some-> state :rum/react-component (.-refs) (aget ref-name)
                           (scroll-element-to-window-center!))
                   state)})

(def global-editing-flag (atom false))
(def editing-when-mounted
  {:did-mount (fn [state]
                (reset! global-editing-flag true)
                state)
   :will-unmount (fn [state]
                   (reset! global-editing-flag false)
                   state)})

(declare edit-box)

(rum/defc token-component < rum/static
  [leaf-class eid child]
  [:span {:key eid
          :class ["tk" leaf-class]
          :on-click (fn [ev]
                      (.stopPropagation ev)
                      (pub! [::select-form eid]))}
   child
   #_(-> text 
         ;; replace with non-breaking hyphen, lmao
         (gstring/replaceAll "-" "‑"))])



(declare fcc)

(defn breadcrumbs-portal-id [eid] (str eid "bp"))
(defn modeline-portal-id [eid] (str eid "mp"))

(rum/defc top-level-form-component < dbrx/ereactive
  [e]
  [:div.form-card
   #_[:span.form-title.code-font (str "#" (:db/id e)
                                      " T+" (- (js/Date.now) load-time) "ms")]
   [:div {:id (breadcrumbs-portal-id (:db/id e))}]
   [:div.top-level-form.code-font (fcc e 0)]
   [:div {:id (modeline-portal-id (:db/id e))}]
   #_(cc/svg-viewbox e)])

(defn do-indent
  [child linebreak? indent-level]
  (if-not linebreak?
    child
    (rum/fragment
     [:span.indent-chars "\n"
      [:span.indenter {:style {:margin-left (str indent-level "ch")}}]]
     child)))

(defn do-highlight
  [child h?]
  (if-not h?
    child
    [:span.selected {:ref "selected"} child]))

(defn computed-indent
  [e indent-prop]
  (-> (:form/indent e)
      (or 0)
      (+ indent-prop)))

(defn coll-fragment
  [e indent-prop]
  (rum/fragment
   (case (:coll/type e) :list "(" :vec "[" :map "{")
   (for [x (e/seq->vec e)]
     (rum/with-key (fcc x (computed-indent e indent-prop)) (:db/id x)))
   (case (:coll/type e) :list ")" :vec "]" :map "}")))

(defmulti display-coll (fn [c indent]
                         (:coll/type c)))

(defmethod display-coll :default [_ _] nil)

(defn delimited-coll
  [e indent open close]
  (rum/fragment
   open
   (for [x (e/seq->vec e)]
     (rum/with-key (fcc x (computed-indent e indent))
       (:db/id x)))
   close))

(defmethod display-coll :list [c i] (delimited-coll c i "(" ")"))
(defmethod display-coll :vec  [c i] (delimited-coll c i "[" "]"))
(defmethod display-coll :map  [c i] (delimited-coll c i "{" "}"))
(defmethod display-coll :set  [c i] (delimited-coll c i "#{" "}"))

(defmethod display-coll :chain [chain i]
  [:div.chain
   {:key (:db/id chain)}
   (for [f (e/seq->vec chain)]
     (-> (top-level-form-component f)
         (rum/with-key (:db/id f))))])

(defmethod display-coll :bar [bar i]
  [:div.bar
    (for [chain-head (e/seq->vec bar)]
      (-> (fcc chain-head i)
          (rum/with-key (:db/id chain-head))))])

#_(defmethod display-coll :tabular [c i]
  (let [cols (:tabular/columns c)]
    [:div.tabluar {:style {:display "grid"
                           :grid-template-columns (str "repeat(" cols ", 1fr)")}}
    (for [e (e/seq->vec c)]
      (-> (fcc e i)
          (rum/with-key (:db/id e))))]))

(rum/defc fcc < dbrx/ereactive (scroll-ref-into-view-after-render "selected")
  [e indent-prop]
  (if (:form/editing e)
    (edit-box (:db/id e)
              (or (:form/edit-initial e)
                  (some-> (:symbol/value e) str)
                  (some-> (:keyword/value e) str)
                  (some-> (:string/value e) pr-str)
                  (some-> (:number/value e) str)))
    (-> (or (when-let [s (:symbol/value e)]
              (token-component
               (case (:symbol/value e)
                 ("defn" "let" "when" "and") "m"
                 ("first") "s"
                 "v")
               (:db/id e) (str s)))
            (when-let [k (:keyword/value e)]
              (token-component "k" (:db/id e)
                               (let [kns (namespace k)]
                                 (if-not kns
                                   (str k)
                                   (rum/fragment ":" [:span.kn kns] "/" (name k))))))
            (when-let [s (:string/value e)]
              (token-component "l" (:db/id e) s))
            (when-let [n (:number/value e)]
              (token-component "n" (:db/id e) (str n)))
            (when (:coll/type e)
              (display-coll e (+ 2 indent-prop)))
            (comment "Probably a retracted entity, do nothing"))
        (do-highlight (:form/highlight e))
        (do-indent (:form/linebreak e)
                   (computed-indent e indent-prop)))))

(defn get-selected-form
  [db]
  (d/entity db [:form/highlight true]))

(defn move-selection-tx
  [src-eid dst-eid]
  [(when src-eid [:db/retract src-eid :form/highlight true])
   [:db/add dst-eid :form/highlight true]])

(defn select-form-tx
  [db eid]
  (move-selection-tx (:db/id (get-selected-form db))
                     eid))

(register-sub ::select-form (->mutation select-form-tx ))

;; list modifications

(defn insert-after-tx
  [target new-node]
  (when-let [spine (first (:seq/_first target))]
    (when-let [coll (first (:coll/_contains target))]
      [{:db/id (:db/id spine)
        :seq/next {:db/id "cons"
                   :seq/first (:db/id new-node)}}
       (when-let [old-next (:seq/next spine)]
         [:db/add "cons" :seq/next (:db/id old-next)])
       {:db/id (:db/id coll)
        :coll/contains (:db/id new-node)}])))

(defn insert-before-tx
  [target new-node]
  (let [spine (first (:seq/_first target))
        coll (first (:coll/_contains target))]
    (prn 'spine (:db/id spine) 'cloll (:db/id coll))
    (when (and spine coll)
      [{:db/id (:db/id spine)
        :seq/first (:db/id new-node)
        :seq/next {:db/id "insert-before-cons"
                   :seq/first (:db/id target)}}
       [:db/add (:db/id coll) :coll/contains (:db/id new-node)]
       (when-let [next (:seq/next spine)]
         [:db/add "insert-before-cons" :seq/next (:db/id next) ])])))

(defn form-delete-tx
  [e]
  (let [spine (first (:seq/_first e))
        next  (some-> spine :seq/next)
        prev  (some-> spine :seq/_next first)]
    [[:db/retractEntity (:db/id e)]
     (cond
       (and prev next) [:db/add (:db/id prev) :seq/next (:db/id next)]
       prev            [:db/retract (:db/id prev) :seq/next (:db/id spine) ]
       next            [:db/add (:db/id spine) :seq/first (:db/id next)])]))


;; overwrite with something that has not existed before
;; if you have a tempid, you want this one
(defn form-overwrite-tx
  "rplaca"
  [e replacement-eid]
  (let [spine (first (:seq/_first e))
        coll (first (:coll/_contains e))]
    (when (and spine coll)
      [[:db/retract (:db/id coll) :coll/contains (:db/id e)]
       [:db/add (:db/id coll) :coll/contains replacement-eid]
       [:db/add (:db/id spine) :seq/first replacement-eid]])))

;; replace with something from somewhere else
;; if you have a second entity, you want this one
(defn form-replace-tx
  [e r]
  (let [old-parent (first (:coll/_contains r))
        old-spine (first (:seq/_first r))]
    (into (form-overwrite-tx e (:db/id r))
          [(when old-spine
             [:db/retract (:db/id old-spine) :seq/first (:db/id r)])
           (when old-parent
             [:db/retract (:db/id old-parent) :coll/contains (:db/id r)])])))

(defn form-raise-tx
  [e]
  (when-let [parent (some-> (:coll/_contains e) first)]
    (when-not (= :chain (:coll/type parent))
      (into [[:db/add (:db/id parent) :form/edited-tx :db/current-tx]]
            (form-replace-tx parent e)))))

(defn raise-selected-form-tx
  [db]
  (form-raise-tx (get-selected-form db)))

(register-sub ::raise-selected-form (->mutation raise-selected-form-tx))

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
          (concat (insert-after-tx sel new-node)
                  (move-selection-tx (:db/id sel) (:db/id new-node))))))

(register-sub ::duplicate-selected-form (->mutation insert-duplicate-tx))

(defn form-wrap-tx
  [e ct]
  (into [{:db/id "newnode"
          :coll/type ct
          :coll/contains (:db/id e)
          :seq/first (:db/id e)}]
        (form-overwrite-tx e "newnode")))

(register-sub ::wrap-selected-form
              (->mutation
               (fn [db ct]
                 (form-wrap-tx (get-selected-form db) ct))))

(defn insert-editing-tx
  [db before-or-after edit-initial]
  (let [sel (get-selected-form db)
        new-node {:db/id "newnode"
                  :form/editing true
                  :form/edit-initial (or edit-initial "")}
        itx (case before-or-after
              :before (insert-before-tx sel new-node)
              :after (insert-after-tx sel new-node))]
    (into [new-node]
          (concat itx (move-selection-tx (:db/id sel) "newnode")))))

(register-sub
 ::wrap-and-edit-first
 (->mutation
  (fn [db ct]
    (let [sel (get-selected-form db)
          spine (first (:seq/_first sel))
          new-node {:db/id "first"
                    :coll/type ct
                    :seq/first {:db/id "funcname"
                                :coll/_contains "first"
                                :form/editing true}
                    :seq/next {:db/id "second"
                               :coll/_contains "first"
                               :seq/first (:db/id sel)}}]
      (into [new-node
             {:db/id (:db/id sel) :coll/_contains "first"}]
            (concat
             (form-overwrite-tx sel "first")
             (move-selection-tx (:db/id sel) "funcname")))))))


;; editing

(register-sub ::insert-editing (->mutation insert-editing-tx))

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
          (concat (insert-after-tx sel new-node)
                  (move-selection-tx (:db/id sel) "inner")))))

(register-sub ::edit-new-wrapped (->mutation edit-new-wrapped-tx))

(defmulti move (fn [t _] t))

(defmethod move :move/most-nested [_ e]
  (last (tree-seq :coll/type e/seq->vec e)))

(defmethod move :move/most-upward [_ e]
  (->> e
       (iterate (partial move :move/up))
       (take-while some?)
       last))

(defmethod move :move/next-sibling [_ e]
  (some-> (:seq/_first e) first :seq/next :seq/first))

(defmethod move :move/prev-sibling [_ e]
  (some-> (:seq/_first e) first :seq/_next first :seq/first))

(defn flow*
  [e]
  (when e
    (or (move :move/next-sibling e)
        (recur (move :move/up e)))))

(defmethod move :move/flow [_ e]
  (or (:seq/first e)
      (flow* e)))

(defmethod move :move/back-flow [_ e]
  (or (->> e
           (move :move/prev-sibling)
           (move :move/most-nested))
      (move :move/up e)))

(defmethod move :move/up [_ e]
  #_(some-> (:coll/_contains e) first)
  (when-let [cs (:coll/_contains e)]
    (let [up (first cs)]
      (when-not (= :chain (:coll/type up))
          up))))

(defmethod move :move/forward-up [_ e]
  (or (move :move/next-sibling e)
      (move :move/up e)))

(defmethod move :move/backward-up [_ e]
  (or (move :move/prev-sibling e)
      (move :move/up e)))

(defn movement-tx
  [db movement-type]
  (let [src (get-selected-form db)
        dst (move movement-type src)] 
    (if-not dst
      (println "Cannot" movement-type "from" src)
      (move-selection-tx (:db/id src) (:db/id dst)))))

(defn repeat-movement-tx
  [db movement-type reps]
  (let [src (get-selected-form db)
        dst (->> src
                 (iterate (partial move movement-type))
                 (take reps)
                 (take-while some?)
                 (last))] 
    (if-not dst
      (println "Cannot" movement-type "from" src)
      (move-selection-tx (:db/id src) (:db/id dst)))))

(register-sub ::move (->mutation movement-tx))

(register-sub ::repeat-move (->mutation repeat-movement-tx))

(defn move-and-delete-tx
  [db movement-type]
  (let [src (get-selected-form db)]
    (when-let [dst (move movement-type src)]
      (concat (form-delete-tx src)
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
        parent-indent (:form/indent (move :move/up sel))]
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


(register-sub ::execute-selected-as-mutation
              (->mutation (fn [db]
                            (let [m (e/->form (get-selected-form db))]
                              (if (vector? (first m))
                                (run! pub! m)
                                (pub! m)))
                            nil)))

(defn parents-array
  [e]
  (->> e
       (iterate (partial move :move/up))
       (take-while some?)
       (clj->js)))

(defn reverse-parents-array
  [e]
  (.reverse (parents-array e)))

(defn import-formdata-tx
  [db data]
  (let [sel (get-selected-form db)
        top-level (first (reverse-parents-array sel))
        chain (some-> top-level :coll/_contains first)
        new-node (-> (e/->tx data)
                     (update :db/id #(or % "import-formdata-tx")))]

    (into [new-node]
          (concat (insert-before-tx top-level new-node)
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

(register-sub ::reify-last-mutation
              (->mutation (fn [db]
                            (let [r @h/last-tx-report]
                              (import-formdata-tx
                               db
                               {:tx-data (mapv vec (:tx-data r) )
                                :tx-meta (:tx-meta r)
                                :tempids (:tempids r)})))))

(register-sub ::eval
              (->mutation (fn [db]
                            (let [pa (some-> (get-selected-form db) (parents-array))
                                  [mut & args] (first (keep :form/eval-action pa))]
                              (prn 'mut mut 'args args)
                              
                              ))))
(register-sub ::select-chain
              (->mutation (fn [db]
                            (let [top-level (first (reverse-parents-array (get-selected-form db)))
                                  chain (some-> top-level :coll/_contains first)]
                              (select-form-tx db (:db/id chain))))))




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
                         (concat (insert-before-tx top-level new-node)
                                 (move-selection-tx (:db/id sel) (:db/id new-node))))))))

#_(register-sub ::toggle-passthru
              (->mutation
               (fn [db]
                 (let [sel      (get-selected-form db)]
                   [[:db/add (:db/id sel) :form/render-passthru (not (:form/render-passthru sel )) ]]))))


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




;; edit box


(defn parse-token-tx
  [text-value eid]
  (try
    (-> text-value
        (edn/read-string)
        (e/->tx)
        (assoc :db/id eid))
    (catch js/Error e
      #_(println "No edn" text-value)
      #_(js/console.log e)
      nil)))

(defn accept-edit-tx
  [form-eid value]
  [{:db/id :db/current-tx
    :edit/of form-eid}
   [:db/retract form-eid :symbol/value]
   [:db/retract form-eid :keyword/value]
   [:db/retract form-eid :string/value]
   [:db/retract form-eid :number/value]
   (parse-token-tx value form-eid)
   [:db/add form-eid :form/edited-tx :db/current-tx]

   [:db/retract form-eid :form/editing true]
   [:db/retract form-eid :form/edit-initial]])

(defn reject-edit-tx
  [db form-eid]
  (concat (movement-tx db :move/backward-up)
          (form-delete-tx (d/entity db form-eid))))

(defn wrap-edit-tx
  [db form-eid ct value]
  (into [[:db/retract form-eid :form/editing true]
         {:db/id "newnode"
          :coll/type ct
          :seq/first {:db/id "inner"
                      :coll/_contains "newnode"
                      :form/edit-initial (or value "")
                      :form/editing true}}]
        (concat
         (form-overwrite-tx (d/entity db form-eid) "newnode")
         (move-selection-tx form-eid "inner"))))

(defn finish-edit-tx
  [db eid text]
  (if (empty? text)
    (reject-edit-tx db eid)
    (accept-edit-tx eid text)))

(register-sub :edit/finish (->mutation (fn [db text] (finish-edit-tx db (d/entid db [:form/editing true]) text))))
(register-sub :edit/reject (->mutation (fn [db] (reject-edit-tx db (d/entid db [:form/editing true])))))
(register-sub :edit/wrap   (->mutation (fn [db ct value] (wrap-edit-tx db (d/entid db [:form/editing true]) ct value))))
(register-sub :edit/finish-and-move-up
              (->mutation (fn [db text]
                            (let [edit (d/entid db [:form/editing true])] 
                              (if (empty? text)
                                (reject-edit-tx db edit)
                                (into (accept-edit-tx edit text)
                                      (movement-tx db :move/up)))))))
(register-sub :edit/finish-and-edit-next-node
              (->mutation (fn [db text]
                            (into (accept-edit-tx (d/entid db [:form/editing true]) text)
                                  (insert-editing-tx db :after "")))))

(defn editbox-keydown-mutation
  [db text key]
  (case key
    "Escape"      [:edit/reject]
    "Backspace"   (when (empty? text)
                    [:edit/reject])
    "Enter"       [:edit/finish text]
    (")" "]" "}") [:edit/finish-and-move-up text]
    ("[" "(" "{") [:edit/wrap (case key "(" :list "[" :vec "{" :map) text]
    " "           (cond
                    (empty? text)
                    [:edit/reject]
                    
                    (= "\"" (first text))
                    (println "Quotedstring")
                    
                    :else
                    [:edit/finish-and-edit-next-node text])
    nil))

(def editbox-ednparse-state (atom nil))
(rum/defcs edit-box
  < (rum/local [] ::text) (focus-ref-on-mount "the-input") editing-when-mounted
  [{::keys [text]} form-eid init]
  (let [value (if (= [] @text)
                init
                @text)]
    [:input.edit-box.code-font
     {:type        :text
      :ref         "the-input"
      :value       (or value "")
      :style       {:width (str (max 4 (inc (count value))) "ch")}
      :on-change   #(let [new-text (string/triml (.-value (.-target %)))
                          token (parse-token-tx new-text form-eid)]
                      (reset! text new-text)
                      (reset! editbox-ednparse-state
                              {:form-eid form-eid
                               :text new-text
                               :valid (some? token)
                               :type (some-> token first val)}))
      :on-key-down (fn [ev]
                     (when-let [mut (editbox-keydown-mutation @conn value (.-key ev))]
                       (.preventDefault ev)
                       (.stopPropagation ev)
                       (pub! mut)))
      ;; :on-blur #(pub! [:edit/finish @text])
      }]))

(defn link-to-form-by-id
  [eid children]
  [:a {:key eid
       :href "#"
       :on-click #(do (.preventDefault %)
                      (pub! [::select-form eid]))}
   children])


#_(defn el-bfs
  [top limit]
  (loop [[e & front] [top]
         out         []]
    (cond
      (nil? e)                    out
      (= limit (count out))       out
      (not (:coll/type e)) (recur front out)
      :else
      (recur
       (into front (e/seq->vec e))
       (if-not (= :list (:coll/type e))
         out
         (conj out e))))))

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
   {" "     [::insert-editing :after]
    "S-\""  [::insert-editing :after "\""]
    "S-:"   [::insert-editing :after ":"]
    "S- "   [::insert-editing :before]
    "'"     [::insert-editing :before "'"]
    "S-("   [::edit-new-wrapped :list]
    "M-S-(" [::wrap-and-edit-first :list]
    "9"     [::wrap-selected-form :list]
    
    "["      [::edit-new-wrapped :vec ]
    "S-{"    [::edit-new-wrapped :map ]
    "r"      [::raise-selected-form]
    "S-X"    [::extract-to-new-top-level]
    "Escape" [::select-chain]
    "m"      [::edit-selected]
    "]"      [::move :move/up]

    "w"         [::exchange-with-previous]
    "s"         [::exchange-with-next]
    "0"         [::move :move/up]
    "v"         [::scroll-into-view]
    "h"         [::move :move/up]
    "j"         [::move :move/next-sibling]
    "k"         [::move :move/prev-sibling]
    "l"         [::move :move/most-nested]
    "f"         [::move :move/flow]
    "a"         [::move :move/back-flow]
    "n"         [::move :move/most-nested]
    "d"         [::delete-with-movement :move/forward-up]
    "e" [::eval]
    "Backspace" [::delete-with-movement :move/backward-up]
    "c"         [::duplicate-selected-form]
    ;; "i"         [::indent-form 1]
    ;; "S-I"       [::indent-form -1]
    "Tab"       [::indent-form 1]
    "S-Tab"     [::indent-form -1]
    "i"         [::reset-indent]
    "Enter"     [::linebreak-form]
    "M-p"       ["placeholder"]
    "M-n"       ["placeholder"]
    ;; "Tab"       ["Placeholder for the real tab handler which is special because it is stateful"]
    ;; "S-Tab"     ["Same as Tab"]
    "S-M"       [::recursively-set-indent true]
    "S-O"       [::recursively-set-indent false]
    "M-x"       [::execute-selected-as-mutation]
    "S-A"       {"a" [::linebreak-form]}
    "S-R"       {"f" [::reify-extract-selected]
                 "m" [::reify-last-mutation]}
    }
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
      #_[:input {:value text}]
      text
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
    #_(pr-str (rum/react editbox-ednparse-state))
    (if-not sel
      "(no selection)"
      (str "#" (:db/id sel) ))
    (command-compose-feedback)]])

(rum/defc modeline-portal  < rum/reactive (dbrx/areactive :form/highlight :form/editing)
  [db]
  (let [sel (get-selected-form db)
        ps (when (:form/editing sel)
             (rum/react editbox-ednparse-state))
        rpa (reverse-parents-array sel)
        _ (prn 'MP_RPA rpa)
        nn (.getElementById js/document (modeline-portal-id (:db/id (first rpa))))]
    (prn nn)
    (when nn
      (-> (modeline-inner
           sel
           (rum/react h/last-tx-report)
           (when (:form/editing sel)
             (rum/react editbox-ednparse-state)))
          (rum/portal nn)))))

(rum/defc modeline-next-always < rum/reactive (dbrx/areactive :form/highlight :form/editing)
  [db]
  (let [sel (d/entity db [:form/highlight true])
        {:keys [text valid]} (when (:form/editing sel)
                               (rum/react editbox-ednparse-state))]
    [:span {:class (str "modeline modeline-size code-font"
                        (when text " editing")
                        (when (and (not (empty? text)) (not valid)) " invalid"))}
     [:span.modeline-echo.modeline-size
      (if text
        #_[:input {:value text}]
        text
        (let [r (rum/react h/last-tx-report)]
          (str (pr-str (:mutation (:tx-meta r)))
               (when-let [kbd (:kbd (:tx-meta r))]
                 (str " " kbd))
               " "
               (:db/current-tx (:tempids r))

               " ("
               (count (:tx-data r))
               ")")))]
     [:span.modeline-content
      #_(pr-str (rum/react editbox-ednparse-state))
      (if-not sel
        "(no selection)"
        (str "#" (:db/id sel) ))
      (command-compose-feedback)]]))








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
     #_(h/history-view conn bus)
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

;; You have a view of the top-level form on the side and edit within it
;; how do we force update the view without touching other views?
;; (it's ereactive to the top-level and there is a toplevel edited-tx?)
;; problem: toplevel edited-tx forces the entire form to re-render on edit accept
;; (it otherwise does not)
;; point the reference the other way and create :edit/tx and :edit/within
;; problem: these entries just accumulate forever
;; query: is this a problem? aren't they also the history? yes1
;; just hack it to make raise work with :edit/force-updates-toplevel?
;; edit/modifies-enclosing?
;;
;; form display mixin?
;; representation as code, representation as db entity, etc

;; multiplayer...need to have highlight be a value (the user) and not true/false
;; but otherwise it's just multiple clients for one db? kinda sync?
;; or multiple independent dbs with async mutation pushing
;; render another root-component which is associated with a remote-shadow db
;; and a bus which sources events from network
;; don't have to show their entire root-component (but could),
;; show only their current toplevel?
;; when joining, each client sets their eid/tx counters to +1mil of highest in sesion?
;; then txdatas are kinda async?

(defn global-keydown*
  [ev]
  (when-not @global-editing-flag
    (let [kbd (event->kbd ev)
          {:keys [bindings compose]} @key-compose-state
          mut (get bindings kbd)
          next-kbd (conj (or compose []) kbd)]
      #_(prn kbd)
      (pub! [::global-keydown kbd])
      (when (or (some? mut)
                (and compose (nil? mut)))
        (.preventDefault ev)
        (.stopPropagation ev))
      (if-not mut
        (reset! key-compose-state initial-compose-state)
        (cond
          (vector? mut) (do (reset! key-compose-state initial-compose-state)
                            (pub! (with-meta mut {:kbd (string/join " " next-kbd)})))
          (map? mut) (reset! key-compose-state {:bindings mut :compose next-kbd})
          :else (do (println "Bad key entry" mut)
                    (reset! key-compose-state initial-compose-state)))))))

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

(defn  ^:dev/after-load init []
  (js/document.removeEventListener "keydown" global-keydown true)
  (js/document.removeEventListener "keyup" global-keyup true)
  
  
  (js/document.addEventListener "keydown" global-keydown true)
  (js/document.addEventListener "keyup" global-keyup true)
  (h/clear!)
  (dbrx/reset-for-reload!)
  (let [se (d/entity @conn ::state)] 
    (rum/mount
     (root-component (d/entity @conn ::state))
     (.getElementById js/document "root"))))

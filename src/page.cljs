(ns page
  (:require
   [clojure.edn :as edn]
   [embed :as e]
   [debug :as debug]
   [tx-history :as h]
   [goog.string :as gstring]
   [datascript.core :as d]
   [clojure.string :as string]
   [db-reactive :as dbrx]
   [rum.core :as rum]
   [cljs.core.async :as async])
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop]]))

(def schema
  (merge
   e/form-schema
   {:state/display-form {:db/valueType :db.type/ref
                         :db/cardinality :db.cardinality/many}
    ;; :state/selected-form {:db/valueType :db.type/ref}
    :form/editing {:db/unique :db.unique/identity} 
    :form/edit-initial {}
    :form/highlight {:db/unique :db.unique/identity}
    :form/indent {}
    :form/linebreak {}
    :form/edited-tx {:db/valueType :db.type/ref}}))

(def test-form-data-original
  '[
    (defn other-thing ;; ^{:tempid "init-selection"}
      [something a b c [:page/repeat-move :move/flow 9] d] blah)
    #_(defn form-overwrite-tx
      [e replacement-eid]
      (let [spine (first (:seq/_first e))
            coll (first (:coll/_contains e))]
        (when (and spine coll)
          [[:db/retract (:db/id coll) :coll/contains (:db/id e)]
           [:db/add (:db/id coll) :coll/contains replacement-eid]
           [:db/add (:db/id spine) :seq/first replacement-eid]])))
    


    
    #_(defn ->tx
      [e]
      (letfn [(coll-tx [coll-type xs]
                (let [id (new-tempid)]
                  (cond-> {:db/id id :coll/type coll-type}
                    (seq xs) (merge (seq-tx (for [x xs]
                                              (assoc (->tx x) :coll/_contains id)))))))]
        (cond 
          (symbol? e)    {:symbol/value (str e)}
          (keyword? e)   {:keyword/value e}
          (string? e)    {:string/value e}
          (number? e)    {:number/value e}
          (list? e)      (coll-tx :list e)
          (vector? e)    (coll-tx :vec e)
          (map? e)       (coll-tx :map (flatten-map e)))))])

(def test-form-data (vec (apply concat (repeat 1 test-form-data-original))))


#_(def test-form-data '[f :fo  cb ])

(def conn
  (doto (d/create-conn schema)
    (d/transact! (apply concat
                        (for [f test-form-data]
                          (let [txe (e/->tx f)]
                            [{:db/ident ::state
                              :state/display-form (:db/id txe)}
                             txe
                             ]))))
    #_(d/transact! [[:db/add 53 :form/highlight true]])))




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
  [leaf-class eid ^String text]
  [:span {:key eid
          ;; :data-eid eid
          :class ["tk" leaf-class]
          :on-click (fn [ev]
                      (.stopPropagation ev)
                      (pub! [::select-form eid])
                      #_(pub! [::select-form  (-> (.-target ev ) (.-dataset) (aget "eid") (js/parseInt))]))}
   (-> text 
       ;; replace with non-breaking hyphen, lmao
       (gstring/replaceAll "-" "‑"))])

(declare fcc)

(defn make-indenter
  [indent-level]
  [:span.indent-chars "\n"
   [:span.indenter {:style {:margin-left (str indent-level "ch")}}]])

(defn do-indent
  [child linebreak? indent-level]
  (if-not linebreak?
    child
    (rum/fragment
     (make-indenter indent-level)
     child)))

(defn do-highlight
  [child h?]
  (if-not h?
    child
    [:span.selected {:ref "selected"} child]))

(defn coll-fragment
  [e indent-prop]
  (let [my-indent (-> (:form/indent e)
                      (or 0)
                      (+ indent-prop))]
    (rum/fragment
     #_(when (:form/linebreak e)
       (make-indenter my-indent))
     #_(when (:form/linebreak e)
         ;; indent in char units
         #_[:span.indent-chars (str "\n" (.repeat " " my-indent))]
         ;; indent in pixel units
         [:span.indent-chars (str "\n"
                                  #_"  ")
          [:span.indenter {:style {:margin-left (str my-indent "ch")}}]])
     (case (:coll/type e) :list "(" :vec "[" :map "{")
     (for [x (e/seq->vec e)]
       (rum/with-key (fcc x my-indent) (:db/id x)))
     (case (:coll/type e) :list ")" :vec "]" :map "}"))))

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
              (token-component "s" (:db/id e) (str s)))
            (when-let [k (:keyword/value e)]
              (token-component "k" (:db/id e) (str k)))
            (when-let [s (:string/value e)]
              (token-component "l" (:db/id e) s))
            (when-let [n (:number/value e)]
              (token-component "n" (:db/id e) (str n)))
            (when (:coll/type e)
              (coll-fragment e (+ 2 indent-prop)))
            (comment "Probably a retracted entity, do nothing"))
        (do-highlight (:form/highlight e))
        (do-indent (:form/linebreak e)
                   (-> (:form/indent e)
                       (or 0)
                       (+ indent-prop))))))

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
  (when-let [spine (first (:seq/_first target))]
    (when-let [coll (first (:coll/_contains target))]
      [{:db/id (:db/id spine)
        :seq/first (:db/id new-node)
        :seq/next {:db/id "cons"
                   :seq/first (:db/id target)}}
       {:db/id (:db/id coll)
        :coll/contains (:db/id new-node)}])))

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
  [e replacement-eid]
  (let [spine (first (:seq/_first e))
        coll (first (:coll/_contains e))]
    (println "No sense" e replacement-eid)
    (println "Fotx" spine coll)
    (when (and spine coll)
      [[:db/retract (:db/id coll) :coll/contains (:db/id e)]
       [:db/add (:db/id coll) :coll/contains replacement-eid]
       [:db/add (:db/id spine) :seq/first replacement-eid]])))

;; replace with something with somewhere else
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
    (println "Parent raise" parent)
    (form-replace-tx parent e)))

(defn raise-selected-form-tx
  [db]
  (let [sel (get-selected-form db)]
    (form-raise-tx (get-selected-form db))))

(register-sub ::raise-selected-form (->mutation raise-selected-form-tx))

(register-sub ::exchange-with-previous
              (->mutation
               (fn [db]
                 (let [sel    (get-selected-form db)
                       spine  (first (:seq/_first sel))
                       prev   (some-> spine :seq/_next first)
                       next   (some-> spine :seq/next)
                       parent (first (:coll/_contains sel))]
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
  (let [newnode {:db/id "newnode"
                 :coll/type ct
                 :coll/contains (:db/id e)
                 :seq/first (:db/id e)}]
   (into [newnode]
         (form-overwrite-tx e "newnode"))))

(register-sub
 ::wrap-selected-form (->mutation
                       (fn [db ct]
                         (form-wrap-tx
                          (get-selected-form db)
                          ct))))

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
          new-node {:db/id "newnode"
                    :coll/type ct
                    :coll/contains "funcname" 
                    :seq/first {:db/id "funcname"
                                :form/editing true}}]
      (println "WAEF" sel)
      (println "FOtx" (form-overwrite-tx (:db/id sel) "newnode"))
      (into [new-node]
            (concat (form-overwrite-tx (:db/id sel) "newnode")
                    (move-selection-tx (:db/id sel) "funcname")))
      #_(into [new-node
               [:db/retract (:db/id sel) :seq/next (:seq/next sel)]
               (when-let [old-next (:seq/next sel)]
                 [:db/add "newnode" :seq/next old-next])
               (when-let [parent (first (:coll/_contains sel))]
                 [:db/retract (:db/id parent) :coll/contains (:db/id sel)])]
              (move-selection-tx (:db/id sel) "funcname")))
    )))
;; paredit


#_(defn slurp-right-tx
  [db c]
  ;; [c] a
  ;; [c a]
  (let [e (if (:coll/type c)
            c
            (some-> (:coll/_contains c) first))
        a (:seq/next )
        ]
    
    )
  )


;; editing

#_(register-sub ::edit-new-node-after-selected (->mutation insert-editing-tx))
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
        (recur (some-> (:coll/_contains e) first)))))

(defmethod move :move/flow [_ e]
  (or (:seq/first e)
      (flow* e)))

(defn back-flow*
  [e]
  (println "Back-flow* " (:db/id e))
  (when e
    (or (move :move/prev-sibling e)
        (recur (some-> (:coll/_contains e) first)))))

(defn most-nested-if-different
  [e]
  (let [[_ & more] (tree-seq :coll/type e/seq->vec e)]
    (when more
      (last more))))

(defmethod move :move/back-flow [_ e]
  (or (->> e
           (move :move/prev-sibling)
           (move :move/most-nested))
      (move :move/up e))
  
  #_(let [mn (move :move/most-nested e)]
      (println "E mn" (:db/id e) (:db/id mn))
      (or (when-not (= (:db/id e) (:db/id mn))
            mn)
          (move :move/prev-sibling e)
          (recur b (some-> (:coll/_contains e) first)))))

(defmethod move :move/up [_ e]
  (some-> (:coll/_contains e) first))

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
        #_dst #_(->> src
                     (iterate (partial move movement-type))
                     (take-while some?)
                     (drop reps)
                     (first))
        aa (->> src
                (iterate (partial move movement-type))
                (take reps)
                (take-while some?)
                (last))
        dst aa
        ] 
    (println "aa" aa)
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
  (let [sel (get-selected-form db)]
    (for [e (tree-seq :coll/type e/seq->vec sel)]
      (when (:coll/type e)
        [:db/add (:db/id e) :form/linebreak indent]))))

(register-sub ::recursively-set-indent (->mutation recursively-set-indent-tx))


(register-sub ::execute-selected-as-mutation
              (->mutation (fn [db]
                            (let [m (e/->form (get-selected-form db))]
                              (if (vector? (first m))
                                (run! pub! m)
                                (pub! m)))
                            nil)))

(register-sub ::import-data-toplevel
              (->mutation (fn [db data]
                            (let [txe (update (e/->tx data) :db/id #(or % "import-data"))]
                              (into
                               [txe
                                {:db/ident ::state
                                 :state/display-form (:db/id txe)}]
                               (select-form-tx db (:db/id txe)))))))


(defn reverse-parents-array
  [e]
  (->> e
       (iterate (partial move :move/up))
       #_(next)
       (take-while some?)
       (clj->js)
       (.reverse)))


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
                 (let [sel      (get-selected-form db)
                       new-node (-> (form-duplicate-tx sel)
                                    (update :db/id #(or % "dup-leaf")))]
                   (into [new-node
                          {:db/ident           ::state
                           :state/display-form (:db/id new-node)}]
                         (move-selection-tx (:db/id sel) (:db/id new-node)))))))

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
  [{:db/id :db/current-tx}
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


(defn el-bfs
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

(def breadcrumbs-max-numeric-label 8)
(rum/defc breadcrumbs
  [sel top-level-eid]
  (let [rpa (reverse-parents-array sel)]
    [:ul.parent-path
     ;; hack to only show for the active toplevel
     (cond
       (= (:db/id sel) top-level-eid)
       (pr-str (for [ebfs (el-bfs sel 9)]
                 (some-> ebfs :seq/first :symbol/value)))
       (not (= top-level-eid (:db/id (first rpa))))
       nil
       
       #_[:li (str js/performance.memory.usedJSHeapSize #_(js/Date.now) )]
       :else
       (for [i (range (dec (count rpa)))]
         (let [parent (nth rpa i)]
           [:li {:key i}
            (link-to-form-by-id (:db/id parent)
                                [:span.code-font
                                 (when (< i breadcrumbs-max-numeric-label)
                                   [:span.parent-index (str (inc i))])
                                 (if (= :list (:coll/type parent))
                                   (or (some-> parent :seq/first :symbol/value) "()")
                                   (case (:coll/type parent) :vec "[]" :map "{}"))])])))]))


(rum/defc focus-info-inner < dbrx/ereactive
  [sel]
  (let [db (d/entity-db sel)]
    [:div 
     [:div (str "#" (:db/id sel)) ]
     #_(debug/datoms-table-ave
      (apply concat
             (for [[k as] schema
                   :when (= :db.type/ref (:db/valueType as) )]
               (d/datoms db :avet k (:db/id sel))))
      "A"
      (str "V = " (:db/id sel))
      "E")
     #_(debug/datoms-table-eav
      (d/datoms db :eavt (:db/id sel))
      (str "E = " (:db/id sel))
      "A"
      "V"
      )
     (debug/datoms-table-eav
      (apply concat
             (d/datoms db :eavt (:db/id sel))
             (for [[k as] schema
                   :when (= :db.type/ref (:db/valueType as) )]
               (d/datoms db :avet k (:db/id sel)))))
     
     (comment
       "Symbols"
       [:pre
        (with-out-str
          (run! println
                (dedupe
                 (for [[e a v t] (d/datoms db :avet :symbol/value)]
                   v))))]

       "Keywords"
       [:pre
        (with-out-str
          (run! println
                (dedupe
                 (for [[e a v t] (d/datoms db :avet :keyword/value)]
                   v))))]
     
       #_ [:pre 
           (with-out-str (cljs.pprint/pprint (e/->form sel)))])])
  )

(rum/defc focus-info < (dbrx/areactive :form/highlight)
  [db]
  (when-let [sel (get-selected-form db)]
    (focus-info-inner sel)))

(def special-key-map
  (merge
   {" "      [::insert-editing :after]
    "S-\""   [::insert-editing :after "\""]
    "S-:"    [::insert-editing ":"]
    "Escape" [::move :move/most-upward]
    "'"      [::insert-editing "'"]
    "S-("    [::edit-new-wrapped :list]
    "M-S-("  [::wrap-and-edit-first :list]
    "9"      [::wrap-selected-form :list]
    
    "["   [::edit-new-wrapped :vec ]
    "S-{" [::edit-new-wrapped :map ]
    "r"   [::raise-selected-form]
    "S-X" [::extract-to-new-top-level]
    ;; "a"      [::edit-selected]
    "]"   [::move :move/up]

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
    "S-A"       {"a" [::linebreak-form]}}
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

#_(rum/defc breadcrumbs-outer < (dbrx/areactive :form/highlight)
  [db toplevel]
  )

(rum/defc what < (dbrx/areactive :form/highlight)
  [db toplevel]
  (breadcrumbs (get-selected-form db) toplevel))

(declare command-compose-feedback)
(rum/defc modeline-next < rum/reactive (dbrx/areactive :form/highlight :form/editing)
  [db top-eid]
  (let [sel (d/entity db [:form/highlight true])
        rpa (reverse-parents-array sel)
        top-parent (:db/id (first rpa))
        {:keys [text valid]} (when (:form/editing sel)
                               (rum/react editbox-ednparse-state))]
    (when (or (= top-eid (:db/id sel))
              (= top-eid (:db/id (first rpa))))
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
        (str "#" (:db/id sel) )
        (command-compose-feedback)]])))

(rum/defc top-level-form-component < dbrx/ereactive
  [e]
  [:div.form-card
   [:span.form-title.code-font (str "#" (:db/id e))]
   (what (d/entity-db e) (:db/id e))
   [:div.top-level-form.code-font
    (fcc e 0)]
   (modeline-next (d/entity-db e) (:db/id e))
   #_(toplevel-modeline (d/entity-db e) (:db/id e))])



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
                " (Selected)"))
         )])]))

(rum/defc keyboard-key-component
  [k->l keycap]
  [:div {:class ["key"
                 (when (= 1 (count keycap)) " single-width")]} 
   [:div.keycap keycap]
   (when-let [legend (k->l keycap)]
     [:div.key-legend (str legend)])])


(rum/defc root-component < dbrx/ereactive
  [state]
  (let [db (d/entity-db state)]
    [:div.root-cols
     [:div.sidebar-left
      (focus-info db)
      (edit-history db)
      (h/history-view conn bus)]
     [:div.main-content
      (for [df (:state/display-form state)]
        (rum/with-key (top-level-form-component df) (:db/id df)))
      #_(all-datoms-table)]
     (let [key->legend {"f" "flow→"
                        "r" "raise"
                        "w" "swap↑"
                        "s" "swap↓"
                        "0" "up"
                        "9" "(wrap)"
                        "[" "new []"
                        "q" "(wrap‸)"
                        "p" ")→"
                        "]" "up"
                        "a" "←flow"
                        "d" "delete"
                        "v" "view"
                        "h" "parent"
                        "j" "next"
                        "k" "prev"
                        "l" "tail"
                        "i" "indent"
                        "c" "clone"
                        "Enter" "linebreak"
                        "Backspace" "←delete"}
           key (partial keyboard-key-component key->legend)]
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
          (key "Ctrl")]]])]))

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
  (when-not @global-editing-flag
    (let [kbd (event->kbd ev)
          {:keys [bindings compose]} @key-compose-state
          mut (get bindings kbd)
          next-kbd (conj (or compose []) kbd)]
      (prn kbd)
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
  (prn "Keyup" (event->kbd ev)))

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


;; insert over empty coll should insert into that coll
;; symbol search
;; query/transaction editor
;; top-level implicit do
;; Inline documentation / eval result
;; Marginalia
;; Bug with exchanges!
;; keybinds in database
;; insert string, have the cursor start in between the quotes
;; import formatted code with rewrite clj
;; Player piano mode
;; Random mutation testing
;; Teleports!
;; reify editor in db to have multiple selections going?

;; Keybinds edited as defs?
;;  -- Vi like language made of (def f [::move :move/flow])

;; Editor double linked list
;; Reify editor state as editable data?
;; Separate text box for command mode
;; Show expanded mut name
;; Separate comp hier for alt presentation
;; For replay mode, generate all dbs upfront, Same as time scrub then
;; Leading comma means edn command
;; Eval creates a Mapping cmd->out , should be able to tag that entity after
;; Command to add/extract to new vertical chain
;; Recall command to select object of previous command by command name
;; Command to find and assign available keybinding

;; File format:  EDN, database value and list of txdatas
;; Idea being to just append to a giant buffer and periodically compact

;; Attach movement source as metadata to movement mutation
;; Pass the root entity id all the way down into the editbox and transact edit-tx on it

;; Undo - go back in time, big loud sign, allow you to teleport forms into the future
;; Replay edits at relative structural positions?

;; Amortize serialization overhead by doing it as the selection moves
;; Keep track of the serialization before selected form
;; Then serialize the tail and close the delimiters

;; ::create-definition mutation, just extract the symbol name into a def (starts editing)
;; This happens if you repeat goto-def?

;; save :form/last-edited-child??
;; highlight that in a diff color? easy jump to it?

;; :form/display :none, lol

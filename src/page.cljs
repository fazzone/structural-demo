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
    :form/edited-tx {:db/valueType :db.type/ref}

    }))

(def test-form-data
  '[
   (defn other-thing ;; ^{:tempid "init-selection"}
     [something a b c d] blah)
   (defn ->tx
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
#_(def test-form-data '[f :fo  cb ])

(def conn
  (doto (d/create-conn schema)
    (d/transact! (apply concat
                        (for [f test-form-data]
                          (let [txe (e/->tx f)]
                            [txe
                             #_{:db/id "init-selection"}
                             {:db/ident ::state
                              :state/display-form (:db/id txe)}]))))
    (d/transact! [[:db/add 11 :form/highlight true]])))




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

#_(def ^:dynamic *et-index* (js/Array. 1024))

(defn ->mutation
  [tx-fn]
  (fn [[_ & args :as mutation]]
    (when-let [tx-data (apply tx-fn @conn args)]
      ;; transact metadata on db/current-tx?  run out of memory?
      (d/transact! conn tx-data (merge {:mutation mutation
                                        :input-tx-data tx-data}
                                       (meta mutation))))))

(d/listen! conn
           dbrx/tx-listen-fn)

#_(d/listen! conn
           (-> (fn [{:keys [db-after tx-data tx-meta tempids] :as tx-report}]
                 (js/window.setTimeout #(h/save-tx-report! tx-report) 0)
                 
                 (doseq [e (dedupe (map first tx-data))]
                   (when-let [cs (aget *et-index* e)]
                     (doseq [c cs]
                       #_(js/console.log "Refs=" (.-refs c ))
                       #_(.forceUpdate c)
                       (.setState c
                                  (fn [state props]
                                    #_(println "Setstate " state props)
                                    (let [rst (aget state :rum/state)]
                                      (vswap! rst assoc :rum/args (cons (d/entity db-after e)
                                                                        (next (:rum/args @rst)))))
                                    state))))))))

;; hacks
(register-sub ::scroll-into-view
              (fn [[_]]
                (let [sel (d/entid @conn  [:form/highlight true])]
                  (when-let [cs (->> sel
                                     (aget dbrx/ereactive-components-by-eid))]
                    (doseq [c cs]
                     (some-> (.-refs c)
                             (aget "selected")
                             (doto (js/console.log "Element") )
                             (.scrollIntoView)))))))

#_(def ereactive
  {:init
   (fn [state props]
     (let [eid (some-> props (aget :rum/initial-state) :rum/args first :db/id)]
       (if-not eid
         (do (println "No db/id! Did you mess up deletion?" props)
             state)
         (do (aset *et-index* eid
                   (doto (or (aget *et-index* eid) (js/Array.))
                     (.push (:rum/react-component state))))
             (assoc state ::eid eid)))))
   :will-unmount
   (fn [state]
     (let [c (:rum/react-component state)
           cs (some->> state ::eid (aget *et-index*))]
       (if-not cs
         state
         (loop [i 0]
           (cond
             (= i (.-length cs))
             state

             (js/Object.is c (aget cs i))
             (do (.splice cs i 1)
                 state)
             
             :else (recur (inc i)))))))})

(defn focus-ref-on-mount
  [ref-name]
  {:did-mount (fn [state]
                (some-> state :rum/react-component (.-refs) (aget ref-name) (.focus))
                state)})

(defn scroll-ref-into-view-after-render
  [ref-name]
  {:after-render (fn [state]
                   (some-> state :rum/react-component (.-refs) (aget ref-name) (.scrollIntoView false))
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
  [leaf-class eid ^String text highlight?]
  [:span {:key eid
          :data-eid eid
          :class (if-not highlight?
                   ["tk" leaf-class]
                   ["tk" "selected" leaf-class])
          :on-click (fn [ev]
                      (pub! [::select-form  (-> (.-target ev ) (.-dataset) (aget "eid") (js/parseInt))])
                      (.stopPropagation ev))}
   (-> text 
       ;; replace with non-breaking hyphen, lmao
       (gstring/replaceAll "-" "‑"))])

(declare fcc)

(defn coll-fragment
  [e indent-prop]
  (let [my-indent (-> (:form/indent e)
                      (or 0)
                      (+ indent-prop))]
    (rum/fragment
     (when (:form/linebreak e)
       [:span.indent-chars (str "\n" (.repeat " " my-indent))])
     (case (:coll/type e) :list "(" :vec "[" :map "{")
     (for [x (e/seq->vec e)]
       (rum/with-key (fcc x my-indent) (:db/id x)))
     (case (:coll/type e) :list ")" :vec "]" :map "}"))))

(rum/defc fcc < dbrx/ereactive #_(scroll-ref-into-view-after-render "selected")
  [e indent-prop]
  #_(println "Fcc" indent-prop)
  (or (when (:form/editing e)
        (edit-box (:db/id e)
                  (or (:form/edit-initial e)
                      (some-> (:symbol/value e) str)
                      (some-> (:keyword/value e) str)
                      (:string/value e)
                      (some-> (:number/value e) str))))
      (when-let [s (:symbol/value e)]
        (token-component "s" (:db/id e) (str s) (:form/highlight e)))
      (when-let [s (:keyword/value e)]
        (token-component "k" (:db/id e) (str s) (:form/highlight e)))
      (when-let [s (:string/value e)]
        (token-component "l" (:db/id e) s (:form/highlight e)))
      (when-let [s (:number/value e)]
        (token-component "n" (:db/id e) (str s) (:form/highlight e)))
      
      (when (:coll/type e)
        (if (:form/highlight e)
          [:span.selected {:ref "selected"} (coll-fragment e (+ 2 indent-prop))]
          (coll-fragment e (+ 2 indent-prop))))
      
      (comment "Probably a retracted entity, do nothing")))

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


(defn form-overwrite-tx
  [e replacement-eid]
  (let [spine (first (:seq/_first e))
        coll (first (:coll/_contains e))]
    (when (and spine coll)
      [[:db/retract (:db/id coll) :coll/contains (:db/id e)]
       [:db/add (:db/id coll) :coll/contains replacement-eid]
       [:db/add (:db/id spine) :seq/first replacement-eid]])))

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
    (form-replace-tx parent e)))

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
                                    (some? (:form/indent e)) (assoc :form/indent (:form/indent e)))
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

(defn insert-editing-tx
  [db edit-initial]
  (let [sel (get-selected-form db)
        new-node {:db/id "newnode"
                  :form/editing true
                  :form/edit-initial (or edit-initial "")}]
    (into [new-node]
          (concat (insert-after-tx sel new-node)
                  (move-selection-tx (:db/id sel) "newnode")))))

(register-sub ::edit-new-node-after-selected (->mutation insert-editing-tx))

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

(defmethod move :move/up [_ e]
  (some-> (:coll/_contains e) first))

(defmethod move :move/forward-up [_ e]
  (or (move :move/next-sibling e)
      (move :move/up e)))

(defmethod move :move/backward-up [_ e]
  (or (move :move/prev-sibling e)
      (move :move/up e)))

(defn move-iterate [m e]
  (if-let [d (move m e)]
    (recur m d)
    m))

(defn movement-tx
  [db movement-type]
  (let [src (get-selected-form db)
        dst (move movement-type src)] 
    (if-not dst
      (println "Cannot" movement-type "from" src)
      (move-selection-tx (:db/id src) (:db/id dst)))))

(register-sub ::move (->mutation movement-tx))

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
    (println "ISel" delta (:form/indent sel))
    [[:db/add (:db/id sel) :form/indent (+ delta
                                           (-> (:form/indent sel)
                                               (or 0)))]]))

(register-sub ::indent-form (->mutation indent-selected-form-tx))

(defn linebreak-selected-form-tx
  [db]
  (let [sel (get-selected-form db)
        parent-indent (:form/indent (move :move/up sel))]
    (println "PArentindent" parent-indent)
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



(defn reverse-parents-array
  [e]
  (->> e
       (iterate (partial move :move/up))
       (next)
       (take-while some?)
       (clj->js)
       (.reverse)))

(defn select-1based-nth-reverse-parent-of-selected-tx
  [db n]
  (let [sel (get-selected-form db)
        rpa (reverse-parents-array sel)]
    (when (< 0 n (inc (count rpa)))
      (move-selection-tx (:db/id sel)
                         (:db/id (nth rpa (dec n)))))))

(register-sub ::select-1based-nth-reverse-parent-of-selected (->mutation select-1based-nth-reverse-parent-of-selected-tx))

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

(register-sub ::global-keydown
              (let [tab-index (atom 0)
                    tab-seq   (atom nil)]
                (fn [[_ k]]
                  (case k
                    "S-Shift"
                    nil
                    
                    (= "S-Tab" "Tab")
                    (let [ts (or @tab-seq
                                 (reset! tab-seq (vec (form-eids-by-edit-tx-desc @conn))))]
                      (when-not (= 0 (count ts))
                        (pub! [::select-form
                               (nth ts
                                    (swap! tab-index
                                           update-tab-index
                                           (count ts)
                                           (case k "Tab" 1 "S-Tab" -1)))])))
                    
                    (do (reset! tab-index 0)
                        (reset! tab-seq nil))))))


;; edit box

(defn parse-token-tx
  [text-value eid]
  (try
    (-> text-value
        (edn/read-string)
        (e/->tx)
        (assoc :db/id eid))
    (catch js/Error e
      (println "No edn" text-value)
      (js/console.log e))))

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

(defn editbox-keydown-tx
  [db eid text key]
  (case key
    "Escape"      (reject-edit-tx db eid)
    "Backspace"   (when (empty? text)
                    (reject-edit-tx db eid))
    "Enter"       (finish-edit-tx db eid text)
    (")" "]" "}") (concat (movement-tx db :move/up)
                          (if (empty? text)
                            (reject-edit-tx db eid)
                            (accept-edit-tx eid text)))
    ("[" "(" "{") (wrap-edit-tx db eid (case key "(" :list "[" :vec "}" :map) text)
    " "           (if (empty? text)
                    (reject-edit-tx db eid)
                    (concat (accept-edit-tx eid text)
                            (insert-editing-tx db "")))
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
      :value       value
      :style       {:width (str (max 4 (inc (count value))) "ch")}
      :on-change   #(let [new-text (string/trim (.-value (.-target %)))
                          token (parse-token-tx new-text form-eid)]
                      (reset! text new-text)
                      (reset! editbox-ednparse-state
                              {:form-eid form-eid
                               :text new-text
                               :valid (some? token)
                               :type (some-> token first val)}))
      :on-key-down (fn [ev]
                     (when-let [tx (editbox-keydown-tx @conn form-eid value (.-key ev))]
                       (.preventDefault ev)
                       (.stopPropagation ev)
                       (d/transact! conn tx {:mutation [:editbox/keydown (.-key ev)]
                                             :input-tx-data tx})))
      ;; :on-blur     #(do (.preventDefault %)
      ;;                   (let [tx (finish-edit-tx @conn form-eid value )]
      ;;                     (d/transact! conn tx {:mutation [:editbox/blur]
      ;;                                           :input-tx-data tx})))
      }]))


(defn link-to-form-by-id
  [eid children]
  [:a {:key eid
       :href "#"
       :on-click #(do (.preventDefault %)
                      (pub! [::select-form eid]))}
   children])


(rum/defc breadcrumbs
  [sel top-level-eid]
  (let [rpa (reverse-parents-array sel)]
    [:ul.parent-path
     ;; hack to only show for the active toplevel
     (when (= top-level-eid (:db/id (first rpa)))
       (for [i (range (count rpa))]
         (let [parent (nth rpa i)]
           [:li {:key i}
            (link-to-form-by-id (:db/id parent)
                                [:span.code-font
                                 (when (< i 9)
                                   [:span.parent-index (str (inc i))])
                                 (if (= :list (:coll/type parent))
                                   (or (some-> parent :seq/first :symbol/value) "()")
                                   (case (:coll/type parent) :vec "[]" :map "{}"))])])))]))

(rum/defc focus-info < rum/reactive
  []
  (let [db (rum/react conn)] 
    (when-let [sel (get-selected-form db)]
      [:div 
       [:div (str "#" (:db/id sel)) ]
       
       "EAVT"
       [:pre (with-out-str
               (doseq [[e a v t] (d/datoms db :eavt (:db/id sel))]
                 (println e a (pr-str v))))]
       "AVET"
       [:pre 
        (with-out-str
          (doseq [[k as] schema
                  :when (= :db.type/ref (:db/valueType as) )]
            (doseq [[e a v t] (d/datoms db :avet k (:db/id sel))]
              (println e a v))))]
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
       
       #_[:pre 
          (with-out-str (cljs.pprint/pprint (e/->form sel)))]])))

(def special-key-map
  (merge
   {" "         [::edit-new-node-after-selected]
    "S-\""      [::edit-new-node-after-selected]
    "S-:"       [::edit-new-node-after-selected ":"]
    "Escape"    [::move :move/most-upward]
    "'"         [::edit-new-node-after-selected "'"]
    "("         [::edit-new-wrapped :list]
    "9"         [::edit-new-wrapped :list]
    "["         [::edit-new-wrapped :vec ]
    "{"         [::edit-new-wrapped :map ]
    "u"         [::move :move/up]
    "r"         [::raise-selected-form]
    "S-X"       [::extract-to-new-top-level]
    "a"         [::edit-selected]
    "]"         [::move :move/up]
    "w"         [::exchange-with-previous]
    "s"         [::exchange-with-next]
    "0"         [::move :move/up]
    "j"         [::move :move/next-sibling]
    "v"         [::scroll-into-view]
    "k"         [::move :move/prev-sibling]
    "f"         [::move :move/flow]
    "n"         [::move :move/most-nested]
    "x"         [::delete-with-movement :move/forward-up]
    "Backspace" [::delete-with-movement :move/backward-up]
    "d"         [::duplicate-selected-form]
    "i"         [::indent-form 1]
    "S-I"       [::indent-form -1]
    "Enter"     [::linebreak-form]
    "Tab"       ["Placeholder for the real tab handler which is special because it is stateful"]
    "S-Tab"     ["Same as Tab"]
    "S-M"       [::recursively-set-indent true]
    "S-O"       [::recursively-set-indent false]}
   (into {}
         (for [i (range 9)]
      [(str (inc i)) [::select-1based-nth-reverse-parent-of-selected (inc i)]]))))

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



#_(rum/defc what < rum/reactive
  [top-level-eid]
  (some-> (rum/react conn)
          (get-selected-form)
          (breadcrumbs top-level-eid)))

(rum/defc what < (dbrx/areactive :form/highlight)
  [db toplevel]
  (breadcrumbs (get-selected-form db) toplevel))

(rum/defc toplevel-modeline < rum/reactive (dbrx/areactive :form/editing)
  [db top-eid]
  (let [edit (d/entity db [:form/editing  true])
        rpa (reverse-parents-array edit)
        {:keys [text valid]} (rum/react editbox-ednparse-state)]
    (println "Top-eid" top-eid "First rpa" (first rpa) )
    (when (= top-eid (:db/id (first rpa)))
      [:span {:class (str "modeline code-font"
                          (when edit " editing")
                          (when (and (not (empty? text)) (not valid)) " invalid"))}
       [:span.modeline-content
        #_(pr-str (rum/react editbox-ednparse-state))
        (if edit
          (str "Editing " (:db/id edit))
          "No edit?")]])))

(rum/defc top-level-form-component < dbrx/ereactive
  [e]
  [:div.form-card
   [:span.form-title.code-font (str "#" (:db/id e))]
   (what (d/entity-db e) (:db/id e))
   [:div.top-level-form.code-font
    (fcc e 0)]
   (toplevel-modeline (d/entity-db e) (:db/id e))])



(rum/defc edit-history < rum/reactive
  []
  (let [db (rum/react conn)
        sel-eid (d/entid db  [:form/highlight true])]
    [:ul
     (for [e (form-eids-by-edit-tx-desc db)]
       [:li {:key e}
        (link-to-form-by-id
         e
         (str "Edited #" e
              (when (= e sel-eid)
                " (Selected)"))
         )])]))

(rum/defc root-component < dbrx/ereactive
  [state]
  (let [db @conn]
    [:div.root-cols
     [:div.sidebar-left
      (focus-info)
      (edit-history)
      (h/history-view conn)]
     [:div.main-content
      (for [df (:state/display-form state)]
        (rum/with-key (top-level-form-component df) (:db/id df)))
      #_(all-datoms-table)]]))

(defn event->kbd
  [^KeyboardEvent ev]
  (str (when (.-altKey ev) "M-")
                         (when (.-ctrlKey ev ) "C-")
                         (when (.-shiftKey ev) "S-")
                         (.-key ev))
  )

(defn global-keydown*
  [ev]
  (when-not @global-editing-flag
    (let [kbd (event->kbd ev)]
      (pub! [::global-keydown kbd])
      (if-let [mut (get special-key-map kbd)]
        (do (pub! (with-meta mut {:kbd kbd}))
            (.preventDefault ev)
            (.stopPropagation ev))
        (.log js/console "Key" kbd ev))
      
      #_(if-let [mut (get special-key-map (.-key ev))]
          (do (pub! mut)
              (.preventDefault ev)
              (.stopPropagation ev))
          (.log js/console "Key" ev)))))

(defonce global-keydown
  (fn [ev] 
    (global-keydown* ev)))



(defn  ^:dev/after-load init []
  (js/document.removeEventListener "keydown" global-keydown true)
  
  (js/document.addEventListener "keydown" global-keydown true)
  (h/clear!)
  #_(make-subs!)
  (rum/mount (root-component (d/entity @conn ::state)) (.getElementById js/document "root"))
  #_(pub! [::select-form (:db/id (:state/display-form (d/entity @conn ::state)))]))


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

(ns page
  (:require
   [rewrite-clj.zip :as z]
   [embed :as e]
   [debug :as debug]
   [tx-history :as h]
   [datascript.core :as d]
   [clojure.string :as string]
   [rum.core :as rum]
   [cljs.core.async :as async])
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop]]))

(def schema
  (merge
   e/form-schema
   {:state/hover-form {:db/valueType :db.type/ref}
    :state/display-form {:db/valueType :db.type/ref
                         :db/cardinality :db.cardinality/many}
    ;; :state/selected-form {:db/valueType :db.type/ref}
    :form/editing {:db/unique :db.unique/identity} 
    :form/edit-initial {}
    :form/highlight {:db/unique :db.unique/identity}
    :form/indent {}
    :form/edited-tx {:db/valueType :db.type/ref}}))

(def test-form-data
  '[
    (defn other-thing [bink] bonks)
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
                             {:db/ident ::state
                              :state/display-form (:db/id txe)}]))))))




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

(def ^:dynamic *et-index* (js/Array. 1024))

(defn ->mutation
  [tx-fn]
  (fn [[_ & args :as mutation]]
    (when-let [tx-data (apply tx-fn @conn args)]
      (d/transact! conn tx-data {:mutation mutation
                                 :input-tx-data tx-data}))))

(d/listen! conn
           (-> (fn [{:keys [db-after tx-data tx-meta tempids] :as tx-report}]
                 (js/window.setTimeout #(h/save-tx-report! tx-report) 0)
                 (doseq [e (dedupe (map first tx-data))]
                   (when-let [cs (aget *et-index* e)]
                     (doseq [c cs]
                       (.setState c
                                  (fn [state props]
                                    (let [rst (aget state :rum/state)]
                                      (vswap! rst assoc :rum/args (cons (d/entity db-after e)
                                                                        (next (:rum/args rst)))))
                                    state))))))))

(def ereactive
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

(declare form-component)

(rum/defc form-component*
  [e]
  (let [eid (:db/id e)
        leaf (or (:symbol/value e)
                 (:keyword/value e)
                 (:string/value e)
                 (:number/value e))
        leaf-class (cond
                     (:symbol/value e) "s"
                     (:keyword/value e) "k"
                     (:string/value e) "l"
                     :else  "")]
    (cond
      leaf
      [:span {:key eid
              :data-eid eid
              :class ["tk"
                      leaf-class
                      (when (:form/highlight e)
                        "selected")]
              :on-click (fn [ev]
                          (pub! [::select-form  (-> (.-target ev ) (.-dataset) (aget "eid") (js/parseInt))])
                          (.stopPropagation ev))}
       (str leaf)]
      
      #_(js/React.createElement
         "span"
         #js {:key eid
              :data-eid eid
              :className (str "tk " leaf-class)
              :onClick (fn [ev]
                         (pub! [::select-form  (-> (.-target ev ) (.-dataset) (aget "eid") (js/parseInt))])
                         (.stopPropagation ev))}
         #js [(str leaf)])
      
      (:coll/type e)
      (rum/fragment 
       (case (:coll/type e) :list "(" :vec "[" :map "{")
       (for [x (e/seq->vec e)]
         (rum/with-key (form-component x) (:db/id x)))
       (case (:coll/type e) :list ")" :vec "]" :map "}")))))

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

(rum/defc form-component < ereactive (scroll-ref-into-view-after-render "selected")
  [e]
  (cond
    (:form/editing e)
    (edit-box (:db/id e) (or (:form/edit-initial e)
                             (let [okay (or (some-> (:symbol/value e) str)
                                            (some-> (:keyword/value e) str)
                                            (:string/value e)
                                            (some-> (:number/value e) str))]
                               (println "Okay" (pr-str okay))
                               okay)))
    
    (not (:coll/type e))
    (form-component* e)
    
    (:form/highlight e)
    [:span.selected {:ref "selected"
                     :class (when (:form/indent e) "indented")}
     (form-component* e)]
    
    (:form/indent e)
    [:span.indented (form-component* e)]
    
    ;; #_(or (:form/highlight e) (:form/indent e))
    ;; #_[:span {:ref (if"selected")
    ;;           :class [(when (:form/highlight e) "selected")
    ;;                   (when (:form/indent e) "indented")]}
    ;;    (form-component* e)]
    
    :else
    (form-component* e)))

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

#_(defn form-duplicate-tx
  [e contained-by-eid]
  (cond
    (:symbol/value e)  {:symbol/value e :coll/_contains contained-by-eid}
    (:keyword/value e) {:keyword/value e :coll/_contains contained-by-eid}
    (:string/value e)  {:string/value e :coll/_contains contained-by-eid}
    (:number/value e)  {:number/value e :coll/_contains contained-by-eid}
    (:coll/type e)     (let [cid (e/new-tempid)]
                         (cond-> {:db/id cid :coll/type e}
                           (:seq/first e) (assoc :seq/first (form-duplicate-tx (:seq/first e) cid ))
                           (:seq/next e)  (assoc :seq/next (form-duplicate-tx (:seq/next e) cid))))
    :else (println "What is this?" e)))



(def duplicate-keys [:form/indent])
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
                                    (:form/indent e) (assoc :form/indent true))
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
  [db]
  (let [sel (get-selected-form db)]
    [[:db/add (:db/id sel) :form/indent (not (:form/indent sel))]]))

(register-sub ::indent-form (->mutation indent-selected-form-tx))

(defn recursively-set-indent-tx
  [db indent]
  (let [sel (get-selected-form db)]
    (for [e (tree-seq :coll/type e/seq->vec sel)]
      (when (:coll/type e)
       [:db/add (:db/id e) :form/indent indent]))))

(register-sub ::recursively-set-indent (->mutation recursively-set-indent-tx))

;; history

(defn form-eids-by-edit-tx-desc
  [db]
  (->> (d/rseek-datoms db :avet :form/edited-tx)
       (take-while (fn [[_ a]] (= a :form/edited-tx)))
       (map (fn [[e]] e))))

(register-sub ::global-keydown
              (let [tab-index (atom 0)
                    tab-seq (atom nil)]
                (fn [[_ k]]
                  (case k
                    "Tab"
                    (let [ts (or @tab-seq
                                 (reset! tab-seq (vec (form-eids-by-edit-tx-desc @conn))))]
                      (when-not (= 0 (count ts))
                        (let [ti (swap! tab-index #(rem (inc %) (count ts)))]
                          (pub! [::select-form (nth ts ti)]))))
                    
                    (do
                      (println 'Reset-tab-state )
                      (reset! tab-index 0)
                      (reset! tab-seq nil))))))


;; edit box

(defn accept-edit-tx
  [form-eid value]
  [{:db/id :db/current-tx}
   [:db/add form-eid :symbol/value value]
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
      :on-change   #(reset! text (string/trim (.-value (.-target %))))
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


(rum/defc focus-info < rum/reactive
  []
  (let [db (rum/react conn)] 
    (when-let [sel (get-selected-form db)]
      [:div 
       [:div (str "#" (:db/id sel)) ]
       "EAVT"
       [:pre (with-out-str
               (doseq [[e a v t] (d/datoms db :eavt (:db/id sel))]
                 (println e a v)))]
       "AVET"
       [:pre 
        (with-out-str
          (doseq [[k as] schema
                  :when (= :db.type/ref (:db/valueType as) )]
            (doseq [[e a v t] (d/datoms db :avet k (:db/id sel))]
              (println e a v))))]
       #_[:pre 
        (with-out-str (cljs.pprint/pprint (e/->form sel)))]])))

(def special-key-map
  {" "         [::edit-new-node-after-selected]
   "\""        [::edit-new-node-after-selected]
   ":"         [::edit-new-node-after-selected ":"]
   "'"         [::edit-new-node-after-selected "'"]
   "("         [::edit-new-wrapped :list]
   "9"         [::edit-new-wrapped :list]
   "["         [::edit-new-wrapped :vec ]
   "{"         [::edit-new-wrapped :map ]
   "u"         [::move :move/up]
   "r"         [::raise-selected-form]
   "a"         [::edit-selected]
   "]"         [::move :move/up]
   "0"         [::move :move/up]
   "j"         [::move :move/next-sibling]
   "k"         [::move :move/prev-sibling]
   "f"         [::move :move/flow]
   "n"         [::move :move/most-nested]
   "x"         [::delete-with-movement :move/forward-up]
   "Backspace" [::delete-with-movement :move/backward-up]
   "d"         [::duplicate-selected-form]
   "i"         [::indent-form]
   "Tab"       [::tabby]
   "M" [::recursively-set-indent true]
   "O" [::recursively-set-indent false]})

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



(rum/defc top-level-form-component < ereactive
  [e]
  [:div.form-card
   [:span.form-title.code-font (str "#" (:db/id e))]
   [:div.top-level-form.code-font (form-component e)]])

(rum/defc edit-history < rum/reactive
  []
  (let [db (rum/react conn)
        sel-eid (d/entid db  [:form/highlight true])]
    [:ul
     (for [e (form-eids-by-edit-tx-desc db)]
       [:div {:key e}
        [:a {:href "#"
             :on-click #(do (.preventDefault %)
                            (pub! [::select-form e]))}
         (str "Edited #" e)
         (when (= e sel-eid)
           " (Selected)")]])]))

(rum/defc root-component
  []
  (let [db @conn]
    [:div.root-cols
     [:div.sidebar-left
      (focus-info)
      (edit-history)
      (h/history-view conn)]
     [:div {:style {:display :flex
                    :flex-direction :column}}
      (for [df (:state/display-form (d/entity db ::state))]
        (rum/with-key (top-level-form-component df) (:db/id df)))]]))

(defn global-keydown*
  [ev]
  (when-not (or @global-editing-flag
                (.-ctrlKey ev))
    (let [jj (str (when (.-altKey ev) "M-")
                  (when (.-ctrlKey ev ) "C-")
                  (when (.-shiftKey ev) "S-")
                  (.-key ev))]
      (pub! [::global-keydown jj])
      (if-let [mut (get special-key-map (.-key ev))]
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
  (rum/mount (root-component) (.getElementById js/document "root"))
  #_(pub! [::select-form (:db/id (:state/display-form (d/entity @conn ::state)))]))

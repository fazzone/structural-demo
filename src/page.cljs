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
    :state/display-form {:db/valueType :db.type/ref}
    ;; :state/selected-form {:db/valueType :db.type/ref}
    :form/editing {:db/unique :db.unique/identity} 
    :form/edit-initial {}
    :form/highlight {:db/unique :db.unique/identity}
    :form/indent {}}))

(def test-form-data '(defn ->tx
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
                           (map? e)       (coll-tx :map (flatten-map e))))))
#_(def test-form-data '[f :fo  cb ])

(def conn
  (let [txe (update (e/->tx test-form-data) :db/id #(or % "top"))]
    (doto (d/create-conn schema)
      (d/transact! [txe
                    {:db/ident ::state
                     :state/display-form (:db/id txe)}]))))


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
(def ^:dynamic *last-tx-report* (atom nil))

(defmulti message->mutation* (fn [_db msg & params] msg))

(defmethod message->mutation* :default [_ msg a b c]
  (println "?" msg a))



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
       #_(for [[a b] (partition-all 2 (e/seq->vec e))]
         (if-not b
           (rum/with-key (form-component a) (:db/id a))
           (rum/fragment
            (rum/with-key (form-component a) (:db/id a))
            " "
            (rum/with-key (form-component b) (:db/id b)))))
       (for [x (e/seq->vec e)]
         (rum/with-key (form-component x) (:db/id x)))
       (case (:coll/type e) :list ")" :vec "]" :map "}")))))

(defn focus-ref-on-mount
  [ref-name]
  {:did-mount (fn [state]
                (.focus (aget (.-refs (:rum/react-component state)) ref-name))
                state) })

(def global-editing-flag (atom false))
(def editing-when-mounted
  {:did-mount (fn [state]
                (reset! global-editing-flag true)
                state)
   :will-unmount (fn [state]
                   (reset! global-editing-flag false)
                   state)})

(declare edit-box)

(rum/defc form-component < ereactive
  [e]
  (cond
    (:form/editing e)
    (edit-box (:db/id e) (:form/edit-initial e))
    
    (not (:coll/type e))
    (form-component* e)
    
    (or (:form/highlight e) (:form/indent e))
    [:span {:class [(when (:form/highlight e) "selected")
                    (when (:form/indent e) "indented")]}
     (form-component* e)]
    
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



(defn form-duplicate-tx
  [e]
  (letfn [#_(seq-dup [head]
            (if-let [x (:seq/first head)]
              (cond-> {:seq/first x}
                (:seq/next head) (assoc :seq/next (seq-dup (:seq/next head))))))]
   (cond 
     (:symbol/value e)  {:symbol/value (:symbol/value e)}
     (:keyword/value e) {:keyword/value (:keyword/value e)}
     (:string/value e)  {:string/value (:string/value e)}
     (:number/value e)  {:number/value (:number/value e)}
     ;; (:coll/type e)     (let [id (e/new-tempid)]
     ;;                      (cond-> {:db/id id :coll/type (:coll/type e)}
     ;;                        (:seq/first e) (merge
     ;;                                        (seq-dup (for [x xs]
     ;;                                                   (assoc (form-duplicate-tx x) :coll/_contains id))))))
     )))



(defn insert-duplicate-tx
  [db]
  (let [sel      (get-selected-form db)
        new-node (assoc (form-duplicate-tx sel) :db/id "dup") 
        txd      (into [new-node]
                       (concat (insert-after-tx sel new-node)
                               (move-selection-tx (:db/id sel) "dup")))]
    (println "New node" new-node)
    (println "IDT" txd)
    txd))

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



;; edit box

(defn accept-edit-tx
  [form-eid value]
  [[:db/retract form-eid :form/editing true]
   [:db/add form-eid :symbol/value value]])

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
      :style       {:width (str (max 4 (count value)) "ch")}
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
       [:pre (with-out-str (run! (partial apply prn) (d/touch sel)))]
       [:pre (with-out-str
               (doseq [[k as] schema
                       :when (= :db.type/ref (:db/valueType as) )]
                 (doseq [[e a v t] (d/datoms db :avet k (:db/id sel))]
                   (println e a v))))]
       [:pre (pr-str (e/->form sel))]
       [:pre {:style {:width "30em"
                      :overflow :auto}}
        (pr-str (e/->form (d/entity db 1)))]
       #_[:pre {:style {:width "30em"
                      :overflow :auto}}
        (with-out-str
          (doseq [el (tree-seq :coll/type e/seq->vec (d/entity db 1))]
            (prn (:db/id el ) (e/->form el))))]])))

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
   "]"         [::move :move/up]
   "0"         [::move :move/up]
   "j"         [::move :move/next-sibling]
   "k"         [::move :move/prev-sibling]
   "f"         [::move :move/flow]
   "x"         [::delete-with-movement :move/forward-up]
   "Backspace" [::delete-with-movement :move/backward-up]
   "d"         [::duplicate-selected-form]
   "i"         [::indent-form]
   "M"         [::recursively-set-indent true]
   "O"         [::recursively-set-indent false]})

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

(rum/defc last-tx-table < rum/reactive []
  (let [{:keys [tx-data tempids]} (rum/react *last-tx-report*)]
    (when tx-data
      [:div
       (str "Transaction #" (:db/current-tx tempids))
       [:pre (with-out-str
               (doseq [[e a v t a?] tx-data]
                 (println "[" e a (pr-str v) t a? "]")))]])))

(rum/defc top-level-form-component < ereactive
  [state]
  (form-component (:state/display-form state)))

(rum/defc root-component
  []
  (let [db @conn]
    [:div 
     [:div {:style {:display :flex
                    :flex-direction :column
                    :justify-content :center
                    :align-items :center
                    ;; :height "80vh"
                    }}
      [:div.top-level-form.code-font
       {:style {:width "900px"
                :overflow :auto
                :resize :both
                :border "1px solid #ae81ff"}}
       [:span {:style {:overflow  :wrap}}
        (top-level-form-component (d/entity db ::state))]]]
     
     [:div {:style {:display :flex
                    :flex-direction :row
                    :justify-content :space-evenly}}
      (h/history-view conn)
      [:div {:style {:width "50%" }}
       (focus-info)]]
     
     #_(text-import-form)
     #_(all-datoms-table)
     #_(key-bindings-table)
     #_(debug/transaction-edit-area conn)
     #_(notes)]))

(defn global-keydown*
  [ev]
  (when-not (or @global-editing-flag
                (.-ctrlKey ev))
    (if-let [mut (get special-key-map (.-key ev))]
      (do (pub! mut)
          (.preventDefault ev)
          (.stopPropagation ev))
      (.log js/console "Key" ev))))

(defonce global-keydown
  (fn [ev] 
    (global-keydown* ev)))



(defn  ^:dev/after-load init []
  (js/document.removeEventListener "keydown" global-keydown true)
  
  (js/document.addEventListener "keydown" global-keydown true)
  (h/clear!)
  #_(make-subs!)
  (rum/mount (root-component) (.getElementById js/document "root"))
  (pub! [::select-form (:db/id (:state/display-form (d/entity @conn ::state)))]))

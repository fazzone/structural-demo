(ns page
  (:require
   [rewrite-clj.zip :as z]
   [embed :as e]
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
    :state/selected-form {:db/valueType :db.type/ref}
    :form/editing {:db/unique :db.unique/identity} 
    :form/highlight {:db/index true}
    
    }))

(def test-form-data '(d/listen! conn
                                (-> (fn [{:keys [db-after tx-data tempids] :as tx-report}]
                                      (->> tx-data
                                           (reduce
                                            (fn [prev [e _ _ t]]
                                              (when-not (= e prev)
                                                (println "Max T write" e t)
                                                (aset e-max-t-index e t))
                                              e)))
                                      (doseq [e (dedupe (map first tx-data))]
                                        (when-let [cs (aget *et-index* e)]
                                          (doseq [c cs]
                                            (.setState c
                                                       (fn [state props]
                                                         (let [rst (aget state :rum/state)]
                                                           (vswap! rst assoc :rum/args (cons (d/entity db-after e)
                                                                                             (next (:rum/args rst)))))
                                                         state)))))))))
#_(def test-form-data '[:a :b :c (1 2 3) "hi" [:singleton] "last"])

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
      (action (async/<! ch))
      (recur))))

(defn pub! [e]
  (async/put! bus e))

(def ^:dynamic *et-index* (js/Array. 1024))

(d/listen! conn
           (-> (fn [{:keys [db-after tx-data tempids] :as tx-report}]
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
         (do (println "No db/id! Did you mess up deletion?"
                      (some-> props (aget :rum/initial-state) :rum/args first))
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

(declare fcnn)
(rum/defc kvc < ereactive
  [{:kv/keys [key value]}]
  (rum/fragment
   (rum/with-key (fcnn key) (:db/id key))
   (rum/with-key (fcnn value) (:db/id value))))

(rum/defc map-entry-component < ereactive
  [{:map-entry/keys [key val]}]
  (rum/fragment
   (rum/with-key (fcnn key) (:db/id key))
   (rum/with-key (fcnn val) (:db/id val))))

(rum/defc fcnn*  ;; < ereactive
  [e]
  (let [eid (:db/id e)
        mouse-enter-tx [[:db/add eid :form/highlight true]]
        mouse-out-tx [[:db/add eid :form/highlight false]]
        a {:key eid
           :style (when (:form/highlight e)
                    {:background-color "tomato"})
           :on-mouse-enter (fn [ev]
                             (d/transact! conn mouse-enter-tx)
                             (.stopPropagation ev))
           :on-mouse-out (fn [ev]
                           (d/transact! conn mouse-out-tx)
                           (.stopPropagation ev))
           :on-click (fn [ev]
                       (pub! [::select-form eid])
                       (.stopPropagation ev))}
        leaf (or (:symbol/value e)
                 (:keyword/value e)
                 (:string/value e)
                 (:number/value e))]
    (cond
      leaf
      (js/React.createElement "span" #js {:key eid
                                          :className "ε"
                                          :onClick (fn [ev]
                                                      (pub! [::select-form eid])
                                                      (.stopPropagation ev))}
                              #js [(str leaf)])
      
      (:coll/type e)
      (rum/fragment
       (when (= :list (:coll/type e))
         [:span {:style {:white-space :pre}} "\n"])
       (case (:coll/type e) :list "(" :vec "[" :map "{")
       (for [x (e/seq->vec (:coll/elements e))]
         (rum/with-key (fcnn x) (:db/id x)))
       (case (:coll/type e) :list ")" :vec "]" :map "}"))
      
      (:map-entry/key e)
      (rum/with-key (map-entry-component e) (:db/id e)))))




(defn focus-ref-on-mount
  [ref-name]
  {:did-mount (fn [state]
                (.focus (aget (.-refs (:rum/react-component state)) ref-name))
                state) })

(def global-editing-flag (atom false))
(def editing-when-mounted
  {:did-mount (fn [state]
                (reset! global-editing-flag true)
                #_(println "Global editing flag TRUE")
                state)
   :will-unmount (fn [state]
                   (reset! global-editing-flag false)
                   #_(println "Global editing flag FALSE")
                   state)})

(rum/defcs edit-box
  < (rum/local "" ::text) (focus-ref-on-mount "the-input") editing-when-mounted
  [{::keys [text]} form-eid]
  (letfn [(accept [ev]
            (d/transact! conn [[:db/retract form-eid :form/editing true]
                               [:db/add form-eid :symbol/value @text]])
            (.preventDefault ev))] 
    [:form.edit-box {:on-submit accept}
     [:input {:type :text
              :ref "the-input"
              :value @text
              :style {:width (str (count @text) "ch")}
              :on-change (fn [ev]
                           (reset! text (.-value (.-target ev))))
              :on-blur accept}]]))

(rum/defc fcnn < ereactive
  [e]
  (cond
    (:form/editing e)
    (edit-box (:db/id e))
    
    (:form/highlight e)
    [:span.selected (fcnn* e)]
    
    :else
    (fcnn* e)))


(rum/defc datoms-table-eavt* [ds]
  [:table
    [:thead
     [:tr
      [:td {:style {:width "3em"}} "E"]
      [:td {:style {:width "20em"}} "A"]
      [:td {:style {:width "20em"}} "V"]
      [:td {:style {:width "10em"}} "T"]
      [:td "added?"]]]
    [:tbody
     {} 
     (->> ds
          (map-indexed
           (fn [i [e a v t r]]
             [:tr {:key i}
              [:td [:code (str e)]]
              [:td [:code (str a)]]
              [:td [:code (str v)]]
              [:td [:code (str t)]]
              [:td [:code (str r)]]])))]])

(rum/defc datoms-table < rum/reactive []
  (datoms-table-eavt* (d/datoms (rum/react conn) :eavt)))

(defn select-form*
  [eid]
  (let [other-highlight (d/datoms @conn :avet :form/highlight)]
    #_(println "select-form*" eid)
    (d/transact! conn
                 (into [{:db/ident ::state
                         :state/selected-form eid}
                        [:db/add eid :form/highlight true]]
                       (for [[e] other-highlight
                             :when (not= e eid)]
                         [:db/add e :form/highlight false])))))

(register-sub ::select-form (fn [[_ eid]] (select-form* eid)))

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

(register-sub ::edit-new-node-after-selected
              (fn [[_]]
                (let [db @conn
                      {:state/keys [selected-form]} (d/entity db ::state)
                      new-node {:db/id "newnode" :form/editing true}
                      {:keys [tempids]} (d/transact! conn
                                                     (into [new-node]
                                                           (insert-after-tx selected-form new-node)))]
                  (select-form* (get tempids "newnode") ))))

(register-sub ::edit-new-wrapped
              (fn [[_ coll-type]]
                (let [db @conn
                      {:state/keys [selected-form]} (d/entity db ::state)
                      new-node {:db/id "newnode"
                                :coll/type coll-type
                                :coll/elements {:seq/first {:db/id "inner"
                                                            :coll/_contains "newnode"
                                                            :form/editing true}}}
                      {:keys [tempids tx-data]} (d/transact! conn
                                                             (into [new-node]
                                                                   (insert-after-tx selected-form new-node)))]
                  (select-form* (get tempids "inner") ))))

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
  (or (some-> (:coll/elements e) :seq/first)
      (flow* e)))

(defmethod move :move/up [_ e]
  (some-> (:coll/_contains e) first))

(defmethod move :move/backward-up [_ e]
  (or (move :move/prev-sibling e)
      (move :move/up e)))

(defn move-iterate [m e]
  (if-let [d (move m e)]
    (recur m d)
    m))

(register-sub ::move
              (fn [[_ movement-type]]
                (let [{:state/keys [selected-form]} (d/entity @conn ::state)] 
                  (some->> selected-form
                           (move movement-type)
                           (:db/id)
                           (select-form*)))))

(register-sub ::select-display-form
              (fn [[_ ]]
                (let [{:state/keys [display-form]} (d/entity @conn ::state)] 
                  (select-form* (:db/id display-form)))))


(defn form-delete-tx
  [e]
  (let [spine (first (:seq/_first e))
        next  (some-> spine :seq/next)
        prev  (some-> spine :seq/_next first)
        coll  (some-> spine :coll/_elements first)]
    [[:db/retractEntity (:db/id e)]
     (cond
       (and prev next) [:db/add (:db/id prev) :seq/next (:db/id next)]
       prev            [:db/retract (:db/id prev) :seq/next (:db/id spine)]
       next            [:db/add (:db/id coll) :coll/elements (:db/id next)])]))


(register-sub ::delete-with-movement
              (fn [[_ movement-type]]
                (let [db @conn
                      {:state/keys [selected-form]} (d/entity db ::state)
                      next-selection (some-> (move movement-type selected-form) :db/id)]
                  #_(prn 'typeconn (type conn))
                  (cljs.pprint/pprint {:form-delete-tx (form-delete-tx selected-form)})
                  (d/transact! conn (form-delete-tx selected-form))
                  (select-form* next-selection)
                  (println "Retractto" (:d/entity @conn (:db/id next-selection)))
                  #_(pub! [::select-form (:db/id next-cursor)]))))



(rum/defc focus-info < rum/reactive
  []
  (let [{:state/keys [display-form selected-form]} (d/entity (rum/react conn) ::state)]
    (when selected-form
      [:div 
       [:div (str "#" (:db/id selected-form)) ]
       [:pre (pr-str (e/->form display-form))]
       [:pre (pr-str (e/->form selected-form))]])))

(def special-key-map
  {" "         [::edit-new-node-after-selected]
   "\""        [::edit-new-node-after-selected]
   ":"         [::edit-new-node-after-selected]
   "Escape"    [::select-display-form]
   "("         [::edit-new-wrapped :list]
   "["         [::edit-new-wrapped :vec ]
   "u"         [::move :move/up]
   "j"         [::move :move/next-sibling]
   "k"         [::move :move/prev-sibling]
   "f"         [::move :move/flow]
   "b"         [::move :move/flow-prev]
   "x"         [::delete-with-movement :move/next-sibling]
   "Backspace" [::delete-with-movement :move/backward-up]
   })

(rum/defc key-bindings-table []
  [:table
    [:thead
     [:tr
      [:td {:style {:width "8em"}} "Key"]
      [:td {:style {:width "30em"}} "Message (click to send)"]]]
    [:tbody
     {} 
     (for [[k msg] special-key-map]
       [:tr {:key k}
        [:td [:code k]]
        [:td [:button {:style {:font-family "monospace"}
                       :on-click #(pub! msg) } (pr-str msg)]]])]])



(rum/defc root-component ;; < rum/reactive []
  []
  (let [ ;; db (rum/react conn)
        db @conn
        ]
    [:div 
     [:div {:style {:display :flex
                    :flex-direction :column
                    :justify-content :center
                    :align-items :center
                    :height "50vh"}}
      [:div.top-level-form
       {:style {:width "900px"
                :overflow :auto
                :resize :both
                :height "300px"
                :border "1px solid tomato"}}
       #_(form-component (:state/display-form (d/entity db ::state)))
       (fcnn (:state/display-form (d/entity db ::state)))]]
     (focus-info)
     (key-bindings-table)
     #_(datoms-table)]))

(defn global-keydown*
  [ev]
  (when-not @global-editing-flag
    (if-let [mut (get special-key-map (.-key ev))]
      (do (pub! mut)
          (.stopPropagation ev))
      (.log js/console "Key" ev))))

(defonce global-keydown
  (fn [ev] 
    (global-keydown* ev)))

(defn  ^:dev/after-load init []
  (let [el (.getElementById js/document "root")]
    (js/document.removeEventListener "keydown" global-keydown true)
    (js/document.addEventListener "keydown" global-keydown true)
    
    (when el
      (rum/mount (root-component) el)
      (pub! [::select-form (:db/id (:state/display-form (d/entity @conn ::state)))])
      )))

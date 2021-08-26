(ns page
  (:require
   [rewrite-clj.zip :as z]
   [embed :as e]
   [debug :as debug]
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
    :form/edit-initial {}
    :form/highlight {:db/unique :db.unique/identity}}))

(def test-form-data '(defn register-sub
                       [topic action]
                       (let [ch (async/chan)]
                         (async/sub the-pub topic ch)
                         (go-loop []
                           (action (async/<! ch))
                           (recur)))))

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

(declare form-component)
(rum/defc map-entry-component < ereactive
  [{:map-entry/keys [key val]}]
  (rum/fragment
   (rum/with-key (form-component key) (:db/id key))
   (rum/with-key (form-component val) (:db/id val))))

(rum/defc form-component*  ;; < ereactive
  [e]
  (let [eid (:db/id e)
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
       (case (:coll/type e) :list "(" :vec "[" :map "{")
       (for [x (e/seq->vec (:coll/elements e))]
         (rum/with-key (form-component x) (:db/id x)))
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

(declare edit-box)
(rum/defc form-component < ereactive
  [e]
  (cond
    (:form/editing e)
    (edit-box (:db/id e) (:form/edit-initial e))
    
    (:form/highlight e)
    [:span.selected (form-component* e)]
    
    :else
    (form-component* e)))



(defn select-form-tx
  [eid]
  (let [prev (d/entity @conn [:form/highlight true]) ]
    [{:db/ident ::state
      :state/selected-form eid}
     (when prev
       [:db/retract (:db/id prev) :form/highlight true ])
     [:db/add eid :form/highlight true]]))

(register-sub ::select-form (fn [[_ eid]] (d/transact! conn (select-form-tx eid))))

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
              (fn [[_ init]]
                (let [db @conn
                      {:state/keys [selected-form]} (d/entity db ::state)
                      new-node {:db/id "newnode"
                                :form/editing true
                                :form/edit-initial (or init "")}]
                  (d/transact! conn
                               (into [new-node]
                                     (insert-after-tx selected-form new-node))))))

(register-sub ::edit-new-wrapped
              (fn [[_ coll-type init]]
                (let [db @conn
                      {:state/keys [selected-form]} (d/entity db ::state)
                      new-node {:db/id "newnode"
                                :coll/type coll-type
                                :coll/elements {:seq/first {:db/id "inner"
                                                            :coll/_contains "newnode"
                                                            :form/edit-initial (or init "")
                                                            :form/editing true}}}]
                  (d/transact! conn
                               (into [new-node]
                                     (insert-after-tx selected-form new-node))))))

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

(register-sub ::move
              (fn [[_ movement-type]]
                (let [{:state/keys [selected-form]} (d/entity @conn ::state)] 
                  (some->> selected-form
                           (move movement-type)
                           (:db/id)
                           (select-form-tx)
                           (d/transact! conn)))))

(register-sub ::select-display-form
              (fn [[_ ]]
                (let [{:state/keys [display-form]} (d/entity @conn ::state)] 
                  (d/transact! conn (select-form-tx (:db/id display-form))))))

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
                      next-selection (some-> (move movement-type selected-form))]
                  (prn "DWM " movement-type next-selection)
                  (d/transact! conn
                               (concat (form-delete-tx selected-form)
                                       (select-form-tx (:db/id next-selection)))))))

(defn accept-edit!
  [form-eid value]
  (d/transact! conn (into [[:db/retract form-eid :form/editing true]
                           [:db/add form-eid :symbol/value value]]
                          (select-form-tx form-eid))))

(defn reject-edit!
  [form-eid]
  (d/transact! conn (form-delete-tx (d/entity @conn form-eid))))

(defn finish-edit!
  [form-eid value]
  (if-not (empty? value)
    (accept-edit! form-eid value)
    (do (reject-edit! form-eid)
        nil)))

(rum/defcs edit-box
  < (rum/local ::empty ::text) (focus-ref-on-mount "the-input") editing-when-mounted
  [{::keys [text]} form-eid init]
  (let [value (if (= ::empty @text)
                init
                @text)]
    [:form.edit-box {:on-submit #(do (accept-edit! form-eid value)
                                     (.preventDefault %))}
     [:input {:type :text
              :ref "the-input"
              :value value
              :style {:width (str (count value) "ch")}
              :on-change #(reset! text (string/trim (.-value (.-target %))))
              :on-key-down (fn [ev]
                             (case (.-key ev )
                               ("Escape") (reject-edit! form-eid)
                               "Backspace" (when (empty? @text)
                                             (reject-edit! form-eid))
                               (")" "]") (when (finish-edit! form-eid value)
                                           (println "The edit was finished!")
                                           (pub! [::move :move/up]))
                               " " (do (.preventDefault ev)
                                       (when (finish-edit! form-eid value)
                                         (pub! [::edit-new-node-after-selected])))
                               nil))
              :on-blur #(do (accept-edit! form-eid value)
                            (.preventDefault %))}]])) 


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
   ":"         [::edit-new-node-after-selected ":"]
   "'"         [::edit-new-node-after-selected "'"]
   "Escape"    [::select-display-form]
   "("         [::edit-new-wrapped :list]
   "9"         [::edit-new-wrapped :list]
   "["         [::edit-new-wrapped :vec ]
   "u"         [::move :move/up]
   "]"         [::move :move/up]
   "0"         [::move :move/up]
   "j"         [::move :move/next-sibling]
   "k"         [::move :move/prev-sibling]
   "f"         [::move :move/flow]
   "x"         [::delete-with-movement :move/forward-up]
   "Backspace" [::delete-with-movement :move/backward-up]})

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
                       :on-click #(pub! msg) } (pr-str msg)]]])]])


(rum/defc all-datoms-table < rum/reactive []
  (debug/datoms-table-eavt* (d/datoms (rum/react conn) :eavt)))

(rum/defc root-component
  []
  (let [db @conn]
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
                :border "1px solid #ae81ff"}}
       (form-component (:state/display-form (d/entity db ::state)))]]
     (focus-info)
     #_(all-datoms-table)
     (key-bindings-table)]))

(defn global-keydown*
  [ev]
  (when-not @global-editing-flag
    (if-let [mut (get special-key-map (.-key ev))]
      (do (pub! mut)
          (.preventDefault ev)
          (.stopPropagation ev))
      #_(.log js/console "Key" ev))))

(defonce global-keydown
  (fn [ev] 
    (global-keydown* ev)))

(defn  ^:dev/after-load init []
  (js/document.removeEventListener "keydown" global-keydown true)
  (js/document.addEventListener "keydown" global-keydown true)
  
  (rum/mount (root-component) (.getElementById js/document "root"))
  (pub! [::select-form (:db/id (:state/display-form (d/entity @conn ::state)))]))

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
    :state/selected-form {:db/valueType :db.type/ref}
    :form/editing {:db/unique :db.unique/identity} 
    :form/edit-initial {}
    :form/highlight {:db/unique :db.unique/identity}}))

(def test-form-data '[nothing here])

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
(def ^:dynamic *last-tx-report* (atom nil))

(d/listen! conn
           (-> (fn [{:keys [db-after tx-data tempids] :as tx-report}]
                 (reset! *last-tx-report* tx-report)
                 #_(println h/save-tx-report!)
                 (h/save-tx-report! tx-report)
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
                      (some-> props (aget :rum/initial-state)))
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

(defn leaf-span
  [e]
  #_(or (when-let [v (:symbol/value e)] (leaf-span "s" v))
      (when-let [v (:keyword/value e)] (leaf-span "k" v))
      (when-let [v (:string/value e)] (leaf-span "p" v))
      (when-let [v (:number/value e)] (leaf-span "n" v)))
  
  )



(rum/defc form-component* ;; < ereactive
  [e]
  (let [eid (:db/id e)
        leaf (or (:symbol/value e)
                 (:keyword/value e)
                 (:string/value e)
                 (:number/value e))]
    (cond
      leaf
      (js/React.createElement
       "span"
       #js {:key eid
            :data-eid eid
            :className "tk k"
            :onClick (fn [ev]
                       (pub! [::select-form  (-> (.-target ev ) (.-dataset) (aget "eid") (js/parseInt))])
                       (.stopPropagation ev))}
       #js [(str leaf)])
      
      (:coll/type e)
      (rum/fragment 
       (case (:coll/type e) :list "(" :vec "[" :map "{")
       (for [x (e/seq->vec (:coll/elements e))]
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

(defn retract-current-highlight-tx
  []
  (when-let [h (d/entity @conn [:form/highlight true])]
    [:db/retract (:db/id h) :form/highlight true]))

(defn move-selection-tx
  [prev-sel-eid new-sel-eid]
  [{:db/ident ::state
    :state/selected-form new-sel-eid}
   [:db/retract prev-sel-eid :form/highlight true]
   [:db/add new-sel-eid :form/highlight true]])

(defn select-form-tx
  [eid]
  [{:db/ident ::state
    :state/selected-form eid}
   (retract-current-highlight-tx)
   [:db/add eid :form/highlight true]])

(register-sub ::select-form (fn [[_ eid]] (d/transact! conn (select-form-tx eid))))

(register-sub ::import-form-text
              (fn [[_ text]]
                (let [txe (update (e/->tx (cljs.reader/read-string text)) :db/id #(or % "top"))]
                  (d/transact! conn (into [txe
                                           {:db/ident ::state
                                            :state/display-form (:db/id txe)}]
                                          (select-form-tx (:db/id txe)))))))

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
        prev  (some-> spine :seq/_next first)
        coll  (some-> spine :coll/_elements first)]
    [[:db/retractEntity (:db/id e)]
     (when coll
       [:db/retract (:db/id coll) :coll/elements (:db/id spine)])
     (cond
       (and prev next) [:db/add (:db/id prev) :seq/next (:db/id next)]
       prev            [:db/retract (:db/id prev) :seq/next (:db/id spine)]
       next            [:db/add (:db/id coll) :coll/elements (:db/id next)])]))

(defn form-replace-tx
  [e replacement-eid]
  (when-let [spine (first (:seq/_first e))]
    (when-let [coll (first (:coll/_contains e))]
      [[:db/retract (:db/id coll) :coll/contains (:db/id e)]
       [:db/add (:db/id coll) :coll/contains replacement-eid]
       [:db/add (:db/id spine) :seq/first replacement-eid]])))

(defn insert-editing-tx
  [edit-initial]
  (let [db @conn
        {:state/keys [selected-form]} (d/entity db ::state)
        new-node {:db/id "newnode"
                  :form/editing true
                  :form/edit-initial (or edit-initial "")}]
    
    (into [new-node]
          (concat
           (insert-after-tx selected-form new-node)
           (select-form-tx "newnode")))))

(register-sub ::edit-new-node-after-selected
              (fn [[_ init]]
                (d/transact! conn (insert-editing-tx init))))

(register-sub ::edit-new-wrapped
              (fn [[_ coll-type init]]
                #_(let [{:state/keys [selected-form]} (d/entity @conn ::state)]
                    (d/transact! conn
                                 (wrap-edit-tx (:db/id selected-form) coll-type init)))
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
                                     (concat
                                      (insert-after-tx selected-form new-node)
                                      (move-selection-tx (:db/id selected-form) "inner")))))))

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

(defn movement-tx
  [movement-type]
  (let [{:state/keys [selected-form]} (d/entity @conn ::state)] 
    (some->> selected-form
             (move movement-type)
             (:db/id)
             (select-form-tx))))

(register-sub ::move
              (fn [[_ movement-type]]
                (d/transact! conn (movement-tx movement-type))))

(register-sub ::select-display-form
              (fn [[_ ]]
                (let [{:state/keys [display-form]} (d/entity @conn ::state)] 
                  (d/transact! conn (select-form-tx (:db/id display-form))))))

(register-sub ::delete-with-movement
              (fn [[_ movement-type]]
                (let [db @conn
                      {:state/keys [selected-form]} (d/entity db ::state)
                      next-selection (some-> (move movement-type selected-form))]
                  (d/transact! conn
                               (concat (form-delete-tx selected-form)
                                       (select-form-tx (:db/id next-selection)))))))

(defn accept-edit-tx
  [form-eid value]
  [[:db/retract form-eid :form/editing true]
   [:db/add form-eid :symbol/value value]])

(defn reject-edit-tx
  [form-eid]
  (concat (movement-tx :move/backward-up)
          (form-delete-tx (d/entity @conn form-eid))))




(defn wrap-edit-tx
  [form-eid ct value]
  (into [[:db/retract form-eid :form/editing true]
         {:db/id "newnode"
          :coll/type ct
          :coll/elements {:seq/first {:db/id "inner"
                                      :coll/_contains "newnode"
                                      :form/edit-initial (or value "")
                                      :form/editing true}}}]
        (concat
         (form-replace-tx (d/entity @conn form-eid) "newnode")
         (move-selection-tx form-eid "inner"))))

(defn editbox-keydown-tx
  [eid text key]
  (case key
    "Escape"      (reject-edit-tx eid)
    "Backspace"   (when (empty? text)
                    (reject-edit-tx eid))
    "Enter"       (if (empty? text)
                    (reject-edit-tx eid)
                    (concat (accept-edit-tx eid text)
                            (select-form-tx eid)))
    (")" "]" "}") (concat (movement-tx :move/up)
                          (if (empty? text)
                            (reject-edit-tx eid)
                            (accept-edit-tx eid text)))
    ("[" "(" "{") (wrap-edit-tx eid (case key "(" :list "[" :vec "}" :map) text)
    " "           (if (empty? text)
                    (reject-edit-tx eid)
                    (concat (accept-edit-tx eid text)
                            #_(insert-editing-tx "")))
    nil))

(rum/defcs edit-box
  < (rum/local [] ::text) (focus-ref-on-mount "the-input") editing-when-mounted
  [{::keys [text]} form-eid init]
  (let [value (if (= [] @text)
                init
                @text)]
    [:input.edit-box
     {:type        :text
      :ref         "the-input"
      :value       value
      :style       {:width (str (count value) "ch")}
      :on-change   #(reset! text (string/trim (.-value (.-target %))))
      :on-key-down (fn [ev]
                     (when-let [tx (editbox-keydown-tx form-eid value (.-key ev))]
                       (.preventDefault ev)
                       (.stopPropagation ev)
                       (d/transact! conn tx)))
      :on-blur     #(do (.preventDefault %)
                        (d/transact! conn (accept-edit-tx form-eid value)))}]))


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
   "{"         [::edit-new-wrapped :map ]
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

(rum/defc notes
  []
  [:div
   [:h3 "Getting started"]
   [:p "First, press " [:code "f"] " to 'flow' into the top-level form."]
   [:p
    "Now, you can press space to insert a new node. "
    "There are a few keybinds to do various kinds of insertion.  "
    "The idea is that you can almost type the code normally and it should do the right thing."]
   [:h3 "Notes"]
   [:p
    "This is a small demonstration of what a purely-structural editor might be like.  "]
   [:ul
    [:li "There is no whitespace or layout of any kind"
     [:ul
      [:li "The only reason that symbols aren't right next to each other is padding on their span class"]
      [:li "I am really not sure about the best way to implement this"
       [:ul
        [:li "Should the layout be persisted in the database?  If so, how?"]
        [:li "How should the rendering work?"]
        [:li "Should there even be a concept of 'lines' at all?"]]]]]
    [:li "When editing a node, there is an important difference between accepting the edit with "
     [:code "Enter"] " or " [:code "Space"] "."
     [:ul
      [:li [:code "Enter"] " exits edit mode and leaves the cursor over the inserted node"]
      [:li [:code "Space"] " inserts the node and immediately starts inserting another"]]]
    [:li "It really doesn't like it when you try to do any kind of mutation with the cursor over the top-level form."]

    [:li "An attempt was made to implement " [:code ":move/flow"] " but there is no backwards version of this"
     [:ul
      [:li "It seems not entirely clear what exactly backwards flow would do"]]]
    [:li "Right now there is only one top-level form edit widget.  Multiple form editing could be implemented in a few different ways and I am not sure which is best"]
    [:li "When you begin editing a node, it immediately exists in the database so that it can take up space and be rendered properly."
     [:ul
      [:li "The problem with this is that is forfeits the invariant that the form under structural editing is always valid"]]]
    [:li "I am not sure the performance hacks are necessary.  "
     [:ul
      [:li "The idea is that you should be able to edit a reasonably large form and hold "
       [:code "f"] " to page through it"]
      [:li "This should be limited by your key repeat speed and not the renderer performance"]
      [:li "I think for forms of appreciable size it may be too slow to do it naively"]]]
    [:li "Because I don't understand key handlers, typing doesn't really work into the import text form"
     [:ul [:li "Just paste into it instead"]]]]])

(rum/defcs text-import-form < (rum/local "" ::text)
  [{::keys [text]}]
  [:form
   {:on-submit #(do (pub! [::import-form-text @text])
                    (.preventDefault %))}
   [:input {:on-change #(reset! text (.-value (.-target %)) )
            :value @text}]
   [:input {:type "submit" :value "import clj text"}]])


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
                    :height "50vh"}}
      [:div.top-level-form
       {:style {:width "900px"
                :overflow :auto
                :resize :both
                :height "300px"
                :border "1px solid #ae81ff"}}
       (top-level-form-component (d/entity db ::state))]]
     
     #_(focus-info)
     #_(last-tx-table)
     (h/history-view)
     (text-import-form)
     #_(all-datoms-table)
     (key-bindings-table)
     #_(debug/transaction-edit-area conn)
     (notes)]))

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
  (h/clear!)
  (rum/mount (root-component) (.getElementById js/document "root"))
  (pub! [::select-form (:db/id (:state/display-form (d/entity @conn ::state)))]))

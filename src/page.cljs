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
    :form/highlight {:db/unique :db.unique/identity}}))

(def test-form-data '[nothing [a b c ] here])

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



#_(defn make-subs!
  []
  (doseq [[dispatch-val action] (methods message->mutation*)]
    (register-sub dispatch-val
                  (fn [[a b c d e]]
                    (d/transact! conn (action @conn a b c d e))))))

(d/listen! conn
           (-> (fn [{:keys [db-after tx-data tempids] :as tx-report}]
                 (reset! *last-tx-report* tx-report)
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
    
    (:form/highlight e)
    [:span.selected (form-component* e)]
    
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

(register-sub ::select-form (fn [[_ eid]] (d/transact! conn (select-form-tx @conn eid))))

#_(register-sub ::import-form-text
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
        ]
    [[:db/retractEntity (:db/id e)]
     (cond
       (and prev next) [:db/add (:db/id prev) :seq/next (:db/id next)]
       prev            [:db/retract (:db/id prev) :seq/next (:db/id spine)])]))

(defn form-replace-tx
  [e replacement-eid]
  (let [spine (first (:seq/_first e))
        coll (first (:coll/_contains e))]
    (println "FRTX" (:db/id e) spine coll)
    (when-not (or spine coll)
      (println "Datoms for " (:db/id e))
      (doseq [[e a v t a?] (d/datoms (d/entity-db e) :eavt) ]
        (when (or (= e (:db/id e))
                  (= v (:db/id e)))
          (println [e a v t a?]))))
    
    (when (and spine coll)
      [[:db/retract (:db/id coll) :coll/contains (:db/id e)]
       [:db/add (:db/id coll) :coll/contains replacement-eid]
       [:db/add (:db/id spine) :seq/first replacement-eid]])))

(defn form-raise-tx
  [e]
  (when-let [parent (some-> (:coll/_contains e) first)]
    (let [rtx (form-replace-tx parent (:db/id e))]
      (println "Raise tx" rtx)

      rtx)))

(register-sub ::raise-selected-form (fn [[_]]
                                      (let [sel (get-selected-form @conn)]
                                        (println "Raise SF" sel)
                                        (some->> (form-raise-tx sel)
                                                 (d/transact! conn)))))

(defn insert-editing-tx
  [db edit-initial]
  (let [sel (get-selected-form db)
        new-node {:db/id "newnode"
                  :form/editing true
                  :form/edit-initial (or edit-initial "")}]
    (into [new-node]
          (concat (insert-after-tx sel new-node)
                  (move-selection-tx (:db/id sel) "newnode")))))

(register-sub ::edit-new-node-after-selected
              (fn [[_ init]]
                (d/transact! conn (insert-editing-tx @conn init))))

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

(register-sub ::edit-new-wrapped
              (fn [[_ coll-type init]]
                (d/transact! conn (edit-new-wrapped-tx @conn coll-type init))))

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

(register-sub ::move
              (fn [[_ movement-type]]
                (d/transact! conn (movement-tx @conn movement-type))))

(defn move-and-delete-tx
  [db movement-type]
  (let [src (get-selected-form db)]
   (when-let [dst (move movement-type src)]
     (concat (form-delete-tx src)
             [[:db/add (:db/id dst) :form/highlight true]]))))

(register-sub ::delete-with-movement
              (fn [[_ movement-type]]
                (d/transact! conn (move-and-delete-tx @conn movement-type))))

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
         (form-replace-tx (d/entity db form-eid) "newnode")
         (move-selection-tx form-eid "inner"))))

(defn editbox-keydown-tx
  [db eid text key]
  (case key
    "Escape"      (reject-edit-tx db eid)
    "Backspace"   (when (empty? text)
                    (reject-edit-tx db eid))
    "Enter"       (if (empty? text)
                    (reject-edit-tx db eid)
                    (accept-edit-tx eid text))
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
    [:input.edit-box
     {:type        :text
      :ref         "the-input"
      :value       value
      :style       {:width (str (count value) "ch")}
      :on-change   #(reset! text (string/trim (.-value (.-target %))))
      :on-key-down (fn [ev]
                     (when-let [tx (editbox-keydown-tx @conn form-eid value (.-key ev))]
                       (.preventDefault ev)
                       (.stopPropagation ev)
                       (d/transact! conn tx)))
      :on-blur     #(do (.preventDefault %)
                        (d/transact! conn (accept-edit-tx form-eid value)))}]))


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
       [:pre (pr-str (e/->form (d/entity db 1)))]])))

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
     
     (focus-info)
     #_(last-tx-table)
     (h/history-view)
     (text-import-form)
     #_(all-datoms-table)
     (key-bindings-table)
     #_(debug/transaction-edit-area conn)
     (notes)]))

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

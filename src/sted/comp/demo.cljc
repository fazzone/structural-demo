(ns sted.comp.demo
  (:require
   
   [sted.embed :as e]
   [sted.schema :as s]
   [sted.embed.data :as sed]
   
   [datascript.core :as d]
   [sted.comp.root :as cr]
   [sted.df.async :as a]
   [sted.teleport :as t]
   [clojure.string :as string]
   [rum.core :as rum]
   [sted.sys.keyboard :as sk]
   [sted.sys.handle :as sh]
   [sted.sys.eval.sci :as eval-sci]
   [sted.comp.keyboard :as kc]
   
   [sted.cmd.mut :as mut]
   [sted.db-reactive :as dbrx]
   [sted.comp.common :as cc]
   [sted.comp.cons :as ccons]
   [sted.comp.debug :as cd]
   [sted.core :as core
    :refer [get-selected-form
            move-selection-tx]]))

(defn setup-app
  [conn]
  (let [a (core/app conn)
        aa (atom nil)]
    
    (println "Setup demo app")
    
    (doseq [[m f] mut/movement-commands]
      (core/register-simple! a m (core/movement->mutation f)))
    (doseq [[m f] mut/editing-commands]
      (core/register-simple! a m f))
    
    (reset! aa
            (-> a
                (sh/setup!)
                (core/register-mutation! :eval-sci (eval-sci/mutatef a aa))
                #_(core/register-mutation! :scroll (fn [_ _ _] (csc/scroll-to-selected!)))
                (core/register-mutation!
                 :sync
                 (fn [[_ r]]
                   (r :sync)))))))




(defn run-steps
  [{:keys [form setup mutations]}]
  (let [conn (t/create-conn form)
        
        mut-map (merge mut/editing-commands
                       (zipmap (keys mut/movement-commands)
                               (map core/movement->mutation
                                    (vals mut/movement-commands))))
        
        dbs (reductions
             (fn [{:keys [db-after]} [mut-name & args :as mut]]
               (let [sel    (core/get-selected-form db-after)
                     mut-fn (get mut-map mut-name)
                     report (d/with db-after (apply mut-fn sel args))]
                 (assoc report :teleport/mutation mut)))
             {:db-after @conn}
             (concat setup mutations))]
    dbs))




(rum/defc demo
  [steps rec classes]
  (let [dbs (run-steps steps)

        eid (:db/id (core/get-selected-form (:db-after (first dbs))))]
    
    [:div {:class (str "alternate-reality " classes)}
     [:div {:style {:display "grid"
                    :grid-template-columns "4ex min-content min-content 1fr 1fr"
                    :align-items "center"
                    :gap "1ex"}}
      (for [i (range (count dbs))
            :let [{:keys [db-after tx-data] :teleport/keys [mutation]} (nth dbs i)]
            w [:index :mut :app :cons :txd]]
        (case w
          :index [:span {:key (str w i)}
                  (str "#" i " ")]
          :mut [:span {:key (str w i)}
                (pr-str mutation)
                #_(let [im (- i (count setup))]
                    (str "Im" im " of " (count mutations))
                    (if (zero? im)
                      "(init)"
                    
                      (str (nth mutations (dec im)))))]
          :txd [:div {:key (str w i)}
                (cd/tx-data tx-data)]
          :app [:div {:key (str w i)}
                (rec (d/entity db-after eid)
                     core/blackhole
                     0 nil)]
          :cons [:div {:key (str w i)}
                 (ccons/testing
                     (d/entity db-after eid)
                     core/blackhole
                     (fn [n] (rec n core/blackhole 0 nil)))]))]]))

(rum/defc render-steps*
  [steps]
  
  )

(rum/defc player*
  [classes rec children]
  (let [[{:keys [index playing] :as s} set-state!] (rum/use-state
                                                    {:playing false
                                                     :index (dec (count children))})
        sref (rum/create-ref)]
    
    (rum/use-effect!
     (fn []
       (when playing
         (let [tid (js/setTimeout #(set-state! (assoc s :index (mod (inc index) (count children))))
                                  100)]
           #(js/clearTimeout tid))))
     [s])

    
    [:div {:style { :border "1px solid aliceblue"
                   :border-radius "2ex"
                   :padding "1ex"
                   :margin "1ex"
                   :width "90vw"
                   :color "#fff"}}
     [:div {:class (str "alternate-reality " classes)
            :style {:display "grid"
                    :grid-template-columns "min-content 1fr 1ex"
                    :gap "1ex"
                    :grid-template-rows "2ex max-content"
                    :align-items "start"}}
      [:span {:style {:justify-self "center"}} "Mutation"]
      [:span {:style {:justify-self "center"}} "After"]
      [:span {:style {:justify-self "center"}} "tx-data"]
       
      [:div {:style {:display "grid"
                     :grid-template-columns "1fr"
                     :align-items "center"
                     :justify-items "end"
                     :max-height "80vh"
                     :overflow "auto"}}
       (for [i (range (count children))
             :let [{:teleport/keys [mutation]} (nth children i)]
             w [:mut-label]]
         (case w
           :mut-label [:div {:key (str w i)
                             ;; :class (when (= i index) "player-active-row")
                             :style {:border-radius "1.5ex"
                                     :padding "1ex 2ex 1ex 2ex"
                                     :background-color (if (= i index)
                                                         "#272727"
                                                         "unset"
                                                         )}
                             :on-click  #(set-state! (assoc s :index i))}
                       (or (some-> mutation pr-str)
                           "(init)")]))]
      
      [:div
       (:comp (nth children index))
       
       [:div {:style {:height "80vh"}}
        (:ccons (nth children index))]]

      [:div {:style {:justify-self "end" :overflow :auto}}
       nil
       #_(cd/tx-data (:tx-data (nth children index)))]
       
      [:div {:style {:grid-column "1 / span 3"
                     :display "grid"
                     :gap "1ex"
                     :grid-template-columns "min-content min-content 1fr"}}
       [:span {:role "button" :on-click #(set-state! (update s :playing not))
               :style {:font-size "200%"}}
        (if playing "\u23f8" "\u25b6")]
       
       [:span {:style {:place-self "center end"}}
        (str index "/" (dec (count children)))]
       
       [:input {:type "range"
                :ref sref
                :min 0
                :max (dec (count children))
                :value index
                :on-change (fn [^js ev]
                             (set-state! (assoc s :index (js/parseInt (.-value (.-target ev))) )))}]]]
     
     #_[:div {:style {:margin-top "1ex"}}
        [:span {:role "button" :on-click #(set-state! (update s :playing not))
                :style {:font-size "200%"}}
         (if playing
           "\u23f8"
           "\u25b6")]]]))

(rum/defc player
  [steps rec classes]
  (player*
   classes rec
   (do
     (println "Rendering the components")
     (vec
      (for [{:keys [db-after] :as s} (run-steps steps)]
        (assoc s
               :comp (rec (d/entity db-after 2)
                          core/blackhole
                          0 nil)
               :ccons (ccons/testing
                          (d/entity db-after 2)
                        core/blackhole
                        (fn [n] (rec n core/blackhole 0 nil)))))))))

(def population
  [[:flow-right]
   [:flow-right]
   [:flow-left]
   [:flow-left]
   [:float] [:float] [:float]
   [:sink] [:sink]
   [:wrap] [:wrap] [:wrap]
   [:delete-left] [:delete-left] [:delete-right] [:delete-right]
   [:clone] [:clone]
   [:split]
   
   ;; [:split] [:split]
   ;; [:splice] [:splice]
   [:barf-right]
   [:slurp-right] [:slurp-right] [:slurp-right]
   [:raise] [:raise]])



(def my-mutations
  (quote
   [[:hop-left]
    [:hop-right]
    [:hop-left]
    [:flow-right]
    [:insert-right]
    [:edit/finish-and-edit-next-node "for"]
    [:edit/wrap :vec "i"]
    [:edit/finish-and-edit-next-node "i"]
    [:edit/wrap :list "range"]
    [:edit/finish-and-edit-next-node "range"]
    [:edit/finish "9"]
    [:parent]
    [:clone]
    [:clone]
    [:next]
    [:insert-left]
    [:edit/finish "j"]
    [:next]
    [:next]
    [:insert-left]
    [:edit/finish "k"]
    [:next]
    [:insert-right]
    [:edit/finish-and-edit-next-node ":when"]
    [:edit/wrap :list ""]
    [:edit/finish-and-edit-next-node "="]
    [:edit/wrap :list ""]
    [:edit/finish-and-edit-next-node "*"]
    [:edit/finish-and-edit-next-node "k"]
    [:edit/finish "k"]
    [:parent]
    [:insert-right]
    [:edit/wrap :list "+"]
    [:edit/finish-and-edit-next-node "+"]
    [:edit/finish-and-edit-next-node "j"]
    [:edit/finish "j"]
    [:parent]
    [:clone]
    [:next]
    [:tail]
    #_[:zp]
    [:linebreak]
    #_[:zp]
    [:move-to-deleted-chain]
    [:offer]
    [:edit/finish "i"]
    [:clone]
    [:parent]
    [:parent]
    [:parent]
    [:insert-right]
    [:edit/wrap :vec ""]
    [:edit/finish-and-edit-next-node "i"]
    [:edit/finish-and-edit-next-node "j"]
    [:edit/finish "k"]
    [:parent]
    [:linebreak]
    #_[:zp]
    [:eval-result 88 ([0 0 0] [2 2 2] [8 8 4])]
    [:m1]
    [:compose]
    [:edit/finish-and-edit-next-node "let"]
    [:edit/wrap :vec ""]
    [:edit/finish-and-edit-next-node "n"]
    [:edit/finish "9"]
    [:parent]
    [:next]
    [:linebreak]
    #_[:zp]
    [:m2]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:offer]
    [:edit/finish "n"]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:offer]
    [:edit/finish "n"]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:offer]
    [:edit/finish "n"]
    [:m1]
    [:m2]
    [:tail]
    [:offer]
    [:edit/finish "12"]
    [:m1]
    [:tail]
    [:compose]
    [:edit/finish-and-edit-next-node "min"]
    [:edit/finish "j"]
    [:parent]
    [:flow-left]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:offer]
    [:edit/finish "i"]
    [:parent]
    [:clone]
    [:flow-right]
    [:offer]
    [:edit/finish "max"]
    [:flow-left]
    [:flow-left]
    [:delete-right]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-right]
    [:insert-right]
    [:edit/finish "1"]
    [:float]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-right]
    [:insert-left]
    [:edit/finish "1"]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:insert-left]
    [:edit/finish "1"]
    [:m1]
    [:eval-result 176 ([2 2 2] [8 8 8])]
    [:tail]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-right]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:offer]
    [:edit/finish "*"]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-left]
    [:offer]
    [:edit/finish "*"]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:compose]
    [:edit/finish-and-move-up "+"]
    [:slurp-right]
    [:linebreak]
    #_[:zp]
    [:flow-right]
    [:flow-right]
    [:linebreak]
    #_[:zp]
    [:eval-result 176 ([3 4 3] [4 4 3] [6 8 6] [8 8 6])]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:offer]
    [:edit/finish "k"]
    [:eval-result 176 ([5 4 3] [5 4 3] [10 8 6] [10 8 6])]
    [:sink]
    [:sink]
    [:eval-result 176 ([4 3 5] [4 3 5] [8 6 10] [8 6 10])]
    [:flow-left]
    [:flow-left]
    [:parent]
    [:float]
    [:eval-result 176 ([3 4 5] [3 4 5] [6 8 10] [6 8 10])]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-right]
    [:flow-right]
    [:offer]
    [:edit/finish "k"]
    [:offer]
    [:edit/finish "j"]
    [:eval-result 176 ()]
    [:eval-result 176 ()]
    [:offer]
    [:edit/finish "n"]
    [:eval-result 176 ([3 4 5] [3 4 5] [6 8 10] [6 8 10])]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:compose]
    [:edit/finish-and-edit-next-node "and"]
    [:edit/wrap :list ""]
    [:edit/finish-and-edit-next-node "<"]
    [:edit/finish-and-edit-next-node "j"]
    [:edit/finish "k"]
    #_[:zp]
    [:eval-result 176 ([3 4 5] [3 4 5] [6 8 10] [6 8 10])]
    [:float]
    [:clone]
    [:delete-right]
    [:eval-result 176 ()]
    [:sink]
    [:eval-result 176 ([3 4 5] [3 4 5] [6 8 10] [6 8 10])]
    [:float]
    [:eval-result 176 ()]
    [:sink]
    [:eval-result 176 ([3 4 5] [3 4 5] [6 8 10] [6 8 10])]
    [:offer]
    [:edit/finish "i"]
    [:eval-result 176 ([3 4 5] [6 8 10])]
    [:float]
    [:eval-result 176 ([3 4 5] [6 8 10])]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:offer]
    [:edit/finish "i"]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:eval-result 176 ()]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-left]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:flow-right]
    [:uneval]
    [:eval-result 176 ([3 4 5] [6 8 10])]
    [:flow-left]
    [:flow-right]
    [:raise]
    #_[:zp]
    [:linebreak]
    #_[:zp]
    [:m1]
    [:eval-result 176 ([3 4 5] [6 8 10])] 
    [:flow-right] 
    [:flow-left] ])

  
  #_(repeatedly 16 #(rand-nth population))) 

#_(do (println "My-Mutations")
    (prn my-mutations))

(rum/defc mutation-card
  [steps rec classes]
  (let [dbs (run-steps steps)

        eid (some-> dbs first
                    :db-after
                    (d/entity :sted.page/state)
                    :state/bar
                    :seq/first
                    :seq/first
                    :db/id)]

    
    [:div {:class (str "alternate-reality " classes)
           :style {:display :grid
                   :width "min-content"
                   :grid-template-columns "min-content 8ex min-content"}}
     [:span
      (rec (d/entity (:db-after (first dbs)) eid)
           core/blackhole
           0 nil)]
     
     [:span {:style {:justify-self "center"}} "=>"]
     
     [:span
      (rec (d/entity (:db-after (last dbs)) eid)
           core/blackhole
           0 nil)]]))

(rum/defc mcn
  [rec classes state-map left right {:keys [view style label] :as opts}]
  (let [view (or (some-> (get state-map view)
                         :db-after
                         core/get-selected-form)
                 (some-> (get state-map left)
                         :db-after
                         (d/entity :sted.page/state)
                         :state/bar
                         :seq/first
                         :seq/first))
        
        eid         (:db/id view)
        before-left (:db-before (state-map left))
        after-left  (:db-after (state-map left))
        after-right (when right
                      (:db-after (state-map right)))]
    
    [:div {:class (str "alternate-reality " classes)
           :style (-> {:display               :grid
                       ;; :width "min-content"
                       :grid-template-columns "1fr 8ex 1fr"
                       :grid-template-rows "1fr 4ex calc(1fr - 3ex)"
                       :justify-items         "center"}
                      (merge style))}
     
     [:span {:style {:grid-area "1 / 1 / 4"}}
      (rec (d/entity (if-not right before-left after-left) eid)
           core/blackhole 0 nil)]
     [:span {:style {:grid-area "2 / 2 / 2"
                     :text-align "center"}}
      "->"
      (when label [:div {} (str label)])]
     [:span {:style {:grid-area "1 / 3 / 4"}}
      (rec (d/entity (if right after-right after-left) eid)
           core/blackhole 0 nil)]]))

(rum/defc teleport-doc
  [rec state-map {:keys [view style label body] :as opts}]
  (let [view-ent (some-> (get state-map view)
                         :db-before
                         core/get-selected-form)
        eid         (:db/id view-ent)]
    (println "View????" view (some? (get state-map view)) (keys state-map))
    [:div {:class "alternate-reality"
           :style (-> {:display               :grid
                       :grid-template-columns "1fr 1fr 3ex 1fr"}
                      (merge style))}
     
     (for [[label sk] body
           k [:lbl :db-before :arrow :db-after ]]
       (case k
         :lbl [:span {:key (str k sk )} label]
         :arrow [:span {:key (str k sk )
                        :style {:justify-self "center"}}
                 "->"]
         
         (:db-before :db-after)
         [:div {:key (str k sk)
                :style {:justify-self "center"}}
          (rec (d/entity (-> state-map (get sk) (get k)) eid)
               core/blackhole 0 nil)]))]))

(defn send!
  [bus msg]
  (js/Promise.
   (fn [resolve reject]
     (core/send! bus msg)
     (core/send! bus [:sync resolve]))))

(defn get-snapshots!
  [app muts]
  (reduce
   (fn [p m]
     (a/let [ss p
             _ (send! (:bus app) m)
             snap (deref (:conn app))]
       (conj ss snap)))
   []
   muts))

(rum/defc real-root
  [rec muts]
  (let [[app set-app!] (rum/use-state (setup-app (t/create-conn [[:a :b :c :d :e]])))
        [snapshot set-snapshot!] (rum/use-state {})]
    
    (rum/use-effect!
     (fn []
       (println "Snapping")
       (a/let [ss (get-snapshots! app muts)]
         (set-snapshot! "Bonkly"))
       (fn cleanup []))
     [app muts])
    
    [:div.alternate-reality 
     [:div {:style {;; :width "10em"
                    ;; :height "15ch"
                    :margin "4ex"
                    }}
      (cr/root app rec)]
     
     [:div.controls {}
      [:input {:type "button"
               :value "Send many"
               :on-click (fn []
                           (core/send! (:bus app)  [:flow-right])
                           (println "SameSel?" (-> app :conn deref core/get-selected-form :db/id))
                           
                           #_(a/let [bink (promise-send! (:bus app) [:flow-right])]
                               (println "MutatedSel"
                                        (-> app :conn deref core/get-selected-form :db/id)))
                           
                           #_(core/send! (:bus app)  [:flow-right])
                           #_(core/send! (:bus app)  [:sync]))}]]

     #_[:ul.muts {}
      (for [i (range (count muts))]
        [:li {:key i} (pr-str (nth muts i))])]
     
     [:div.snaps {}
      (str "Keys" (pr-str (type (deref (:history app)))))
      (let [hs (doto (into-array (deref (:history app)))
                 (.reverse))]
        (for [i (range (count hs))
              :let [{:keys [mut db-after]} (nth hs i)]]
          [:div {:key i}
           (str i " " (pr-str mut))
           (cr/root {:conn (atom db-after)
                     :bus core/blackhole}
                    rec
                    )]))]]))



;; Attributes
;; M - moves the selection
;; F - moves further than O(1) links away
;; N - creates new nodes
;; D - deletes nodes
;; E - enters token-editing state
;; 

(rum/defc demo*
  [r c]

  #_[:div.mememe
     (real-root r
                #_[[:flow-right]
                   [:flow-right]
                   [:flow-right]
                   [:new-list]
                   [:edit/finish-and-edit-next-node "+"]
                   [:edit/finish-and-edit-next-node "1"]
                   [:edit/finish "1"]
                   [:eval-sci]]
                [[:flow-right]
                 [:linebreak]
                 [:clone]
                 [:clone]
                 [:clone]
                 [:clone]
                 [:clone]])]
  

  #_(rum/bind-context
   [cc/*modeline-ref* nil]
   [:div {:style {:display :flex
                  :flex-direction :column}}
      
    (demo {:form '((a [1] c))
           :setup [[:flow-right] [:flow-right] [:flow-right]]
           :mutations [[:split]]}
          r c)

    #_(for [j (range 1)]
        [:div {:key j}
         [:h2 (str j)]
         (player {:form (quote (fn [n] (r 0 g)))
                  :setup [[:flow-right] [:flow-right] [:flow-right] [:flow-right] [:flow-right]]
                  :mutations my-mutations}
                 r c)])])
  
  ;; flow-right
  (let [t-flow (t/build t/flow-left-right)
        t-tail (t/run-steps-next
                '(a (b) (c (d e) f))
                [{:tag :tail :do [:tail]}
                 {:tag :parent :do [:parent]}

                 [:tail]
                 {:tag :tl-b :do [:toplevel]}])
        
        t-float (t/run-steps-next
                 '(c (d e) f)
                 [[:tail]
                  {:tag :float :do [:float]}
                  {:tag :sink :do [:sink]}])
        
        t-del (t/run-steps-next
               '(a L x R b)
               [[:flow-right] [:flow-right] [:flow-right]
                {:tag :dla :do [:delete-left]}])
        t-delr (t/run-steps-next
                '(a L x R b)
                [[:flow-right] [:flow-right] [:flow-right]
                 {:tag :dr :do [:delete-right]}])
        
        t-raise (t/run-steps-next
                 '(f (if a x y))
                 [[:flow-right] [:flow-right] [:flow-right] [:flow-right] 
                  {:tag :bonk :do [:raise]}])

        t-comp (t/run-steps-next
                '(f x)
                [[:flow-right]
                 [:flow-right]
                 {:tag :create-partial :do [:compose]}
                 {:tag :finish :do [:edit/finish-and-move-up "g"]}])
        
        t-tear (t/run-steps-next ["/usr/bin/env"]
                                 [[:flow-right]
                                  [:flow-right]
                                  {:tag :tear :do [:tear]} 
                                  {:tag :stitch :do [:tear]}])
        t-offer (t/run-steps-next '(f val)
                                  [[:tail]
                                   {:tag :offer-tk :do [:offer]}
                                   [:edit/finish "x"]
                                   [:parent]
                                   {:tag :offer-coll :do [:offer]}
                                   [:edit/finish "g"]])
        
        t-insl (t/run-steps-next '(x y z)
                                 [[:flow-right]
                                  [:flow-right]
                                  {:tag :insl :do [:insert-left]}])

        t-insr (t/run-steps-next '(x y z)
                                 [[:flow-right]
                                  [:flow-right]
                                  {:tag :insr :do [:insert-right] }])
        t-wrap (t/run-steps-next '(f x y)
                                 [[:flow-right]
                                  [:flow-right]
                                  {:tag :wrap :do [:wrap] }])
        t-newlist (t/run-steps-next '(f x y)
                                    [[:flow-right]
                                     [:flow-right]
                                     {:tag :newlist :do [:new-list]}
                                     [:edit/finish "g"]
                                     {:tag :newvec :do [:new-vec]}])
        t-split (t/run-steps-next '(f [1 2 3 4])
                                  [[:flow-right] [:flow-right] [:flow-right] [:flow-right]
                                   {:tag :split :do [:split]}])

        t-splice (t/run-steps-next '(f [1 2 3 4])
                                   [[:flow-right] [:flow-right] [:flow-right] [:flow-right]
                                    {:tag :splice :do [:splice]}])
        
        t-slurp (t/run-steps-next '(f (g a) b c)
                                  [[:flow-right] [:flow-right] [:flow-right] [:flow-right]
                                   {:tag :slurp-tk :do [:slurp-right]}
                                   [:parent]
                                   {:tag :slurp-coll :do [:slurp-right]}
                                   {:tag :barf-coll :do [:barf-right]}
                                   [:tail]
                                   {:tag :barf-tk :do [:barf-right]}])
        
        t-chain #_(t/run-steps-next '[(ns example) (def x 1)]
                                    [[:flow-right]
                                     [:splice]
                                     [:select-chain]
                                     {:tag :bar :do [:parent]}
                                     [:tail]
                                     {:tag :bink :do [:drag-left]}])
        (t/run-steps-next '[(ns left)
                            (def x 1)
                            (ns right)
                            (def y 2)]
                          [[:flow-right]
                           [:splice]
                           [:select-chain]
                           {:tag :bar :do [:parent]}
                           [:flow-right]
                           [:flow-right]
                           [:drag-left]
                           [:select-chain]
                           [:next]
                           [:flow-right]
                           [:drag-left]
                           [:sink]
                           [:select-chain]
                           {:tag :next-chain :do [:next]}
                           {:tag :select-bar :do [:parent]}
                           [:flow-right]
                           {:tag :sink-chain :do [:sink]}
                           [:float]
                           {:tag :flow-into :do [:flow-right]}
                           {:tag :parent-nop :do [:parent]}
                           [:next]
                           [:tail]
                           [:prev]
                           {:tag :select-chain :do [:select-chain]}
                           {:tag :select-chain-memory :do [:select-chain]}
                         
                           {:tag :hop-right :do [:hop-right] }
                           [:next]
                           {:tag :hop-left :do [:hop-left]}
                         
                           [:parent]
                           {:tag :drag-right :do [:drag-right]}
                           {:tag :drag-right-new :do [:drag-right]}
                         
                         
                           ])]
    (rum/bind-context
     [cc/*modeline-ref* nil]
     [:div {:style {:display :flex
                    :flex-direction :column
                    :width "55em"}}
      
      [:div.mutdoc {:style {:width "30em"}}
       [:div.form-title.mutdoc-title [:span "==>"] [:span "flow-right"]]
       [:div {:style {:margin "0 0 1ex 0"}}
        "Traverses structure"]
       (teleport-doc r t-flow
                     {:view :start
                      :body [["On tokens, select next form" :like-next]
                             ["On lists, step in one level" :into-colls]
                             ["At the end, step out" :outof-colls]
                             ["Steps out of deep nesting" :outof-many]]})
       
       [:div.form-title.mutdoc-title [:span "<=="] [:span "flow-left"]]
       [:div {:style {:margin "0 0 1ex 0"}}
        "Inverse of " [:code "flow-right"]]
       (teleport-doc r t-flow
                     {:view :start
                      :body [["Steps in to deep nesting" :left-into-many]
                             ["Steps out one level" :left-outof-one]]})]
      
      [:div.mutdoc {:style {:width "30em"}}
       [:p.form-title "Movement"]
       [:div.form-title.mutdoc-title
        [:span "==>"]
        [:span "flow-right"]]
       [:div {:style {:margin-left "2em"}}
        [:div "Like next"]
        (mcn r c t-flow :like-next)
       
        [:div "But it goes into collections"]
        (mcn r c t-flow :into-colls)
       
        [:div "And flows out"]
        (mcn r c t-flow :outof-colls)
       
        [:div "Out of many levels"]
        (mcn r c t-flow :outof-many)]

       [:div.form-title.mutdoc-title
        [:span "<=="]
        [:span "flow-left"]]

       [:div "Into many levels"]
       (mcn r c t-flow :left-into-many)
       
       [:div "Out one level at a time"]
       (mcn r c t-flow :left-outof-one)
       

       [:div.form-title.mutdoc-title
        [:span {:style {:transform "rotate(45deg)"}} "==>"]
        [:span "tail"]]
       
       [:div "Selects the last and most-nested node"]
       (mcn r c t-tail :tail)
      
       [:div.form-title.mutdoc-title
        [:span {:style {:transform "rotate(45deg)"}} "<--"]
        [:span "parent"]]
       
       [:div "One level up"]
       (mcn r c t-tail :parent)

       [:div.form-title.mutdoc-title
        [:span {:style {:transform "rotate(45deg)"}} "<=="]
        [:span "toplevel"]]
       
       [:div "Straight to the top"]
       (mcn r c t-tail :tl-b)]
      
      
      [:div.form-title.mutdoc-title
       [:span {} ")->"]
       [:span "slurp-right"]]
      
      [:div "on collections, add next to end"]
      (mcn r c t-slurp :slurp-coll)
      
      [:div "on tokens, apply to parent"]
      (mcn r c t-slurp :slurp-tk)

      [:div.mutdoc
       [:div.form-title.mutdoc-title
        [:span {} "<-)"]
        [:span "barf-right"]]
       
       [:div "on collections, move last element to next"]
       (mcn r c t-slurp :barf-coll)
       
       [:div "on tokens, apply to parent"]
       (mcn r c t-slurp :barf-tk)]

      [:div.mutdoc
       [:div.form-title.mutdoc-title
        [:span {} " "]
        [:span "splice"]]
       
       [:div "remove nesting"]
       (mcn r c t-splice :splice)]

      [:div.mutdoc
       [:div.form-title.mutdoc-title
        [:span {} " "]
        [:span "split"]]
       
       [:div "new list with rest"]
       (mcn r c t-split :split)]

      [:div.mutdoc
       [:div.form-title.mutdoc-title
        [:span {} "+()"]
        [:span "new-list"]]
       
       [:div "create new list after selected"]
       (mcn r c t-newlist :newlist)
       
       [:div "also new-vec"]
       (mcn r c t-newlist :newvec)]

      [:div.mutdoc
       [:div.form-title.mutdoc-title
        [:span {} "(+)"]
        [:span "wrap"]]
       
       [:div "create new list around selected"]
       (mcn r c t-wrap :wrap)]

      [:div.mutdoc
       [:div.form-title.mutdoc-title
        [:span {} ""]
        [:span "insert-left"]]
       
       [:div "add token before selected"]
       (mcn r c t-insl :insl)]
      
      
      
      
      
      [:div.mutdoc
       [:div.form-title.mutdoc-title
        [:span {} ""]
        [:span "insert-right"]]
       
       [:div "add token after selected"]
       (mcn r c t-insr :insr)]

      [:div.mutdoc
       [:div.form-title.mutdoc-title
        [:span {} ""]
        [:span "offer"]]
       
       [:div "on tokens, overwrite"]
       (mcn r c t-offer :offer-tk)

       [:div "on collections, add to end"]
       (mcn r c t-offer :offer-coll)]

      [:div.mutdoc
       [:div.form-title.mutdoc-title
        [:span {} "\u00ab\u22ef\u00bb"]
        [:span "tear/stitch"]]
       
       [:div "split the selected token"]
       (mcn r c t-tear :tear)

       [:div "or put it back together"]
       (mcn r c t-tear :stitch)]
      
      [:div.mutdoc
       [:div.form-title.mutdoc-title
        [:span {} "\u25ef"]
        [:span "compose"]]
       
       [:div "Wrap selected, then edit first of created list"]
       (mcn r c t-comp :create-partial)
       
       [:div "When the edit is completed, the parent is selected"]
       (mcn r c t-comp :finish)]

      [:div.mutdoc
       [:div.form-title.mutdoc-title
        [:span  {:style {:transform "rotate(45deg)"}} "<<-"]
        [:span "raise"]]
       
       [:div "replace parent with selected"]
       (mcn r c t-raise :bonk)]
      
      [:div.mutdoc
       [:div.form-title.mutdoc-title
        [:span  {:style {:transform "scale(-1,1)"}} "\u2326"]
        [:span "delete-left"]]
       
       [:div "Delete selected, select previous"]
       (mcn r c t-del :dla)]

      [:div.mutdoc
       [:div.form-title.mutdoc-title
        [:span "\u2326"]
        [:span "delete-right"]]
       
       [:div "Delete selected, select next"]
       (mcn r c t-delr :dr)]
      
      
      [:div.mutdoc
       [:div.form-title.mutdoc-title
        [:span {:style {:transform "rotate(-90deg)"}}  "->>"]
        [:span "float"]]
       
       [:div "Swap with previous"]
       (mcn r c t-float :float)]
      
      [:div.mutdoc
       [:div.form-title.mutdoc-title
        [:span {:style {:transform "rotate(90deg)"}}  "->>"]
        [:span "sink"]]
       
       [:div "Swap with next"]
       (mcn r c t-float :sink)]
      
      

      [:div.mutdoc
       [:div.form-title
        "Bar & chain"]
       [:p "The root of the display is a pair of special forms."]
       [:p "The 'chain' displays forms vertically, within a horizontal 'bar'."]
       (mcn r c t-chain :select-bar nil
            {:view :bar
             :label "parent" 
             :style {:height "12ex" :margin-bottom "-3ex"}})
       [:p "Most commands treat the bar and chains normally."]
       [:p "For example, swap the display order with " [:code "sink"] ":"]
       (mcn r c t-chain :sink-chain nil
            {:view :bar
             :label "sink" 
             :style {:height "12ex" :margin-bottom "-3ex"}})
       [:p "Flow into the chain to edit the contents:"]
       (mcn r c t-chain :flow-into nil
            {:view :bar
             :label "flow-right"
             :style {:height "12ex" :margin-bottom "-3ex"}})
       
       [:p "For convenience, the bar and chains are handled specially by some commands."]
       [:p [:code "parent"] " does not move to chains, even though it is in fact the parent:"]
       (mcn r c t-chain :parent-nop nil
            {:view :bar
             :label "parent"
             :style {:height "12ex" :margin-bottom "-3ex"}})
       [:p "Instead, use " [:code "select-chain"] " to get there from anywhere:"]
       (mcn r c t-chain :select-chain nil
            {:view :bar
             :label "select-chain"
             :style {:height "12ex" :margin-bottom "-3ex"}})
       [:p "When the chain is already selected, " [:code "select-chain"] " tries to take you back:"]
       (mcn r c t-chain :select-chain-memory nil
            {:view :bar
             :label "select-chain"
             :style {:height "12ex" :margin-bottom "-3ex"}})

       [:p "Move between chains with hop-left and hop-right:"]
       (mcn r c t-chain :hop-right nil
            {:view :bar
             :label "hop-right"
             :style {:height "12ex" :margin-bottom "-3ex"}})
       [:p "These commands remember your selection too:"]
       (mcn r c t-chain :hop-left nil
            {:view :bar
             :label "hop-left"
             :style {:height "12ex" :margin-bottom "-3ex"}})
       

       [:p "Move forms between chains with drag-left and drag-right:"]
       (mcn r c t-chain :drag-right nil
            {:view :bar
             :label "drag-right"
             :style {:height "12ex" :margin-bottom "-3ex"}})
       [:p "Dragged forms are inserted before the destination chain's memory position."]
       
       [:p "Dragging past the end creates a new chain:"]
       (mcn r c t-chain :drag-right-new nil
            {:view :bar
             :label "drag-right"
             :style {:height "12ex" :margin-bottom "-3ex"}})]])))

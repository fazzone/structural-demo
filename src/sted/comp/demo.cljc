(ns sted.comp.demo
  (:require
   
   [sted.embed :as e]
   [sted.schema :as s]
   [sted.sys.search.setup :as search]
   [sted.embed.data :as sed]
   
   [datascript.core :as d]
   [sted.comp.root :as cr]
   [sted.df.async :as a]
   [sted.teleport :as t]
   [clojure.string :as string]
   [sted.comp.modeline :as ml]
   [rum.core :as rum]
   [sted.sys.keyboard :as sk]
   [sted.sys.handle :as sh]
   [sted.sys.eval.sci :as eval-sci]
   [sted.comp.keyboard :as kc]
   [sted.comp.doc :as cdoc]
   
   [sted.cmd.mut :as mut]
   [sted.db-reactive :as dbrx]
   [sted.comp.common :as cc]
   [sted.comp.cons :as ccons]
   [sted.comp.debug :as cd]
   [shadow.resource :as rc]
   [sted.core :as core
    :refer [get-selected-form
            move-selection-tx]]))


(defn setup-app
  [conn]
  (let [a (core/app conn)
        aa (atom nil)]
    
    (println "Setup new demo app")
    
    (doseq [[m f] mut/movement-commands]
      (core/register-simple! a m (core/movement->mutation f)))
    (doseq [[m f] mut/editing-commands]
      (core/register-simple! a m f))
    
    (reset! aa
            (-> a
                (sh/setup!)
                (core/register-mutation! :eval-sci (eval-sci/mutatef a aa))
                #_(core/register-mutation! :scroll (fn [_ _ _] (csc/scroll-to-selected!)))
                (core/register-mutation! :sync (fn [[_ r]] (r :sync)))
                (core/register-mutation! :update-bar-ref
                                         (fn [[_ r] _ _]
                                           (js/console.log "Update bar ref!" r)
                                           ))
                (search/setup!)))))

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

(def search-test-form
  (e/->tx (quote (prefix suffix conj matching testing connect)))
  #_(e/string->tx-all (rc/inline "sted/test/example.cljc")))

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
             _ (println "Perform" m  "{{{ ")
             _ (send! (:bus app) m)
             snap app]
       (println "}}} Performed" m)
       (conj ss snap)))
   []
   muts))


(rum/defc real-root
  [rec init-tx muts]
  (let [[app set-app!] (rum/use-state nil)
        [st set-st!] (rum/use-state 1)
        [rs set-rs!] (rum/use-state nil)]
    
    (rum/use-effect!
     (fn []
       (if (nil? app)
         (set-app!
          (setup-app (t/create-conn*  init-tx)))
         (do
           #_(set-st! 2)
           (a/let [ss (get-snapshots! app muts)]
             (set-rs! 1)
             (swap! ml/kick inc)
             #_(doseq [s ss]
                 (prn "Snap" (deref (-> s :system :search :results)))))))
       nil)
     [app muts])

    [:div.alternate-reality
     [:div {:style {:margin "2ex"
                    :width "32em"}}
      (when app (cr/root app rec))]
     
     #_[:div.controls {}
        [:input {:type "button"
                 :value "Send flow"
                 :on-click (fn [] (core/send! (:bus app) [:flow-right]))}]]

     
     
     
     ;; shows undo history....  not the same
     #_[:div.snaps {}
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

  #_(cdoc/mutations-reference r nil)
  
  
  [:div.mememe
   (real-root r
              (e/->tx (quote (prefix suffix conj matching testing connect)))
              [[:flow-right]
               [:update-search "co"]
               [:update-search "con"]])
   
   (real-root r
              (e/->tx (quote (prefix suffix conj matching testing connect)))
              [[:tail]
               [:update-search "fix"]
               
               #_[:update-search "conj"]])

   (real-root r
              (e/->tx (quote ("Very long strings with search results"
                              a b :string c d)))
              [[:tail]
               [:update-search "ring"]
               
               #_[:update-search "conj"]])]
  

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
  )



















(ns sted.sys.search.setup
  (:require
   [rum.core :as rum]
   [sted.core :as core]
   [sted.cmd.nav :as nav]
   [sted.sys.search.db :as sdb]
   [datascript.core :as d]
   [sted.cmd.mut :as mut]
   [datascript.db :as db #?@(:cljs [:refer [Datom]])]
   #?(:cljs [sted.sys.search.dom :as sdom])
   ["flexsearch" :as fs])
  #?(:clj (:import [datascript.db Datom])))

(defn cleanup!
  [app]
  (dissoc app ::results))

(defn initial-index
  [db]
  (reduce (fn [acc ^Datom d]
            (doto acc (.add (.-e d) (.-v d))))
          (fs/Index. #js {:tokenize "forward"})
          (d/datoms db :avet :token/value)))

#_(defn get-state
  [db]
  (some-> db (d/entity :sted.page/state) ::state))

(defn sfs
  [db index text limit]
  (let [srs (.search index text)
        tokens (into (sorted-set)
                     (for [eid srs]
                       (-> (d/datoms db :eavt eid :token/value)
                           (nth 0)
                           (nth 2))))
        results (for [tk tokens]
                  (let [tkds (d/datoms db :avet :token/value tk)
                        [[le _ _ lt]] (sort-by #(- (nth % 3)) tkds)]
                    #js {:token tk
                         :n (count tkds)
                         :eid le
                         :last_tx lt}))]
    
    (->> results
         (sort-by (juxt (comp - #(.-last_tx ^js %))
                        #(.-n ^js %)))
         (take 5))))

(defn setup!
  [{:keys [conn bus] :as app}]
  (if (::results app)
    (recur (cleanup! app))
    (let [state (atom nil)
          index (initial-index @conn)
          bar-el (atom nil)]
      
      (d/listen!
       conn
       (fn [{:keys [tx-data]}]
         (js/console.time "FTS indexing")
         (doseq [^Datom dtm tx-data]
           (when (= (.-a dtm) :token/value)
             #_(prn "Indexing" dtm)
             (if (db/datom-added dtm)
               (.add index (.-e dtm) (.-v dtm))
               (.remove index (.-e dtm)))))
         (js/console.timeEnd "FTS indexing")))

      (d/transact! conn
                   [{:db/ident :sted.page/state
                     ::state state}])
      
      (-> app
          
          (core/register-mutation! :update-bar-ref
                                   (fn [[_ r] _ _]
                                     (d/transact! conn [{:db/ident :sted.page/state
                                                         ::bar-ref r}])))

          (core/register-mutation! :search/start  #(reset! state {:query ""}))
          (core/register-mutation! :search/cancel #(reset! state nil))
          
          (core/register-simple! :search/select
                                 (fn [sel isr]
                                   (let [st @state]
                                     (or
                                      ;; 
                                      (when-let [rs (:results @state)]
                                        (when (<= 1 isr (count rs))
                                          (let [^js r (nth rs (dec isr))]
                                            (swap! state dissoc :query)
                                            (core/move-selection-tx (:db/id sel) (.-eid r)))))
                                      ;; 
                                      (when-let [rs (:extra @state)]
                                        (when (<= 1 isr (count rs))
                                          (let [^js r (nth rs (dec isr))]
                                            (swap! state dissoc :query)
                                            (core/move-selection-tx (:db/id sel) (.-eid r)))))))))

          (core/register-mutation! :search/numeric
                                   (fn [_ db bus]
                                     (swap! state
                                            (fn [{:keys [cheese]}]
                                              (if cheese
                                                {}
                                                {:cheese
                                                 (sdom/numeric
                                                  (::bar-ref (d/entity db :sted.page/state))
                                                  (:db/id (core/get-selected-form db)))})))))
          
          (core/register-mutation! :update-search
                                   (fn [[_ text] db bus]
                                     (let [top-level (nav/parents-vec (core/get-selected-form db))
                                           chain (some-> top-level :coll/_contains first)
                                           rs (sdom/substring-search-all-visible-tokens
                                               (::bar-ref (d/entity db :sted.page/state))
                                               (:db/id chain)
                                               text)
                                           
                                           ;; _ (js/console.time "dbprefixsearch")
                                           ;; extra (when (< 0 (count text))
                                           ;;         (sdb/db-prefix-search db text 10))
                                           ;; _ (js/console.timeEnd "dbprefixsearch")
                                           
                                           ;; _ (js/console.time "flexsearch")
                                           ;; extra (when (< 0 (count text))
                                           ;;         (sfs db index text 10))
                                           ;; _ (js/console.timeEnd "flexsearch")
                                           ]
                                       (reset! state {:query text
                                                      :results rs
                                                      #_ #_:extra extra}))))))))

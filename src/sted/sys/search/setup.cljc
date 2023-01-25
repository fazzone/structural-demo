(ns sted.sys.search.setup
  (:require
   [rum.core :as rum]
   [sted.core :as core]
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

(defn setup!
  [{:keys [conn bus] :as app}]
  (if (::results app)
    (recur (cleanup! app))
    (let [results (atom [])
          state (atom nil)
          index (initial-index @conn)
          bar-el (atom nil)]
      
      (d/listen!
       conn
       (fn [{:keys [tx-data]}]
         #_(js/console.time "FTS indexing")
         (doseq [^Datom dtm tx-data]
           (when (= (.-a dtm) :token/value)
             #_(prn "Indexing" dtm)
             (if (db/datom-added dtm)
               (.add index (.-e dtm) (.-v dtm))
               (.remove index (.-e dtm)))))
         #_(js/console.timeEnd "FTS indexing")))

      (d/transact! conn
                   [{:db/ident :search/state
                     :bing results
                     :bong state}])
      
      #_(println "######## Create a new search updated")
      
      (-> app
          (assoc-in [:system :search]
                    {:results results :state state})
          
          (core/register-mutation! :update-bar-ref
                                   (fn [[_ r] _ _]
                                     (d/transact! conn [{:db/ident :sted.page/state
                                                         ::bar-ref r}])
                                     (js/console.log "Updated bar-ref " r)
                                     #_(reset! bar-el (rum/deref r))))

          (core/register-mutation! :search/start  #(reset! state true))
          (core/register-mutation! :search/cancel #(do
                                                     (reset! state nil)
                                                     (reset! results [])))
          
          (core/register-simple! :search/select
                                 (fn [sel isr]
                                   (let [rs (:results @results)]
                                     (when (<= 1 isr (count rs))
                                       (let [^js r (nth rs (dec isr))]
                                         (reset! state nil)
                                         (reset! results [])
                                         (core/move-selection-tx (:db/id sel) (.-eid r)))))))

          (core/register-mutation! :update-search
                                   (fn [[_ text] db bus]
                                     (when (< 1 (count text))
                                       (js/console.log "Updating search?" (::bar-ref (d/entity db :sted.page/state)))
                                       (let [rs (sdom/substring-search-all-visible-tokens
                                                 (::bar-ref (d/entity db :sted.page/state))
                                                 text)]
                                         (reset! results rs)))))))))

(ns sted.sys.search.setup
  (:require
   [rum.core :as rum]
   [sted.core :as core]
   [sted.sys.search.db :as sdb]
   [datascript.core :as d]
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
          state (atom {})
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
                                     (reset! bar-el (rum/deref r))))
          
          (core/register-mutation!
           :update-search
           (fn [[_ text] _ _]
             #_(println "UpdS" text)
             (let [t (str "dbsearch " text)]
               #_(prn "Prefxi" (count (sdb/db-prefix-search
                                       @conn
                                       text
                                       32)))
               
               #_(js/console.timeEnd t)
               
               #_(when (< 1 (count text))
                   (js/setTimeout
                    (fn []
                      (let [rs (sdom/substring-search-all-visible-tokens @bar-el text)]
                        (println "Search"
                                 (pr-str text)
                                 " finished. Swap the results")
                        (reset! results rs)))
                    0))
               
               (when (< 1 (count text))
                 (let [rs (sdom/substring-search-all-visible-tokens @bar-el text)]
                   #_(println "Search" (pr-str text) " finished. Swap the results")
                   (reset! results rs))))))))))

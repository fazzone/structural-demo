(ns sted.sys.search.setup
  (:require [sted.core :as core]
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
          index (initial-index @conn)]
      
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
      
      
      (-> app
          (assoc-in [:system :search]
                    {:results results :state state})
          (core/register-mutation!
           :update-search
           (fn [[_ text] _ _]
             #?(:cljs
                (let [t (str "dbsearch " text)]
                  #_(js/console.time t)
                  #_(prn "Prefxi" (count (sdb/db-prefix-search
                                          @conn
                                          text
                                          32)))
                  #_(js/console.timeEnd t)
                  (js/setTimeout
                   (fn []
                     (let [rs (.search index text)]
                       (println "Got some results " rs )
                       (reset! results rs)
                       (reset! sdom/results rs)
                       #_(reset! results (.search index text)
                                 #_(sdom/substring-search-all-visible-tokens text))))
                   #_(fn [] (reset! results (sdom/substring-search-all-visible-tokens text)))
                   0)))))))))

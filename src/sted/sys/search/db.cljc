(ns sted.sys.search.db
  (:require [datascript.core :as d]
            [datascript.db :as db #?@(:cljs [:refer [Datom]])]

            [clojure.string :as string]
            [sted.core :as core]
            #?(:cljs ["flexsearch" :as fs]))
  #?(:clj (:import [datascript.db Datom])))

#?(:cljs
   (defn search*
     [db sa text]
     ;; U+10FFFF
     #_(js/console.log "Inde"
                       (fs/Index.))
     (js/console.log "tokenvals"
                     #_(reduce
                        (fn [acc ^Datom d]
                          (doto acc (.push (.-v d))))
                        (array)
                        (d/datoms db :avet sa))
                     )
     #_(let [_ (js/console.time "Create index")
             fsindex (reduce
                      (fn [acc ^Datom d]
                        (doto acc
                          (.add (.-e d) (.-v d))
                          #_(.push (.-v d))))
                      (fs/Index.
                       #js {:tokenize "forward"})
                      (d/datoms db :avet sa))]
         (js/console.timeEnd "Create index")
         (doseq [mi (.search fsindex text)]
           (println "Match" (d/datoms db :eavt mi :token/value))))
  
     (d/index-range db sa text (str text "􏿿"))))

#?(:clj
   (defn search* [db sa text] nil))

(defn db-prefix-search
  [db prefix maxr]
  (let [rs (->> (search* db :token/value prefix)
                (group-by #(nth % 2))
                (sort-by (comp - count second))
                (take maxr))]
    rs))

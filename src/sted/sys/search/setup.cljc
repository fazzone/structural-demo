(ns sted.sys.search.setup
  (:require [sted.core :as core]
            #_[sted.sys.search.db :as sdb]
            #?(:cljs [sted.sys.search.dom :as sdom])))

(defn cleanup!
  [app]
  (dissoc app ::results))

(defn setup!
  [{:keys [bus] :as app}]
  (if (::results app)
    (recur (cleanup! app))
    (let [results (atom [])
          state (atom {})]
      (-> app
          (assoc ::results results
                 ::state state)
          (core/register-mutation!
           :update-search
           (fn [[_ text] _ _]
             #?(:cljs
                (let [t (str "dbsearch " text)]
                  #_(js/console.time t)
                  #_(prn "Prefxi" (count (sdb/db-prefix-search)))
                  #_(js/console.timeEnd t)
                  #_(js/setTimeout (fn [] (reset! results (sdom/substring-search-all-visible-tokens text)))
                                 0)))))))))

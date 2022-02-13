(ns sted.sys.search.setup
  (:require [sted.core :as core]
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
                (reset! results (sdom/substring-search-all-visible-tokens text)))))))))

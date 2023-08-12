(ns sted.embed.except
  (:require [sted.embed :as e]
            [sted.embed.common :as ec]
            [datascript.core :as d]))

(defn ex->tx
  [ex]
  (println "Extx" (pr-str ex))
  (let [d (ex-data ex)]
   (-> [:exception
        #?(:clj (str (type ex))
           :cljs (some-> ex (.-constructor ) (.-name)))
        (ex-message ex)
        {:d 1}
        (when-let [stk (some->> d :sci.impl/callstack deref)]
          (vec (for [ste stk]
                 [(symbol (str (:ns ste))
                          (or (some-> ste :sci.impl/f-meta :name)
                              "nofunc"))]
                 
                 )))]
       (e/->tx)
       (assoc :coll/type :eval-result))))

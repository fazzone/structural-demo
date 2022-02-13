(ns sted.sys.search.api
  (:require
   [sted.sys.search.dom :as sdom]))

(defn request-search-update!
  [text state results]
  #_(js/setTimeout
   (fn []
     (println "Now searching" text)
     (reset! results (sdom/substring-search-all-visible-tokens text)))
   0))

(ns sted.macros
  (:require
   [clojure.java.io :as io]
   #_[sted.embed :as e]))

(defmacro macro-slurp [a]
  (println "Slurp " (meta &form) a)
  (clojure.core/slurp a))

(defmacro macro-resource [a]
  (clojure.core/slurp (io/resource a)))

(defmacro macro-list-sources []
  (vec (map str (file-seq (io/file "src")))))
#_(defmacro macro->tx [a]
  (e/string->tx-all
   (clojure.core/slurp a)))

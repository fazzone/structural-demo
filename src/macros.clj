(ns macros
  (:require [embed :as e])
  )

(defmacro macro-slurp [a]
  (println "Slurp " a)
  (clojure.core/slurp a))

(defmacro macro->tx [a]
  (e/string->tx-all
   (clojure.core/slurp a)))



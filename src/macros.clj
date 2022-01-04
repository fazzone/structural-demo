(ns macros
  )

(defmacro macro-slurp [a]
  (println "Slurp " a)
  (clojure.core/slurp a))



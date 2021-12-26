(ns macros
  )

(defmacro macro-slurp [a]
  (println "Slurp " a)
  (clojure.core/slurp a))

#_(do
  (defmacro defmutation* [name lookup-ref & args]
    `(do
       {:name name
        :args args
        
        }
       )
    )
  (macroexpand
   (defmutation finish-edit [:form/editing true] []  )
   )
  
  )


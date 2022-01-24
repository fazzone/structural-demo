(ns ^:no-doc sted.df.async
  (:refer-clojure :exclude [let promise])
  #? (:cljs (:require-macros sted.df.async)))

(defmacro do [& body]
  (reduce
   (fn [chain form]
     `(.then ~chain
             (fn [] (js/Promise.resolve ~form))))
   `(js/Promise.resolve nil)
   body))

(defn do*
  [body]
  (reduce (fn [chain form]
            ;; `(.then ~chain (fn [] (js/Promise.resolve ~form)))
            (list '.then chain
                   (list 'fn [] (list 'js/Promise.resolve form))))
          (list 'js/Promise.resolve nil)
          body))



(defn let**
  [bindings body]
  (->> (partition-all 2 bindings)
       reverse
       (reduce (fn [body [n v]]
                 `(.then (js/Promise.resolve ~v) (~'fn [~n] ~body)))
               `(df.async/do ~@body)
               #_(do* body))))

(defn sci-let**
  [bindings body]
  (->> (partition-all 2 bindings)
       reverse
       (reduce (fn [body [n v]]
                 `(.then (js/Promise.resolve ~v) (~'fn [~n] ~body)))
               `(~'do ~@body)
               #_(do* body))))




(defmacro let [bindings & body] (let** bindings body))

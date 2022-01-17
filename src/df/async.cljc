(ns ^:no-doc df.async
  (:refer-clojure :exclude [let promise])
  #? (:cljs (:require-macros df.async)))

#_(defmacro do [& body]
  (reduce
   (fn [chain form]
     `(.then ~chain
             (fn [] (js/Promise.resolve ~form))))
   `(js/Promise.resolve nil)
   body))

(defn do*
  [& body]
  (reduce (fn [chain form] `(.then ~chain (fn [] (js/Promise.resolve ~form))))
    `(js/Promise.resolve nil)
    body))

(defn let**
  [bindings & body]
  (->> (partition-all 2 bindings)
       reverse
       (reduce (fn [body [n v]]
                 `(.then (js/Promise.resolve ~v) (fn [~n] ~body)))
         #_`(df.async/do ~@body)
         (do* body))))

(defmacro let [& args] (apply let** args))
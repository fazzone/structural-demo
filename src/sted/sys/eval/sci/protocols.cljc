(ns sted.sys.eval.sci.protocols
  (:refer-clojure :exclude [create-ns])
  (:require [clojure.core.protocols :as real-protocols]
            [clojure.datafy :as real-datafy]
            [sci.core :as sci :refer [copy-var]]
            [sci.impl.types :as types]
            [sci.impl.namespaces :as nses]))

;; lifted from https://github.com/babashka/babashka/blob/c7cc5663cbfb8842729e7e6368c8817aa47e78a0/src/babashka/impl/protocols.clj

(defmulti datafy (fn [x] (types/type-impl x)))

;; We require the context
(defmulti datafy* (fn [x c] (types/type-impl x)))

(defmethod datafy :sci.impl.protocols/reified [x]
  (let [methods (types/getMethods x)]
    ((get methods 'datafy) x)))

(defmethod datafy* :sci.impl.protocols/reified [x ctx]
  (let [methods (types/getMethods x)]
    ((get methods 'datafy) x)))

(defmethod datafy :default [x] (real-datafy/datafy x))

(defmulti nav types/type-impl)

(defmethod nav :sci.impl.protocols/reified [coll k v]
  (let [methods (types/getMethods coll)]
    ((get methods 'nav) coll k v)))

(defmethod nav :default [coll k v] (real-datafy/nav coll k v))

(def protocols-ns (sci/create-ns 'clojure.core.protocols nil))

(def core-procols
  {'Datafiable (-> `real-protocols/Datafiable
                   (sci/new-var {:methods  #{'datafy}
                                 :protocol real-protocols/Datafiable
                                 :ns       protocols-ns}
                                {:ns protocols-ns}))
   'datafy     (copy-var datafy protocols-ns)
   'Navigable  (-> `real-protocols/Navigable
                   (sci/new-var {:methods  #{'nav}
                                 :protocol real-protocols/Navigable
                                 :ns       protocols-ns}
                                {:ns protocols-ns}))
   'nav        (copy-var nav protocols-ns)
   'IKVReduce  (-> `real-protocols/IKVReduce
                   (sci/new-var {:protocol `real-protocols/IKVReduce
                                 #_ #_ :methods #{'kv-reduce}
                                 :ns       protocols-ns}
                                {:ns protocols-ns}))
   #_ #_'kv-reduce (copy-var kv-reduce protocols-ns)})

(def datafy-ns (sci/create-ns 'clojure.datafy nil))

(def core-datafy
  {'datafy (copy-var datafy datafy-ns)
   'nav    (copy-var nav datafy-ns)})


#_(extend-protocol p/Datafiable
  SciNamespace
  (datafy [n]
    (with-meta {:name 9
                :publics (->> n (sci-ns-publics @ctx) sortmap)
                :imports (->> n (sci-ns-imports @ctx) sortmap)
                :interns (->> n (sci-ns-interns @ctx) sortmap)}
      (meta n))))

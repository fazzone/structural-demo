(ns sted.sys.eval.sci
  (:require [sci.core :as sci]
            [clojure.core.protocols :as p]
            [sted.embed :as e]
            [sted.cmd.move :as move]
            [sted.cmd.nav :as nav]
            [daiquiri.interpreter :as rdi]
            [datascript.core :as d]
            [datascript.serialize :as dser]
            [sci.impl.vars :as v]
            [sted.sys.nrepl.bencode :as benc]
            [sted.df.github :as dfg]
            [sted.df.async :as a]
            [clojure.datafy :as datafy]
            [clojure.string :as string]
            [sted.core :as core :refer [get-selected-form]]))

(def electron-bridge #? (:cljs (aget js/window "my_electron_bridge")
                        :clj nil))

(defn list-dir*
  [p]
  (when-some [f (some-> electron-bridge
                        (aget "list_dir"))]
    (.then (f p) js->clj)))

(defn sci-opts
  ([app] (sci-opts app nil))
  ([{:keys [conn bus] :as app} {:keys [namespaces bindings]}]
   {:classes    {'js goog/global :allow :all}
    :namespaces (-> {'d        {'datoms    d/datoms
                                'touch     d/touch
                                'entity-db d/entity-db
                                'entity    d/entity
                                'q         d/q
                                'freeze    dser/serializable
                                'thaw      dser/from-serializable}
                     'js       {'Promise.resolve (fn [p]
                                                   (println "Resolver" p)
                                                   (js/Promise.resolve p))}
                     'df.async {'do ^:sci/macro (fn [&form &env & body]
                                                  (println "Macro!" &form body)
                                                  (let [ret (a/do* body)]
                                                    (println "Ret:Do" ret)
                                                    ret))}
                     'dom {'qsa (fn [s] (vec (js/document.querySelectorAll s)))
                           'ecl (fn [s] (vec (js/document.getElementsByClassName s)))
                           'qs  (fn [s] (js/document.querySelector s))
                           'eid (fn [s] (js/document.getElementById s))}
                     'di       {'element   rdi/element
                                'interpret rdi/interpret}
                     'a        {'let ^:sci/macro (fn [&form &env bindings & body]
                                                   (println "Macro!Let" &form)
                                                   (prn 'bindings bindings 'body body)
                                                   (let [ret (a/sci-let** bindings body)]
                                                     (println "Ret:Let" ret)
                                                     ret))}
                     
                     'b {'encode benc/serialize
                         'decode benc/deserialize}
                     'hn {'stories dfg/stories}
                     #?@ (:cljs ['p {'resolve (fn [p] (js/Promise.resolve p))
                                     'all     (fn [a b c] (js/Promise.all a b c))}])}
                    (merge namespaces))
    :bindings (-> {'send!  (fn [m] (core/send! bus m))
                   '->seq  e/seq->seq
                   'datafy datafy/datafy
                   'nav    datafy/nav
                   'ls     dfg/ls
                   'thing  (fn [z]
                             (mapv pr-str z))
                   #?@ (:clj ['slurp slurp
                              'spit spit]
                        :cljs ['fetch-text (fn [z]
                                             (.then (js/fetch z)
                                                    (fn [resp] (.text resp))))
                               'fetch-json (fn [u]
                                             (.then (js/fetch u)
                                                    (fn [resp]
                                                      (.then (.json resp)
                                                             #(js->clj % :keywordize-keys true)))))
                               'ingest (fn [z]
                                         (with-meta (list z) {`ingest true}))
                               'then (fn [p f] (.then (js/Promise.resolve p) f))
                               'slurp      (some-> electron-bridge (aget "slurp"))
                               'spit       (some-> electron-bridge (aget "spit"))
                               'list-dir   list-dir*   #_(some-> electron-bridge (aget "list_dir"))
                               'new-window (fn [u name features]
                                             (js/window.open
                                              (or u js/window.location)
                                              name
                                              features))])}
                  (merge bindings))}))

(defn failure-cont
  [eval-target bus print-output]
  (fn [ex]
    (core/send! bus
                [:eval-result eval-target
                 (str (ex-message ex)
                      "\n"
                      (.-stack ex))])))

(defn coll-contains?
  [parent child?]
  (-> (d/entity-db child?)
      (d/datoms :avet
                :coll/contains
                (:db/id child?)
                (:db/id parent))
      (seq)))

#_(defn key-within-map
  [target context]
  (when (coll-contains? context target)
    (loop [k nil
           s context
           in-key? true]
      (if (= (:db/id target) (:db/id (:seq/first s)))
        (-> (if in-key? s k) :seq/first e/->form)
        (if-not (:seq/next s)
          (throw (ex-info "It was supposed to be in here" {}))
          (recur s (:seq/next s) (not in-key?)))))))

(defn key-within-map
  [target context]
  (when (coll-contains? context target)
    (e/->form target)))

(defn eval-action?
  [e]
  (let [ea (or (some-> e :eval/action vector)
               (when-some [m (:nav/pointer e)]
                 [:nav]))]
    (when ea
      (into [e] ea))))

(defn mutatef
  [{:keys [conn bus] :as app}]
  (let [sel# (sci/new-dynamic-var 'sel nil)
        ctx  (sci/init (sci-opts app {:bindings {'sel sel#}}))]
    (fn [_ db bus]
      (let [eval-target                  (get-selected-form db)
            parents                      (nav/parents-seq eval-target)
            [eval-context action :as jj] (or (some eval-action? parents)
                                             [(last parents) :eval])
            print-output                 (atom [])]
        (letfn [(failure [ex]
                  (core/send! bus [:eval-result eval-target
                                   (str (ex-message ex) "\n" (.-stack ex))]))
                (success [r]
                  (core/send! bus
                              (cond
                                (or (= :nav action) (some-> r meta (get `p/nav)))
                                [:ingest-result eval-target r]

                                (some-> r meta (get `ingest))
                                [:ingest-after eval-target (first r)]
                                
                                :else
                                [:eval-result eval-target (str r) print-output ])))]
          (try (-> (case action
                     :nav (when-some [ptr (:nav/pointer eval-context)]
                            (let [k (e/->form eval-target)]
                              (case (:coll/type eval-context)
                                (:vec :list) (datafy/nav ptr nil k)
                                (:map :set)  (datafy/nav ptr k (get ptr k)))))
                     :eval (sci/binding [sel# eval-target
                                         sci/print-newline true
                                         sci/print-fn (fn [m] (swap! print-output conj m))]
                             (sci/eval-string* ctx (e/->string eval-context))))
                   #?@ (:clj [(success)]
                        :cljs [(js/Promise.resolve) (.then success) (.catch failure)]))
               (catch :default ex (js/console.log ex) (failure ex))))))))

(comment
  (sci/binding [sci/print-newline true
                sci/print-fn (fn [s] (swap! output str s))]
    (sci/eval-string "(print :hello) (println :bye)")))

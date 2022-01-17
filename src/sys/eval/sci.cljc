(ns sys.eval.sci
  (:require
   [sci.core :as sci]
   [clojure.core.protocols :as p]
   [embed :as e]
   [cmd.move :as move]
   [cmd.nav :as nav]
   [datascript.core :as d]
   [sci.impl.vars :as v]
   [df.github :as dfg]
   [df.async :as a]
   [clojure.datafy :as datafy]
   [core :as core :refer [get-selected-form]]))

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
   {:classes {'js goog/global :allow :all}

    :namespaces (-> {'d {'datoms d/datoms 'touch d/touch}
                     'js {'Promise.resolve (fn [p]
                                             (println "Resolver" p)
                                             (js/Promise.resolve p))}

                     'df.async {'do ^:sci/macro (fn [&form &env & body]
                                                  (println "Macro!" &form body)
                                                  (let [ret (a/do* body)]
                                                    (println "Ret:Do" ret)
                                                    ret))}
                     'a {'let ^:sci/macro (fn [&form &env bindings & body]
                                            (println "Macro!Let" &form)
                                            (prn 'bindings bindings 'body body)
                                            (let [ret (a/sci-let** bindings body)]
                                              (println "Ret:Let" ret)
                                              ret))}
                     'hn {'stories dfg/stories}
                     #?@ (:cljs ['p {'resolve (fn [p] (js/Promise.resolve p))
                                     'all     (fn [a b c] (js/Promise.all a b c))}])}
                    (merge namespaces))
    :bindings   (-> {'send! (fn [m] (core/send! bus m))
                     '->seq e/seq->seq
                     'datafy datafy/datafy
                     'nav    datafy/nav
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
                                 'ingest (fn [z] (with-meta z {`ingest true}))
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



(defn success-cont
  [eval-target bus]
  (fn [result]
    (cond
      (some-> result meta (get `p/nav))
      (core/send! bus [:ingest-result eval-target result])

      (some-> result meta (get `ingest))
      (core/send! bus [:ingest-after eval-target result])
      
      (v/var? result)
      (core/send! bus
                  [:eval-result eval-target
                   (str (pr-str result) " => " (pr-str @result))])
      
      :else (core/send! bus [:eval-result eval-target (pr-str result)]))))

(defn failure-cont
  [eval-target bus]
  (fn [ex]
    (core/send! bus
                [:eval-result eval-target
                 (or (ex-message ex) (str ex))])))


(defn coll-contains?
  [parent child?]
  (seq (d/datoms (d/entity-db child?) :avet
                 :coll/contains
                 (:db/id child?)
                 (:db/id parent))))

(defn key-within-map
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

(defn mutatef
  [{:keys [conn bus] :as app}]
  (let [sel# (sci/new-dynamic-var 'sel nil)
        ctx  (sci/init (sci-opts app {:bindings {'sel sel#}}))]
    (fn [_ db bus]
      (let [eval-target  (get-selected-form db)
            parents      (nav/parents-seq eval-target)
            eval-context (or (first (filter :eval/action parents))
                             (last parents))
            action       (:eval/action eval-context)
            eval-string  (e/->string eval-context)
            success      (success-cont eval-target bus)
            failure      (failure-cont eval-target bus)]
        (try
          (-> (case action
                :nav (when-some [ptr (:nav/pointer eval-context)]
                       (case (:coll/type eval-context)
                         (:vec :list) (let [k (e/->form eval-target)]
                                        (datafy/nav ptr nil k))
                         :map         (let [k (key-within-map eval-target eval-context)]
                                        (datafy/nav ptr k (get ptr k)))))
                
                (sci/binding [sel# eval-target]
                  (sci/eval-string* ctx (e/->string eval-context))))
              #?@(:clj [(success)]
                  :cljs [(js/Promise.resolve)
                         (.then success)
                         (.catch failure)]))
          (catch :default ex (js/console.log ex) (failure ex)))))))

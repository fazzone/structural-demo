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
            [sci.impl.namespaces :as n]
            #_[sted.sys.nrepl.bencode :as benc]
            [sted.df.github :as dfg]
            [sted.df.async :as a]
            [clojure.datafy :as datafy]
            [clojure.string :as string]
            [sted.core :as core :refer [get-selected-form]]
            [sted.sys.eval.sci.protocols :as esp]
            [goog.object :as gobj]
            [sted.sys.handle :as sh]
            [mdast-util-from-markdown :as mdast]
            [sted.doc.remark :as sdr]))

(def electron-bridge #? (:cljs (aget js/window "my_electron_bridge")
                         :clj nil))

(def the-sci-context (atom nil))

(defn list-dir*
  [p]
  (when-some [f (some-> electron-bridge
                        (aget "list_dir"))]
    (.then (f p) js->clj)))

(defn list-chains
  [db]
  )

(defn transactor
  [conn]
  (fn [tx-data]
    (js/Promise.
     (fn [resolve reject]
       (let [tx-report (d/transact! conn tx-data)]
         (js/setTimeout
          (fn []
            (resolve {:tx-data (:tx-data tx-report)
                      :tempids (:tempids tx-report)}))
          0))))))

(defn new-window-with-text
  [text]
  (let [w (js/window.open "")]
    (-> (fn []
          (let [el (js/document.createElement "pre")]
            (set! (.-innerText el) text)
            (.appendChild (.-body (.-document w)) el)))
        (js/setTimeout 0))))

(defn sci-opts
  [{:keys [conn bus] :as app}]
  {:classes    {'js goog/global :allow :all}
   :namespaces {'d        {'datoms    d/datoms
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
                'dom      {'qsa (fn [s] (vec (js/document.querySelectorAll s)))
                           'ecl (fn [s] (vec (js/document.getElementsByClassName s)))
                           'qs  (fn [s] (js/document.querySelector s))
                           'eid (fn [s] (js/document.getElementById s))
                           'sel (fn [s] (js/document.querySelectorAll ".selected"))}
                'di       {'element   rdi/element
                           'interpret rdi/interpret}
                'sdr {'go
                      (fn [j]
                        (sdr/go j))}
                'mdast    {'parse
                           (fn [mdtext]
                             (mdast/fromMarkdown mdtext))}
                'gobj     {'view   (fn [obj] (gobj/createImmutableView obj))
                           'extend (fn [a b] (gobj/extend a b))
                           'get (fn [o k d]
                                  (gobj/get o k d))}
                'a        {'let ^:sci/macro (fn [&form &env bindings & body]
                                              (a/sci-let** bindings body))}
                'hn       {'stories dfg/stories}
                #?@ (:cljs ['p {'resolve (fn [p] (js/Promise.resolve p))
                                'all     (fn [a b c] (js/Promise.all a b c))}])}
   :bindings   {'send!   (fn [m]
                           (js/setTimeout (fn [] (core/send! bus m))
                                          0))
                '->seq   e/seq->seq
                'datafy  p/datafy
                'nav     p/nav
                'ls      dfg/ls
                'object? (fn [o] (object? o))
                'textwin new-window-with-text
                'handle (fn [k]
                          (get @sh/storage k))
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
                                           features))])}})

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

(defn nav-root
  [conn po]
  (fn []
    (let [db @conn
          cts (fn [ch]
                (if-let [[_ _ _ t] (d/datoms db :eavt (:db/id ch) :chain/selection)]
                  t
                  (:db/id ch)))
          nav-entity (fn nav-impl [c k v]
               (js/console.log "At" (:max-tx @conn) "Nav you " c k v )
               (if (number? k)
                 (d/entity db k)))]
      (with-meta
        {:max-tx (:max-tx db)
         :prints (deref po) 
         :ctl '[transact!]
         :db (d/entity db :sted.page/state)}
        {`p/nav nav-entity}))))

(extend-protocol p/Datafiable
  v/SciNamespace
  (datafy [n]
    the-sci-context
    ))

(defn mutatef
  [{:keys [conn bus] :as app} app-atom-circular-ref]
  (let [sel#           (sci/new-dynamic-var 'sel nil)
        app#           (sci/new-var 'sted
                                    app-atom-circular-ref
                                    nil)
        unbound-prints (atom [])
        ctx            (-> (sci-opts app #_{:bindings {'sel sel#}})
                           (sci/init)
                           (sci/merge-opts {:bindings {'sel sel#
                                                       'sted app#      #_(nav-root conn unbound-prints)
                                                       'bus       bus
                                                       'transact! (transactor conn)}}))]
    (sci/alter-var-root sci/print-newline (fn [_] false))
    (sci/alter-var-root sci/print-fn (fn [_]
                                       (fn [msg]
                                         (swap! unbound-prints conj msg))))
    (reset! the-sci-context ctx)
    (fn [_ db bus]
      (let [tes                          (js/performance.now)
            eval-target                  (get-selected-form db)
            parents                      (nav/parents-seq eval-target)
            [eval-context action :as jj] (or (some eval-action? parents)
                                             [(last parents) :eval])
            print-output                 (atom [])]
        (letfn [(failure [ex]
                  (core/send! bus [:eval-result eval-target
                                   (str (ex-message ex) "\n" (.-stack ex))]))
                (success [r]
                  (println "Eval-success " (type r)
                           (- (js/performance.now) tes)
                           "ms")
                  (when (instance? v/SciVar r)
                    (println "Value" (v/getRawRoot r)))
                  (when (instance? v/SciNamespace r)
                    (println "Value" r))

                  (core/send! bus
                              (cond
                                (or (= :nav action) (some-> r meta (get `p/nav)))
                                [:ingest-result eval-target r]

                                (some-> r meta (get `ingest))
                                [:ingest-after eval-target (first r)]
                                
                                :else
                                [:eval-result eval-target (str r) print-output ]
                                
                                )))]
          (try (-> (case action
                     :nav  (when-some [ptr (:nav/pointer eval-context)]
                             (let [k (e/->form eval-target)]
                               (case (:coll/type eval-context)
                                 (:vec :list) (datafy/nav ptr nil k)
                                 (:map :set)  (datafy/nav ptr k (get ptr k)))))
                     :eval (sci/binding [sel# eval-target
                                         sci/print-fn (fn [m] (swap! print-output conj m))]
                             (sci/eval-string* ctx (e/->string eval-context))))
                   #?@ (:clj [(success)]
                        :cljs [(js/Promise.resolve) (.then success) (.catch failure)]))
               (catch :default ex (js/console.error "sci exception" ex) (failure ex))))))))

(comment
  (sci/binding [sci/print-newline true
                sci/print-fn (fn [s] (swap! output str s))]
    (sci/eval-string "(print :hello) (println :bye)")))

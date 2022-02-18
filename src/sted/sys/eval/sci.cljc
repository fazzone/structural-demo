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
            [sted.df.github :as dfg]
            [sted.df.async :as a]
            [clojure.datafy :as datafy]
            [clojure.string :as string]
            [sted.core :as core :refer [get-selected-form]]
            [sted.sys.eval.sci.protocols :as esp]
            [goog.object :as gobj]
            [sted.sys.handle :as sh]
            [mdast-util-from-markdown :as mdast]))

(def electron-bridge #? (:cljs (aget js/window "my_electron_bridge")
                         :clj nil))

(def the-sci-context (atom nil))

(defn list-dir*
  [p]
  (when-some [f (some-> electron-bridge
                        (aget "list_dir"))]
    (.then (f p) js->clj)))

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
                           (js/Promise.
                            (fn [s f]
                             (js/setTimeout (fn [] (s (core/send! bus m)))
                                            0))))
                '->seq   e/seq->seq
                'datafy  p/datafy
                'nav     p/nav
                'ls      dfg/ls
                'object? (fn [o] (object? o))
                'textwin new-window-with-text
                'handle (fn [k]
                          (get @sh/storage k))
                'storage sh/storage
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
  (or (when-let [t (:handle/token e)]
        [t :token]))
  #_(let [ea (or (some-> e :eval/action vector)
                 #_(when-some [m (:nav/pointer e)]
                     [:nav])
                 (when-some [m (:handle/token e)]
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
                           (sci/merge-opts {:bindings {'sel       sel#
                                                       ;; 'sted      app# 
                                                       'bus       bus
                                                       'transact! (transactor conn)}}))]
    (sci/alter-var-root sci/print-newline (fn [_] false))
    (sci/alter-var-root sci/print-fn (fn [_]
                                       (fn [msg]
                                         (swap! unbound-prints conj msg))))
    (reset! the-sci-context ctx)
    (fn [_ db bus]
      (let [target  (get-selected-form db)]
        (if-let [h (:handle/token target)]
          (core/send! bus [:eval-cont target (sh/load h)])
          (if-let [up-h (some-> target move/up :handle/token)]
            (let [c (sh/load up-h)
                  k (e/->form target)]
              (.then (js/Promise.resolve
                      (cond
                        (sequential? c)
                        (datafy/nav c nil k)

                        (associative? c)
                        (datafy/nav c k (get c k))
                        
                        (set? c)
                        (datafy/nav c k k)
                        
                        :else
                        (str "Lmao:" (type c))))
                     (fn [z]
                       #_(core/send! bus [:eval-cont target (list nil z)])
                       (core/send! bus [:eval-inplace target z]))))
            ;; 
            (let [top (peek (nav/parents-vec target))
                  estr (e/->string top)
                  res (sci/binding [sel# target] (sci/eval-string* ctx estr))]
              (if-not (instance? js/Promise res)
                (core/send! bus [:eval-result top res])
                (do
                  (println "It was a promise")
                  (.then res
                         (fn [v]
                           (core/send! bus [:eval-result top v]))))))))))))

(comment
  (sci/binding [sci/print-newline true
                sci/print-fn (fn [s] (swap! output str s))]
    (sci/eval-string "(print :hello) (println :bye)")))

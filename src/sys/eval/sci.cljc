(ns sys.eval.sci
  (:require
   [sci.core :as sci]
   [embed :as e]
   [cmd.move :as move]
   [datascript.core :as d]
   [sci.impl.vars :as v]
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
   {:namespaces (-> {'d {'datoms d/datoms 'touch d/touch}
                     #?@ (:cljs ['p {'resolve (fn [p] (js/Promise.resolve p))
                                     'all     (fn [a b c] (js/Promise.all a b c))}])}
                    (merge namespaces))
    :bindings   (-> {'send! (fn [m] (core/send! bus m))
                     '->seq e/seq->seq
                     'datafy datafy/datafy
                     'nav    datafy/nav
                     ;; 'stories dfg/stories
                     #?@ (:clj ['slurp slurp
                                'spit spit]
                          :cljs ['fetch-text (fn [z f]
                                               (.then (js/fetch z)
                                                      (fn [resp] (.text resp))))
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

(defn mutatef
  [{:keys [conn bus] :as app}]
  (let [sel# (sci/new-dynamic-var 'sel nil)
        ctx  (sci/init (sci-opts app {:bindings {'sel sel#}}))]
    (fn [_ db bus]
      (let [eval-target  (get-selected-form db)
            eval-context (->> (get-selected-form db)
                              (move/move :move/most-upward))
            eval-string  (e/->string eval-context)
            success      (fn [result]
                           (if (v/var? result)
                             (core/send! bus [:eval-result eval-target (str
                                                                        (pr-str result)
                                                                        " => "
                                                                        (pr-str @result))])
                             (core/send! bus [:eval-result eval-target (pr-str result)])))
            failure      (fn [ex]
                           (core/send! bus [:eval-result eval-target (or (ex-message ex) (str ex))]))]
        (try
          (let [ans (sci/binding [sel# eval-target]
                      (sci/eval-string* ctx eval-string))]
            #? (:clj (success ans)
               :cljs (.then (js/Promise.resolve ans)
                            success)))
          (catch :default ex
            (js/console.log ex)
            (failure ex)))))))
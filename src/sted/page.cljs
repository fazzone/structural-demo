(ns sted.page
  (:require
   [clojure.edn :as edn]
   [sted.embed :as e]
   [sted.schema :as s]
   [goog.string :as gstring]
   [goog.functions :as gfunc]
   [datascript.core :as d]
   [clojure.datafy :as datafy]
   [clojure.string :as string]
   [rum.core :as rum]
   [cljs.core.async :as async]
   [sci.core :as sci]
   [sted.db-reactive :as dbrx]
   [sted.comp.cons :as cc]
   [sted.comp.edit-box :as eb]
   [sted.comp.code :as code]
   [sted.comp.common :as ccommon]
   [sted.comp.modeline :as ml]
   [sted.cmd.move :as move]
   [sted.cmd.nav :as nav]
   [sted.cmd.insert :as insert]
   [sted.cmd.edit :as edit]
   [sted.cmd.mut :as mut]
   [sted.df.github :as dfg]
   [sted.df.async :as a]
   [sted.sys.eval.sci :as eval-sci]
   [sted.sys.fmt :as sf]
   [sted.sys.kbd.map :as skm]
   [sted.sys.kbd.evt :as ske]
   [sted.sys.mouse :as sm]
   [zprint.core :as zp-hacks]
   [sted.core :as core :refer [get-selected-form
                               move-selection-tx]])
  (:require-macros
   [cljs.core.async.macros :refer [go
                                   go-loop]]
   [sted.macros :as m]))

(def test-form-data-bar (e/string->tx-all (m/macro-slurp "srv/index.cljc")))

(def init-tx-data
  
  (let [chains (concat
                #_[(e/string->tx-all (m/macro-slurp "src/core.cljc"))]
                #_[(e/string->tx-all (m/macro-slurp "src/cmd/edit.cljc"))]
                [test-form-data-bar]
                #_[(e/->tx [^:form/highlight ()])]
                #_[(e/string->tx-all (m/macro-slurp "subtree/input.clj"))])]
    [{:db/ident ::state  :state/bar "bar"}
     {:db/ident ::command-chain
      :db/id "command-chain"
      :coll/type :vec
      :form/highlight true}
     {:db/ident ::inspect  :db/id "inspect"  :coll/type :inspect}
     {:db/ident ::default-keymap
      :db/id "defaultkeymap"
      :coll/type :keyboard
      :keymap/bindings (for [[k m] skm/default]
                         {:key/kbd k  :key/mutation m})}
     (assoc (e/seq-tx
             (concat
              (for [ch chains]
                (assoc ch
                       :coll/type :chain
                       :coll/_contains "bar"))
              [{:db/ident ::meta-chain
                :coll/type :chain
                :coll/_contains "bar"
                :coll/contains #{"label" "defaultkeymap" "inspect"
                                 "command-chain"}
                :seq/first {:db/id "label"
                            :token/type :string
                            :token/value "Keyboard"}
                :seq/next {:seq/first "defaultkeymap"
                           :seq/next {:seq/first "command-chain"
                                      :seq/next {:seq/first "inspect"}}}}]))
            :db/id "bar"
            :coll/type :bar)]))

#_(declare snapshot)

#_(rum/defc display-undo-preview
  [c b s top?]
  [:ul.undo-preview
   #_(when s {:class s :ref "selected"})
   {}
   (when top?
     [:span.form-title "History"])
   (for [e (cond-> (e/seq->vec c) (not top?) reverse)]
     (rum/with-key
       (snapshot e b)
       (:db/id e)))])

#_(rum/defc snapshot < dbrx/ereactive
  [e bus]
  (let [tx (int (:token/value e))
        r  (core/get-history bus tx)]
    (if-not tx
      (display-undo-preview e bus (when (:form/highlight e) "selected") nil)
      [:li.undo-preview-entry
       {:class [(when (:form/highlight e)
                  "selected")]}
       (str tx " " (some-> r :mut pr-str))
       #_(apply str (interpose " " (map pr-str (:mut r))))
       (if-not (some (fn [[e a v t]] (not= a :form/highlight))
                     (:tx-data r))
         " movement only"
         (when r
           [:div.alternate-reality
            {}
            ^:inline (-> (peek (nav/parents-vec (get-selected-form (:db-after r))))
                         (fcc core/blackhole 0 nil))]))])))

#_(defmethod display-coll :undo-preview  [c b i s p]
  (display-undo-preview c b s true))

(rum/defc root-component
  [db bus]
  (let [state (d/entity db ::state)
        ml-ref (rum/create-ref)]
    [:div.bar-container {} #_(test-image)
     (rum/bind-context [ccommon/*modeline-ref* ml-ref]
                       (code/form (:state/bar state) bus 0 nil))
     [:div.modeline-outer {:id "modeline" :ref ml-ref}]
     #_(ml/modeline-portal db bus)
     #_(cc/svg-viewbox (:state/bar state) core/blackhole)]))

(def keyboard-bus (atom nil))

(defn global-keydown*
  [ev]
  (let [tkd (js/performance.now)]
    (when (identical? js/document.body js/document.activeElement)
      (let [kbd (ske/event->kbd ev)
            bindings skm/default
            mut (get bindings kbd)]
        (println kbd mut)
        (core/send! @keyboard-bus [:kbd kbd tkd])
        (when (some? mut) (.preventDefault ev) (.stopPropagation ev))))))

(defonce global-keydown
  (fn [ev]
    (global-keydown* ev)))



(defn save*
  [file contents]
  (if-let [spit (some-> (aget js/window "my_electron_bridge")
                        (aget "spit"))]
    (-> (spit file contents)
        (.then  (fn [] (swap! ml/save-status assoc :status :ok :file file)))
        (.catch (fn [] (swap! ml/save-status assoc :status :error))))
    #_(do
      (js/console.time "Thing")
      (prn  (thinger "c"))
      (js/console.timeEnd "Thing"))
    (let [w (js/window.open "")]
      (js/setTimeout
       (fn []
         (let [el (js/document.createElement "pre")]
           (set! (.-innerText el) contents)
           (.appendChild (.-body (.-document w)) el)))
       0))))

(defn setup-app
  ([] (setup-app (doto (d/create-conn s/schema) (d/transact! init-tx-data))))
  ([conn]
   (let [a (core/app conn)]
     (doseq [[m f] mut/movement-commands]
       (core/register-simple! a m (core/movement->mutation f)))
     (doseq [[m f] mut/editing-commands]
       (core/register-simple! a m f))
     (-> a
         (core/register-mutation! :kbd
                                  (fn [[_ kbd tkd] db bus]
                                    (when-let [mut (:key/mutation (d/entity db [:key/kbd kbd]))]
                                      (core/send! bus [mut]))))
         (core/register-mutation! :eval-sci (eval-sci/mutatef a))
         (core/register-simple! :zp (sf/mutatef a))
         (sm/setup!)
         (core/register-mutation!
          :save
          (fn [_ db bus]
            (let [sel (get-selected-form db)
                  chain (-> sel
                            (nav/parents-vec)
                            (peek)
                            :coll/_contains
                            (first))
                  file (or (:chain/filename chain) "noname.clj")]
              (when chain
                (reset! ml/save-status {:at (:max-tx db)
                                        :on (:db/id sel)
                                        :status :saving})
                (save* file (e/->string chain)))
              nil)))))))

(defonce the-singleton-db
  (doto (d/create-conn s/schema)
    (d/transact! init-tx-data)))

(defn fetch-json
  [u]
  (-> (js/fetch u)
      (.then #(.json %))
      (.then #(js->clj % :keywordize-keys true))))

(defn stupid-github-crap
  []
  (a/let [ref    (fetch-json "https://api.github.com/repos/babashka/sci/git/ref/heads/master")
          commit (fetch-json (-> ref :object :url))
          tree   (fetch-json (-> commit :tree :url))]
    #_(cljs.pprint/pprint tree)))

(defonce the-app (atom nil))


(defn ^:dev/after-load init
  []
  (some-> @the-app (sm/cleanup!))
  
  (js/document.removeEventListener "keydown" global-keydown true)
  (js/document.addEventListener "keydown" global-keydown true)
  
  (comment
   (js/document.removeEventListener "mousedown" global-mousedown)
   (js/document.addEventListener "mousedown" global-mousedown)
    
   (js/document.removeEventListener "mouseup" global-mouseup true)
   (js/document.addEventListener "mouseup" global-mouseup true)

   (js/document.removeEventListener "contextmenu" global-ctxmenu true)
   (js/document.addEventListener "contextmenu" global-ctxmenu true))
  
  (some-> the-singleton-db meta :listeners (reset! {}))
  (let [{:keys [conn bus] :as app} (setup-app the-singleton-db)]
    (reset! the-app app)
    (reset! keyboard-bus bus)
    
    (when-let [req-title (some-> js/window.location.search
                                 (js/URLSearchParams.)
                                 (.get "title"))]
      (set! js/document.title req-title))
    (rum/mount #_(debug-component)
               (root-component @conn bus)
               (.getElementById js/document "root"))))

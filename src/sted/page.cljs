(ns sted.page
  (:require
   [clojure.edn :as edn]
   [sted.embed :as e]
   [sted.embed.common :as ec]
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
   [sted.comp.scroll :as csc]
   [sted.comp.common :as ccommon]
   [sted.comp.modeline :as ml]
   [sted.comp.root :as cr]
   [sted.comp.doc :as cdoc]
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
   [sted.sys.search.setup :as search]
   [sted.sys.mouse :as sm]
   [sted.sys.handle :as sh]
   [sted.sys.keyboard :as sk]
   [goog.object :as gobj]
   [zprint.core :as zp-hacks]
   [sted.core :as core :refer [get-selected-form]]
   [shadow.resource :as rc])
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop]]
   [sted.macros :as m]))

(def test-form-data-bar (assoc (e/string->tx-all (m/macro-slurp "src/sted/user.clj"))
                               :chain/filename  "src/sted/user.clj"))

(def init-tx-data
  (let [chains (concat
                #_[(e/string->tx-all (m/macro-slurp "src/core.cljc"))]
                #_[(e/string->tx-all (m/macro-slurp "src/cmd/edit.cljc"))]
                #_[(e/->tx [()])]
                [test-form-data-bar]
                #_[(e/->tx ["hello"
                            "hello"
                            "hello"
                            "hello"
                            "hello"
                            "hello"
                          
                            ])]
                #_[(e/->tx [^:form/highlight ()])]
                #_[(e/string->tx-all (m/macro-slurp "subtree/input.clj"))])]
    [{:db/ident ::state
      :state/bar "bar"
      :state/limit 512}
     {:db/ident ::command-chain
      :db/id "command-chain"
      :coll/type :vec
      :form/highlight true}
     {:db/ident ::inspect  :db/id "inspect"
      :coll/type :demo
      
      }
     {:db/ident ::default-keymap
      :db/id "defaultkeymap"
      :coll/type :keyboard
      :keymap/bindings (for [[k m] skm/default]
                         {:key/kbd k  :key/mutation m})}
     (assoc (ec/seq-tx
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

(defn ask-download-file
  [path contents]
  (let [b (js/Blob. #js [contents])
        u (js/URL.createObjectURL b)
        a (js/document.createElement "a")]
    (set! (.-href a) u)
    (set! (.-download a) path)
    (.click a)))

(defn save*
  [path contents]
  #_(.writeFile (js/require "fs/promises") file contents)
  (or (when-let [require (aget js/window "secret_electron_require")]
        #_(-> (.writeFile ^js (require "fs/promises") path contents )
              (.then  (fn [] (swap! ml/save-status assoc :status :ok :file path)))
              (.catch (fn [] (swap! ml/save-status assoc :status :error))))
        (let [^js fs (require "fs/promises")
              hack (str path "##")]
          (a/let [_ (.writeFile fs hack contents)
                  _ (.rename fs hack path)]
            (js/console.log "Wrote " path))))
      
      (do (ask-download-file path contents)
          (swap! ml/save-status assoc :status :ok :file path)))
  
  #_(if-let [spit (some-> (aget js/window "my_electron_bridge")
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

(defonce ^:export the-app (atom nil))


(defn setup-app
  ([] (setup-app (doto (d/create-conn s/schema) (d/transact! init-tx-data))))
  ([conn]
   (let [zb (some-> the-app deref :bus (core/reset))
         _ (js/console.log "Zbus" zb)
         a (core/app conn zb)]
     (doseq [[m f] mut/movement-commands]
       (core/register-simple! a m (core/movement->mutation f)))
     (doseq [[m f] mut/editing-commands]
       (core/register-simple! a m f))
     (-> a
         
         (sh/setup!)
         
         (core/register-mutation! :eval-sci (eval-sci/mutatef a the-app))
         (core/register-simple! :zp (sf/mutatef a))
         (core/register-mutation! :scroll (fn [_ _ _] (csc/scroll-to-selected!)))
         
         (sk/setup!)
         (sm/setup!)
         (search/setup!)
         (core/register-mutation!
          :save
          (fn [_ db bus]
            (let [sel (get-selected-form db)
                  chain
                  (cond-> sel
                    (not= :chain (:coll/type sel))
                    (-> (nav/parents-vec)
                        (peek)
                        :coll/_contains
                        (first)))
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

(defonce set-scroll-user
  (fn [ev]
    (vswap! core/scroll-sequence-number inc)))

(defn ^:dev/before-load stop []
  (js/console.log "stop")
  (let [ls (:listeners (meta the-singleton-db))]
    (doseq [l (vals @ls)]
      (println "Cleaning listener" (meta l)))
    (reset! ls {}))
  (println "Cleared DB listeners")
  (swap! the-app
         #(-> %
              (sk/cleanup!)
              (sm/cleanup!))))

(defn ^:dev/after-load init
  []
  #_(some-> the-singleton-db meta :listeners (reset! {}))
  (js/document.addEventListener "scroll" set-scroll-user true)
  (let [{:keys [conn bus] :as app} (setup-app the-singleton-db)
        el (.getElementById js/document "root")]
    (reset! the-app app)
    
    (println "Created new app and reset kbdb.  Mount root...")
    
    (-> (cr/root app code/form)
        (rum/mount el))
    #_(-> (cdoc/root code/form)
        (rum/mount el))))

(defn ^:export become
  [db]
  (-> (fn []
        (rum/unmount (.getElementById js/document "root"))
        (stop)
        (reset! the-singleton-db db)
        (init))
      (js/setTimeout 0))
  "Goodbye")

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
   [sted.comp.demo :as cdemo]
   [sted.cmd.move :as move]
   [sted.cmd.nav :as nav]
   [sted.cmd.insert :as insert]
   [sted.cmd.edit :as edit]
   [sted.cmd.mut :as mut]
   [sted.df.github :as dfg]
   [sted.df.async :as a]
   [sted.sys.eval.core :as eval-core]
   [sted.sys.eval.sci :as eval-sci]
   [sted.sys.eval.sys-eval :as sys-eval]
   [sted.sys.nrepl.bencode :as benc]
   [sted.sys.nrepl.setup :as nrepl]
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


(def test-form-data-bar
  (assoc (e/string->tx-all (m/macro-slurp "src/sted/user.clj"))
    :chain/filename "src/sted/user.clj"))


(def meta-chain*
  [{:db/id "label"
    :token/type :string
    :token/value "Keyboard"}
   {:db/ident ::default-keymap
    :db/id "defaultkeymap"
    :coll/type :keyboard
    :keymap/bindings (for [[k m] skm/default]
                       {:key/kbd k  :key/mutation m})}
   {:db/ident :messages
    :db/id "messages"
    :coll/type :ol}
   {:db/ident ::command-chain
    :db/id "command-chain"
    :coll/type :ol
    :form/highlight true}
   {:db/ident ::inspect
    :db/id "inspect"
    :coll/type
    #_:demo
    :vec}])


(def init-tx-data
  (let [chains [test-form-data-bar]]
    (into meta-chain*
          [
           ;;
           {:db/ident ::state
            :state/bar "bar"
            :state/limit 128}
           ;;
           (merge {:db/id "bar" :coll/type :bar}
                  (ec/seq-tx
                   (concat
                    ;; chains first
                    (for [ch chains]
                      (assoc ch
                             :coll/type :chain
                             :coll/_contains "bar"))
                    ;; meta chain at the end
                    [(-> {:db/ident ::meta-chain
                          :coll/type :chain
                          :coll/_contains "bar"}
                         ;; populate it
                         (assoc :coll/contains (into #{} (map :db/id) meta-chain*))
                         (merge (ec/seq-tx (map :db/id meta-chain*))))])))])))


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
  (or (when-let [require (aget js/window "secret_electron_require")]
        (let [^js fs (require "fs/promises")
              hack (str path "##")]
          (a/let [_ (.writeFile fs hack contents)
                  _ (.rename fs hack path)]
            (js/console.log "wrote " path (count contents) "bytes")
            {:path path :size (count contents)})))
      (do (ask-download-file path contents)
          (swap! ml/save-status assoc :status :ok :file path))))


(defonce ^:export the-app (atom nil))


(defn message-tx
  [db msg]
  (let [m {:db/id "msg"
           :token/type :string
           :token/value msg}]
    (into [m] (edit/form-cons-tx m (d/entity db :messages)))))


(defn setup-app
  ([] (setup-app (doto (d/create-conn s/schema) (d/transact! init-tx-data))))
  ([conn]
   (let [zb (some-> the-app
                    deref
                    :bus
                    (core/reset))
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
         (eval-core/setup!)
         (nrepl/setup!)
         (sys-eval/setup!)
         (core/register-mutation!
          :save
          (fn [_ db bus]
            (let [sel (get-selected-form db)
                  chain (cond-> sel
                          (not= :chain (:coll/type sel))
                          (-> (nav/parents-vec)
                              (peek)
                              :coll/_contains
                              (first)))
                  file (or (:chain/filename chain) "noname.clj")]
              (when chain
                (a/let [tstart (js/performance.now)
                        {:keys [path size]} (save* file (e/->string chain))
                        tend (js/performance.now)]
                  (d/transact! conn
                               (message-tx @conn
                                           (str size
                                                " b " (js/Math.round (- tend tstart))
                                                " ms " path
                                                " @ " (.toString (:max-tx db) 16)
                                                " @ " (.toString (js/Date.)))))))
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


(defonce set-scroll-user (fn [ev] (vswap! core/scroll-sequence-number inc)))

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


(rum/defc eye-test
  []
  (let [halfw 16
        w (* 2 halfw)
        halfh 10
        h (* 2 halfh)
        inset 2
        ;; control vertical extent
        ch 12
        ;; control pointiness
        cw 8]
    [:div {:style {:display :flex
                   :flex-wrap :wrap
                   ;; :width "300px"
                   :gap "1em"
                   :padding "1em"}}
     (for [ch (range 10 14)
           cw (range 2 10)]
       [:div {:style {:display :flex
                      :flex-direction "column"}}
        [:svg {:width "20em"
               :viewBox (str (- (+ halfw inset)) " " (- halfh) " " (+ w inset inset) " " h)
               :stroke "#fff"
               :fill "none"
               :stroke-width "2"
               :style {:display "block"
                       :outline "1px dashed #777"}}
         [:circle {:x 0 :y 0 :r (* 0.3 halfw)}]
         [:path {:d (str "M" (- halfw) " " 0
                         " C" (- cw) " " (- ch) " " cw " " (- ch) " " halfw " " 0
                         " C" cw " " ch " " (- cw) " " ch " " (- halfw) " " 0
                         " Z")}]]
        [:div {:style {:margin :auto}} (str ch ", " cw)]])]))


(defn ^:dev/after-load init
  []
  #_(some-> the-singleton-db meta :listeners (reset! {}))
  (js/document.addEventListener "scroll" set-scroll-user true)
  (let [{:keys [conn bus] :as app} (setup-app the-singleton-db)
        el (.getElementById js/document "root")]
    
    (reset! the-app app)
    #_(println "Created new app and reset kbdb.  Mount root...")
    (println "App created" (core/uniqueid bus) "Mount root...")
    (reset! core/pending-reload true)
    (-> (cr/root app code/form)
        (rum/mount el))
    
    #_(-> (cdemo/root code/form)
        (rum/mount el))
    
    #_(-> (eye-test)
          (rum/mount el))
    
    #_(-> (cdoc/root code/form)
          (rum/mount el))
    
    ;; 
    (reset! core/pending-reload nil)
    ))


(defn ^:export become
  [db]
  (-> (fn []
        (rum/unmount (.getElementById js/document "root"))
        (stop)
        (reset! the-singleton-db db)
        (init))
      (js/setTimeout 0))
  "Goodbye")

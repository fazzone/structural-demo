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
   [sted.comp.scroll :as scroll]
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
   [sted.sys.eval.sci :as eval-sci]
   [sted.sys.kbd :as sk]
   [sted.cmd.insert :as insert]
   [sted.cmd.edit :as edit]
   [sted.cmd.mut :as mut]
   [sted.cmd.invar :as invar]
   [sted.df.github :as dfg]
   [sted.df.async :as a]
   [zprint.core :as zp-hacks]
   [sted.core :as core :refer [get-selected-form
                               move-selection-tx]])
  (:import
   [goog.net XhrIo]
   [goog.net EventType])
  (:require-macros
   [cljs.core.async.macros :refer [go
                                   go-loop]]
   [sted.macros :as m]))

(def load-time (js/Date.now))

(def test-form-data-bar
  '[["Chain 1"
     (def thing
       [1 (+ 2 3 foo) [:a :c] "ok"])
     (nav hn/stories :topstories (:topstories stories))
     (defn open-file
       [path]
       (then (slurp path)
             (fn [text]
               (send! [:open-chain text {:chain/filename path}]))))
     (defn open-url
       [u]
       (then (fetch-text u)
             (fn [text] (send! [:open-chain text]))))
     (new-window "http://localhost:8087?title=SH"
                 "_blank"
                 "top=500,left=3000")
     (open-url "https://raw.githubusercontent.com/fazzone/structural-demo/master/src/page.cljs")
     ^:form/highlight (open-file "src/page.cljs")
     (open-file "src/cmd/mut.cljc")
     (open-file "src/embed.cljc")
     ()
     (defn get-repo [ident ref-name]
       (a/let [ref (fetch-json (str "https://api.github.com/repos/" ident "/git/ref/heads/" ref-name))
             commit (fetch-json (-> ref :object :url))
             tree (fetch-json (str (-> commit :tree :url) "?recursive=true"))]
       (ingest tree)))
     (get-repo "fazzone/structural-demo" master)
     (defn explore-tree
       [{:keys [tree]  :as t}]
       (let [by-type (group-by :type tree)]
         (a/let [rec (js/Promise.all
                      (into-array
                       (for [{:keys [url path] :as st} (by-type "tree")]
                         (a/let [f (fetch-json url) sf (explore-tree f)] [path sf]))))]
           (into (sorted-map)
                 (concat (for [{:keys [path] :as b} (by-type "blob")] [path b])
                         rec)))))]])

(def init-tx-data
  (let [chains (concat
                 #_[(e/string->tx-all (m/macro-slurp "src/core.cljc"))]
                 #_[(e/string->tx-all (m/macro-slurp "src/cmd/edit.cljc"))]
                 #_(map e/->tx test-form-data-bar)
                 [(e/->tx [^:form/highlight ()])]
                 #_[(e/string->tx-all (m/macro-slurp "subtree/input.clj"))])]
    [{:db/ident ::state  :state/bar "bar"}
     {:db/ident ::command-chain  :db/id "command-chain"  :coll/type :vec}
     {:db/ident ::inspect  :db/id "inspect"  :coll/type :inspect}
     {:db/ident ::default-keymap
      :db/id "defaultkeymap"
      :coll/type :keyboard
      :keymap/bindings (for [[k m] sk/default-keymap]
                         {:key/kbd k  :key/mutation m})}
     (assoc (e/seq-tx
              (concat
                (for [ch chains]
                  (assoc ch
                    :coll/type :chain
                    :coll/_contains "bar"))
                #_[{:db/ident ::meta-chain
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

(comment
  [:span.c.dl
   {}
   [:span.d
    #_[:span.inline-tag-outer [:span.inline-tag-inner (subs (str (:db/id e)) 0 1)]]
    #_(when-let [p (get proply (:db/id e))]
        [:span.inline-tag-outer
         [:span.inline-tag-inner
          (str p)]])
    #_[:span.inline-tag.debug
       (str (swap! render-counter inc))
       #_(str (:db/id e))]
    open]
   "..."
   [:span.d.cl close]])

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

#_(defmethod display-coll :alias [{:alias/keys [of] :as c} b i s]
  (println "DCA" c)
  [:div.alias
   (when s {:class s :ref "selected"})
   [:div.form-title "Alias " (:db/id c) " of " (:db/id of)]
   (fcc of b i s)])

(declare setup-app)

(rum/defc test-image
  []
  (let [cell-width 8
        cell-height 4
        rows 10
        cols 10
        height (* rows cell-height)
        width (* cols cell-width)]
    [:svg
     {:viewBox (str "0 0 " width " " height)
      :style {:width "800px"  :border "1px solid aliceblue"}}
     [:g {:stroke-width (/ cell-width 32)  :stroke "#fff"}
      (for [i (range cols)]
        [:line {:x1 (* i cell-width)  :y1 0  :x2 (* i cell-width)  :y2 height}])
      (for [i (range cols)]
        [:line {:x1 0  :y1 (* i cell-height)  :x2 width  :y2 (* i cell-height)}])]]))

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
      (let [kbd (sk/event->kbd ev)
            bindings sk/default-keymap
            mut (get bindings kbd)]
        (core/send! @keyboard-bus [:kbd kbd tkd])
        (when (some? mut) (.preventDefault ev) (.stopPropagation ev))))))

(defonce global-keydown
  (fn [ev]
    (global-keydown* ev)))

(defn global-keyup*
  [ev]
  #_(prn "Keyup" (event->kbd ev)))

(defonce global-keyup
  (fn [ev]
    (global-keyup* ev)))

#_(def ensure-selected-in-view!
  (-> (fn [] (scroll/scroll-to-selected! false))
      (gfunc/debounce 10)))

(defn save*
  [file contents]
  (let [spit (some-> (aget js/window "my_electron_bridge")
                     (aget "spit"))]
    (println "Spit=" spit)
    (when spit
      (let [pr (spit file contents)]
        (prn pr)
        (-> pr
         (.then  (fn [] (swap! ml/save-status assoc :status :ok :file file)))
         (.catch (fn [] (swap! ml/save-status assoc :status :error))))))))

(defn setup-app
  ([] (setup-app (doto (d/create-conn s/schema) (d/transact! init-tx-data))))
  ([conn]
   (let [a (core/app conn)]
     (doseq [[m f] mut/movement-commands]
       (core/register-simple! a m (core/movement->mutation f)))
     (doseq [[m f] mut/editing-commands]
       (core/register-simple! a m f))
     (doto a
       (core/register-mutation! :kbd
                                (fn [[_ kbd tkd] db bus]
                                  (when-let [mut (:key/mutation (d/entity db [:key/kbd kbd]))]
                                    (core/send! bus [mut]))))
       #_(core/register-mutation! :scroll (fn [_ db _] (scroll/scroll-to-selected!)))
       (core/register-mutation! :eval-sci (eval-sci/mutatef a))
       (core/register-simple!
        :zp
        (fn [db _]
          (let [_ (js/console.time "formatting")
                _ (js/console.time "preparing")
                _ (zp-hacks/set-options! {:map {:sort? nil}})
                q (-> (get-selected-form db)
                      nav/parents-vec
                      peek)
                _ (js/console.timeEnd "preparing")
                _ (js/console.time "stringifying")
                my-string (e/->string q)
                _ (js/console.timeEnd "stringifying")
                _ (js/console.time "zprint")
                p (zp-hacks/zprint-file-str my-string (:db/id q))
                _ (js/console.timeEnd "zprint")
                _ (js/console.time "parsing")
                pt (e/string->tx p)
                _ (js/console.timeEnd "parsing")
                _ (js/console.time "reconciling")
                ans (vec
                     (mapcat (fn [a b]
                               (when-not
                                   (and (= (:coll/type a) (:coll/type b))
                                        (= (:token/type a) (:token/type b))
                                        (= (:token/value a) (:token/value b)))
                                   (println "!!!! cannot reconcile !!! ")
                                   (println "A:")
                                   (println my-string)
                                   (println "B:")
                                   (println p)
                                   (throw (ex-info "cannot reconcile " {})))
                               (for [k [:form/indent :form/linebreak]
                                     :when (not= (k a) (k b))]
                                 (if (k b)
                                   [:db/add (:db/id a) k (k b)]
                                   [:db/retract (:db/id a) k (k a)])))
                             (tree-seq :coll/type e/seq->vec q)
                             (tree-seq :coll/type e/seq->vec pt)))]
            (js/console.timeEnd "reconciling")
            (js/console.timeEnd "formatting")
            ans)
          #_(scroll-to-selected!)))
       (core/register-mutation!
        :save
        (fn [_ db bus]
          (let [sel (get-selected-form db)
                chain (-> sel
                          (nav/parents-vec)
                          (peek)
                          :coll/_contains
                          first)
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

(defn ^:dev/after-load init
  []
  (js/document.removeEventListener "keydown" global-keydown true)
  (js/document.removeEventListener "keyup" global-keyup true)
  (js/document.addEventListener "keydown" global-keydown true)
  (js/document.addEventListener "keyup" global-keyup true)
  (some-> the-singleton-db meta :listeners (reset! {}))
  (let [{:keys [conn bus]} (setup-app the-singleton-db)]
    (reset! keyboard-bus bus)
    (when-let [req-title (some-> js/window.location.search
                                 (js/URLSearchParams.)
                                 (.get "title"))]
      (set! js/document.title req-title))
    (rum/mount #_(debug-component)
               (root-component @conn bus)
               (.getElementById js/document "root"))))

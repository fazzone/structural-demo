(ns page
  (:require
   [clojure.edn :as edn]
   [embed :as e]
   [schema :as s]
   [debug :as debug]
   [tx-history :as h]
   [goog.string :as gstring]
   [goog.functions :as gfunc]
   [datascript.core :as d]
   [clojure.datafy :as datafy]
   [clojure.string :as string]
   [rum.core :as rum]
   [comp.scroll :as scroll]
   [cljs.core.async :as async]
   [sci.core :as sci]
   [db-reactive :as dbrx]
   [comp.cons :as cc]
   [comp.edit-box :as eb]
   [comp.search]
   [comp.code :as code]
   [comp.common :as ccommon]
   [comp.modeline :as ml]
   [cmd.move :as move]
   [cmd.nav :as nav]
   [sys.eval.sci :as eval-sci]
   [cmd.insert :as insert]
   [cmd.edit :as edit]
   [cmd.mut :as mut]
   [cmd.invar :as invar]
   [df.github :as dfg]
   [df.async :as a]
   [zprint.core :as zp-hacks]
   [core :as core
    :refer [get-selected-form
            move-selection-tx]])
  (:import
   [goog.net XhrIo]
   [goog.net EventType])
  (:require-macros
   [cljs.core.async.macros :refer [go
                                   go-loop]]
   [macros :as m]))

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

(def default-keymap
  {"f"   :flow-right
   "S-^" :new-meta
   "S-F" :flow-right-coll
   "u"   :undo
   "S-A" :alias
   "g"   :gobble
   "S-I" :zp
   "o"   :offer
   ";"   :new-comment
   "S-S" :split
   "M-s" :splice
   "S-Z" :drag-left
   "S-X" :drag-right
   "n"   :find-next
   "S-N" :find-first
   "C-/" :undo
   "S-R" :reify-undo
   "S-_" :uneval
   "S-W" :save
   "t"   :tear
   "S-@" :new-deref
   "a"   :flow-left
   "w"   :float
   "s"   :sink
   ;; "S-H"       :toplevel
   "h"         :parent
   "j"         :next
   "k"         :prev
   "l"         :tail
   "r"         :raise
   " "         :insert-right
   "S- "       :insert-left
   "d"         :delete-right
   "S-H"       :hoist
   "Backspace" :delete-left
   ;; "Backspace" :move-to-deleted-chain
   "Enter"     :linebreak
   "C-Enter"   :insert-right-newline
   "Escape"    :select-chain
   "c"         :clone
   "z"         :hop-left
   "x"         :hop-right
   "9"         :wrap
   ;; "9"         :new-list
   "0"     :parent
   "]"     :parent
   "p"     :slurp-right
   "S-P"   :barf-right
   "Tab"   :indent
   "S-Tab" :dedent
   "e"     :eval-sci
   "S-("   :new-list
   "["     :new-vec
   "S-C"   :new-chain
   "S-B"   :new-bar "'" :new-quote
   "1"     :m1
   "2"     :m2
   "3"     :m3
   "4"     :m4
   "5"     :m5
   "6"     :m6
   "7"     :m7
   "8"     :m8
   "v"     :scroll
   "-"     :hide
   "i"     :insert-left
   "S-Q"   :stringify
   "S-+"   :plus})

(def init-tx-data
  (let [chains (concat
                #_[(e/string->tx-all (m/macro-slurp  "src/core.cljc"))]
                [(e/string->tx-all (m/macro-slurp  "src/cmd/edit.cljc"))]
                (map e/->tx test-form-data-bar)
                #_[(e/string->tx-all (m/macro-slurp  "subtree/input.clj"))])]
    [{:db/ident ::state
      :state/bar "bar"}
     {:db/ident ::command-chain
      :db/id "command-chain"
      :coll/type :vec}
     {:db/ident ::inspect
      :db/id "inspect"
      :coll/type :inspect}
     {:db/ident ::default-keymap
      :db/id "defaultkeymap"
      :coll/type :keyboard
      :keymap/bindings (for [[k m] default-keymap]
                         {:key/kbd k :key/mutation m})}
     (assoc
      (e/seq-tx
       (concat
        (for [ch chains]
          (assoc ch
                 :coll/type :chain
                 :coll/_contains "bar"))
        [{:db/ident ::meta-chain
          :coll/type :chain
          :coll/_contains "bar"
          :coll/contains #{"label"
                           "defaultkeymap"
                           "inspect"
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

(def special-key-map
  (merge
   {" "      [::insert-editing :after]
    "S-\""   [::insert-editing :after "\""]
    "S-:"    [::insert-editing :after ":"]
    "S- "    [::insert-editing :before]
    "'"      [::insert-editing :before "'"]
    "S-("    [::edit-new-wrapped :list]
    "M-S-("  [::wrap-and-edit-first :list]
    "q"      [::wrap-and-edit-first :list]
    "9"      [::wrap-selected-form :list]
    "["      [::edit-new-wrapped :vec]
    "S-{"    [::edit-new-wrapped :map]
    "r"      [::raise-selected-form]
    "S-X"    [::extract-to-new-top-level]
    "Escape" [::select-chain]
    "m"      [::edit-selected]
    "]"      [::move :move/up]
    "w"      [::exchange-with-previous]
    "s"      [::exchange-with-next]
    "0"      [::move :move/up]
    "v"      [::scroll-into-view]
    "h"      [::move :move/up]
    "j"      [::move :move/next-sibling]
    "k"      [::move :move/prev-sibling]
    "l"      [::move :move/most-nested]
    "f"      [::move :move/flow]
    "a"      [::move :move/back-flow]
    "n"      [::move :move/most-nested]
    "d"      [::delete-with-movement :move/forward-up]
    "e"      [::eval]
    "z"   [::hop-left]
    "x"   [::hop-right]
    "S-Z" [::drag-left]
    "Backspace" [::delete-with-movement :move/backward-up]
    "c"         [::duplicate-selected-form]
    ;; "i"         [::indent-form 1]
    ;; "S-I"       [::indent-form -1]
    "Tab"       [::indent-form 1]
    "S-Tab"     [::indent-form -1]
    "i"         [::check-invariants] #_[::reset-indent]
    "Enter"     [::linebreak-form]
    "M-p"       ["placeholder"]
    "M-n"       ["placeholder"]
    "-"         ["placeholder"]
    "S-Q"         ["placeholder"]
    "S-M"       [::recursively-set-indent true]
    "S-O"       [::recursively-set-indent false]
    ;; "M-x"       [::execute-selected-as-mutation]
    "S-A"       {"a" [::linebreak-form]}
    "S-R"       {"f" [::reify-extract-selected]
                 "m" [::reify-last-mutation]
                 "p" [::reify-parse-selected]}
    "C-z"       [::revert-last]}
   (into {}
         (for [i (range  8)]
           [(str (inc i)) [::select-1based-nth-reverse-parent (inc i)]]))))

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
     #_(rum/bind-context [ccommon/*modeline-ref* ml-ref]
                       (code/form (:state/bar state) bus 0 nil))
     (code/form (:state/bar state) bus 0 nil)
     [:div.modeline-outer {:id "modeline"
                           :ref ml-ref}]
     #_(ml/modeline-portal db bus)
     #_(cc/svg-viewbox (:state/bar state) core/blackhole)]))

(defn event->kbd
  [^KeyboardEvent ev]
  (str (when (.-altKey ev) "M-")
       (when (.-ctrlKey ev)
         "C-")
       (when (.-shiftKey ev) "S-")
       (.-key ev)))

(def keyboard-bus (atom nil))

(defn global-keydown*
  [ev]
  (let [tkd (js/performance.now)]
    (when (identical? js/document.body js/document.activeElement)
      (let [kbd (event->kbd ev)
            bindings default-keymap
            mut (get bindings kbd)
            ;; _ (println "Kbd" kbd :mut mut)
            ]
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

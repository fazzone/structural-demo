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
   [cljs.core.async :as async]
   [sci.core :as sci]
   [db-reactive :as dbrx]
   [comp.cons :as cc]
   [comp.edit-box :as eb]
   [comp.search]
   [comp.code :as code]
   [comp.modeline :as ml]
   [cmd.move :as move]
   [cmd.nav :as nav]
   [sys.eval.sci :as eval-sci]
   [cmd.insert :as insert]
   [cmd.edit :as edit]
   [cmd.mut :as mut]
   [cmd.invar :as invar]
   #_[df.github :as dfg]
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

(rum.core/set-warn-on-interpretation! true)

(def load-time (js/Date.now))

(def test-form-data-bar
  '[["Chain 1"
     (def thing
       [1 (+ 2 3 foo) [:a :c] "ok"])
     (defn hn-test
       []
       (ingest (then (nav stories :topstories (:topstories stories))
                     (fn [x] (nav x x (first x))))))
     (defn open-file
       [path]
       (then (slurp path)
             (fn [text]
               (send! [:open-chain text {:chain/filename path}]))))
     (defn open-url
       [u]
       (then (fetch-text u)
             (fn [text] (send! [:open-chain text]))))
     (open-url "https://raw.githubusercontent.com/fazzone/structural-demo/master/src/page.cljs")
     ^:form/highlight (open-file "src/macros.clj")]])

(def default-keymap
  {"f"   :flow-right
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
   "Backspace" :move-to-deleted-chain
   "Enter"     :linebreak
   "C-Enter"   :insert-right-newline
   "Escape"    :select-chain
   "c"         :clone
   "z"         :hop-left
   "x"         :hop-right
   "q"         :compose
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
   "S-B"   :new-bar
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
                #_[(e/string->tx-all (m/macro-slurp  "src/cmd/edit.cljc"))]
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

(rum/defc root-component
  [db bus]
  (let [state (d/entity db ::state)]
    [:div.bar-container
     (code/form (:state/bar state) bus 0 nil)
     (ml/modeline-portal db bus)
     #_(cc/svg-viewbox (:state/bar state) core/blackhole)]))

(defn event->kbd
  [^KeyboardEvent ev]
  (str (when (.-altKey ev) "M-")
       (when (.-ctrlKey ev)
         "C-")
       (when (.-shiftKey ev) "S-")
       (.-key ev)))

(def initial-compose-state {:bindings special-key-map :compose nil})

(def key-compose-state (atom initial-compose-state))

(def keyboard-bus (atom nil))

(defn global-keydown*
  [ev]
  (let [tkd (js/performance.now)]
    (when-not @eb/global-editing-flag
      (let [kbd (event->kbd ev)
            {:keys [bindings compose]} @key-compose-state
            mut (get bindings kbd)
            next-kbd (conj (or compose []) kbd)]
        (core/send! @keyboard-bus [:kbd kbd tkd])
        (when (or (some? mut)
                  (and compose (nil? mut)))
          (.preventDefault ev)
          (.stopPropagation ev))))))

(defonce global-keydown
  (fn [ev]
    (global-keydown* ev)))

(defn global-keyup*
  [ev]
  #_(prn "Keyup" (event->kbd ev)))

(defonce global-keyup
  (fn [ev]
    (global-keyup* ev)))

(def ^:const scroll-hysteresis-px 32)

(defn scroll-1d
  [size h pos off]
  (let [align-bottom (- off (- size h))
        top-closer? (< (js/Math.abs (- pos off))
                       (js/Math.abs (- pos align-bottom)))
        [best other] (if (or top-closer? (> h size))
                       [off align-bottom]
                       [align-bottom off])]
    (if-not (< (- scroll-hysteresis-px) (- pos best) scroll-hysteresis-px)
      (int best))))

(defn scroll-to-selected!
  ([] (scroll-to-selected! true))
  ([always]
   (let [#_          #_el (js/document.querySelector ".selected")
         [el & more] (js/document.querySelectorAll ".selected")
         ;; _ (prn "More" more)
         tl    (some-> el (.closest ".form-card"))
         chain (some-> el (.closest ".chain"))
         bar   (some-> chain (.closest ".bar"))
         chain-height (some-> chain (.-clientHeight))
         bar-width    (some-> bar (.-clientWidth))
         ;; fit the entire toplevel if we can, otherwise just the selection
         tlh  (some-> tl (.getBoundingClientRect) (.-height) (js/Math.ceil))
         elh  (some-> el (.getBoundingClientRect) (.-height) (js/Math.ceil))
         vst  (if (< tlh chain-height) tl  el)
         h    (if (< tlh chain-height) tlh elh)
         vpos (some-> chain (.-scrollTop))
         voff (some-> vst (.-offsetTop))
         new-chain-top (and tl
                            chain
                            #_(< h chain-height)
                            (or always
                                (not (< vpos voff (+ h voff)
                                        (+ vpos chain-height))))
                            (scroll-1d chain-height h vpos voff))
         w    (some-> chain (.getBoundingClientRect) (.-width) (js/Math.ceil))
         hpos (some-> bar (.-scrollLeft))
         hoff (some-> chain (.-offsetLeft))
         new-bar-left (and bar
                           (or always
                               (not (< hpos hoff (+ w hoff)
                                       (+ hpos bar-width))))
                           (scroll-1d bar-width w hpos hoff))]
     #_(js/console.log "Tl" tl "Chain" chain "Bar" bar)
     #_(println "================================Scroll"
                "\nChain-height" chain-height
                "\nBar-width" bar-width
                "\nTLH" h
                "vpos" vpos
                "voff" voff
                "\nCHW" w
                "hpos" hpos
                "hoff" hoff
                "\nCan fit?" (< h chain-height)
                "\nAlready visible?" (not (< vpos voff (+ h voff)
                                             (+ vpos chain-height)))
                "\nAlways scroll?" always)
     (when (> h chain-height)
       (println "Too bigby " (- h chain-height) h chain-height)
       (println "NCT" new-chain-top))
     (when new-chain-top (.scrollTo chain #js {:top new-chain-top}))
     (when new-bar-left  (.scrollTo bar   #js {:left new-bar-left})))))

(def ensure-selected-in-view!
  (-> (fn [] (scroll-to-selected! false))
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
       (core/register-mutation! :scroll (fn [_ db _] (scroll-to-selected!)))
       (core/register-mutation! :form/highlight (fn [_ _ _] (js/window.setTimeout ensure-selected-in-view! 1)))
       (core/register-mutation! :form/edited-tx (fn [_ _ _] (js/window.setTimeout ensure-selected-in-view! 1)))
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

(comment
  (letfn [(pubsub []
            (let [subs (js/Set.)]
              {:sub (fn [f]
                      (.add subs f)
                      (fn [] (.delete subs f)))
               :pub (fn [m]
                      (.forEach subs (fn [f] (f m))))}))]
    (let [{:keys [pub sub]} (pubsub)
          unsub (sub (fn [m] (println "Messg" m)))
          _ (pub "Message")
          _ (unsub)
          _ (pub "M@")])))

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
  (let [{:keys [conn bus]} (setup-app the-singleton-db)]
    (reset! keyboard-bus bus)
    (when-let [req-title (some-> js/window.location.search
                                 (js/URLSearchParams.)
                                 (.get "title"))]
      (set! js/document.title req-title))
    (rum/mount #_(debug-component)
               (root-component @conn bus)
               (.getElementById js/document "root"))))

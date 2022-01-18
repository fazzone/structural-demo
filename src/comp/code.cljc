(ns comp.code
  (:require
   [embed :as e]
   [schema :as s]
   [goog.string :as gstring]
   [goog.functions :as gfunc]
   [datascript.core :as d]
   [clojure.datafy :as datafy]
   [clojure.string :as string]
   [rum.core :as rum]
   [cljs.core.async :as async]
   [sci.core :as sci]
   [db-reactive :as dbrx]
   [comp.edit-box :as eb]
   [comp.scroll :as scroll]
   [cmd.move :as move]
   [cmd.nav :as nav]
   [cmd.mut :as mut]
   [comp.common :as cc]
   [core :as core
    :refer [get-selected-form
            move-selection-tx]]
   [comp.keyboard]
   [comp.inspect]))

(declare form)

(defn computed-indent
  [e indent-prop]
  (+ indent-prop
     (or (:form/indent e)
         0)))

(rum/defc top-level-form
  ;; < dbrx/ereactive
  [e bus p]
  [:div.form-card
   {}
   #_[:div.form-title.code-font {:style {:margin-bottom "4ex"}} (str "#" (:db/id e) " T+" (- (js/Date.now) load-time) "ms")]
   [:div.top-level-form.code-font ^:inline (form e bus 0 p)]
   [:div.modeline-outer {:id (cc/modeline-portal-id (:db/id e))}]])

(rum/defc indenter
  [nl? ip fi]
  (if (and (or (nil? fi) (zero? fi)) (not nl?))
    nil
    [:s.indent-chars (if-not nl? {} {:class "nl"})
     (when nl? "\n")
     (when fi
       #_[:span.indenter
        {:style {:color "cadetblue"}}
        (apply str (repeat fi "-"))]
       [:span.indenter {:style {:margin-left (str fi "ch")}}])]))

(def render-counter (atom 0))

(rum/defc erc
  < rum/reactive
  [{:eval/keys [of out]  :as e} bus classes]
  (let [[result] (e/seq->vec e)]
    [:div.eval-result {:class classes} #_(form result bus 0 (first ()))
     (:token/value result)
     [:div "Result of "
      [:a.eval-result-ref
       {:on-click (fn [] (core/send! bus [:select (:db/id of)]))}
       (str "#" (:db/id (:eval/of e)))]]]))

(rum/defc chain
  [ch bus classes]
  [:div.chain.hide-scrollbar
   {:key (:db/id ch)
    :class classes
    :id (str "c" (:db/id ch))}
   (for [f (e/seq->vec ch)]
     (-> (top-level-form f bus nil)
         (rum/with-key (:db/id f))))])

(rum/defc bar [b bus classes]
  [:div.bar.hide-scrollbar
   {:class classes}
   (for [chain-head (e/seq->vec b)]
     (-> (form chain-head bus 0 nil)
         (rum/with-key (:db/id chain-head))))])

(def dispatch-coll
  {:keyboard comp.keyboard/keyboard-diagram
   ;; :inspect comp.inspect/inspect
   :eval-result erc
   :bar bar
   :chain chain})

(rum/defc any-coll
  [e classes bus indent proply]
  (let [ct (:coll/type e)
        coll-class (case ct
                     (:list :vec :map :set :fn :tear) "dl"
                     nil)
        extra-class (case ct
                      :uneval "unev"
                      (:deref :quote
                              :syntax-quote :unquote
                              :unquote-splicing :reader-macro)
                        "pf"
                      nil)
        open-delim (e/open-delim ct)
        close-delim (e/close-delim ct)]
    (if-not (or coll-class extra-class open-delim close-delim)
      ^:inline ((get dispatch-coll ct (constantly nil)) e bus classes)
      (let [children (e/seq->vec e)]
        [:span {:class ["c" coll-class extra-class classes]}
         #_[:span.inline-tag.debug (str (swap! render-counter inc))
            #_(str (:db/id e))]
         (when-some [p (get proply (:db/id e))]
           [:span.inline-tag-outer [:span.inline-tag-inner ^String (str p)]])
         (cond extra-class [:span.d.pfc ^String open-delim]
               open-delim [:span.d ^String open-delim]
               :else nil) (for [c children] ^:inline (form c bus indent proply))
         (when close-delim [:span.d ^String close-delim])]))))

(defn token-class
  [t v]
  (case t
    :symbol         (case v
                      ("defn" "let" "when" "and" "or" "if" "do" "for" "some->"
                       "when-not" "if-not" "def" "cond" "case" "->" "->>" "some->>"
                       "if-let" "when-let" "recur" "try" "catch" "nil" "defmacro")
                      "m"
                      ("first" "map" "filter" "apply" "reset!" "swap!"
                       "get" "assoc" "update" "cons" "conj" "seq" "next"
                       "prn" "println" "into" "set" "vector"
                       "take" "drop" "take-while" "drop-while" "reduce"
                       "concat")
                      "s"
                      "v")
    :keyword        "k"
    (:string :char) "l"
    :number         "n"
    :regex          "re"
    :verbatim       "verbatim"
    :comment        "comment"))

(defn token-text
  [t v]
  #_(subs xxx 0 (if (number? v)
                (count (str v))
                (count v)))
  (case t
    :symbol v
    :keyword  v #_(let [is (string/index-of k "/")]
                    (if (> 0 is)
                      k
                      (rum/fragment
                       [:span.kn (subs k 0 is)]
                       (subs k is))))
    :string (pr-str v)
    :verbatim v
    :number (str v)
    :comment v
    :char v
    :regex (str "REGEX:" v)))

(def scroll-selected
  {:did-update
   (fn [state]
     (when (some-> state :rum/args first :form/highlight)
       #_(println "Do the scroll "
                (some-> state :rum/args first (e/->string)))
       (let [el (rum/dom-node state)
             real-el (if-not (= "S" (.-tagName el) )
                       el
                       (.-nextElementSibling el))]
         (scroll/scroll-to-selected* real-el false)))
     state)})

(rum/defc form
  < dbrx/ereactive scroll-selected
  {:key-fn (fn [e b i p] (:db/id e))}
  [e bus indent-prop proply]
  (let [selected? (:form/highlight e)]
    #_(when selected?
        (rum/use-effect!
         (fn [] (println "Effect#" (:db/id e)))
         [selected?]))
    
    (rum/fragment
     (indenter (:form/linebreak e) indent-prop (:form/indent e))
     (cond (:form/editing e) (eb/edit-box e bus)
           (:token/type e)
           (let [tt (:token/type e)
                 tv (:token/value e)
                 tc (token-class tt tv)
                 it (token-text tt tv)]
             [:span
              {:key (:db/id e)
               ;; :dangerouslySetInnerHTML {:__html it}
               :class (if selected?
                        (str "tk selected " tc)
                        (str "tk " tc))
               :on-click (fn [ev]
                           (.stopPropagation ev)
                           (core/send! bus [:select (:db/id e)]))}
              ^String it])
           (:coll/type e)
           (case (:coll/type e)
             nil (comment
                   "Probably a retracted entity, do nothing")
             (any-coll e
                       (when selected? "selected")
                       bus
                       (+ 2 indent-prop)
                       proply
                       #_(if-not (:form/highlight e)
                           proply
                           (zipmap (map :db/id
                                        (next (mut/get-numeric-movement-vec e)))
                                   (range 2 9)))))))))

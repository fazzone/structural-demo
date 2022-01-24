(ns sted.comp.code
  (:require
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
   [sted.comp.edit-box :as eb]
   [sted.comp.scroll :as scroll]
   [sted.cmd.move :as move]
   [sted.cmd.nav :as nav]
   [sted.cmd.mut :as mut]
   [sted.comp.common :as cc]
   [sted.comp.modeline :as ml]
   [sted.core :as core
    :refer [get-selected-form
            move-selection-tx]]
   [sted.comp.keyboard :as ck]
   [sted.comp.inspect :as ci]))

(rum.core/set-warn-on-interpretation! true)


(declare form)

(defn computed-indent
  [e indent-prop]
  (+ indent-prop
     (or (:form/indent e)
         0)))

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

(rum/defc top-level-form
  ;; < dbrx/ereactive
  [e bus p]
  (let [ml-ref (rum/create-ref)]
    [:div.form-card
     {}
     #_[:div.form-title.code-font {:style {:margin-bottom "4ex"}} (str "#" (:db/id e) " T+" (- (js/Date.now) load-time) "ms")]
     #_[:div.top-level-form.code-font {}
        (rum/bind-context
         [cc/*indenter* indenter]
         (rum/bind-context [cc/*modeline-ref* ml-ref]
                           ^:inline (form e bus 0 p)))]
     [:div.top-level-form.code-font {}
      (rum/bind-context
       [cc/*indenter* indenter]
       ^:inline (form e bus 0 p))]
     
     #_[:div.modeline-outer {:ref ml-ref }]]))

(def render-counter (atom 0))

(rum/defc erc
  < rum/reactive
  [{:eval/keys [of out]  :as e} bus classes]
  (let [[result] (e/seq->vec e)]
    [:div.eval-result {:class classes} #_(form result bus 0 (first ()))
     ^String (:token/value result)
     [:div "Result of "
      [:a.eval-result-ref
       {:on-click (fn [] (core/send! bus [:select (:db/id of)]))}
       (str "#" (:db/id (:eval/of e)))]]]))

#_(def inhibit-scroll? (volatile! false))

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


(rum/defc aliasc [{:alias/keys [of] :as a} bus classes]
  [:div.alias.alternate-reality
   {:class classes}
   [:div.form-title "Alias " (str (:db/id a)) " of " (str (:db/id of))]
   (when of (rum/bind-context [cc/*modeline-ref* nil]
                              ^:inline (form of bus 0 nil)))])

(def dispatch-coll
  {:keyboard ck/keyboard-diagram
   ;; :inspect comp.inspect/inspect
   :eval-result erc
   :bar bar
   :chain chain
   :alias aliasc})

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
         #_[:span.inline-tag.debug
          (str (swap! render-counter inc))
          #_(str (:db/id e))]
         (when-some [p (get proply (:db/id e))]
           [:span.inline-tag-outer [:span.inline-tag-inner ^String (str p)]])
         (cond extra-class [:span.d.pfc ^String open-delim]
               open-delim [:span.d ^String open-delim]
               :else nil)
         (for [c children] ^:inline (form c bus indent proply))
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
    :string v
    :verbatim v
    :number (str v)
    :comment v
    :char v
    :regex (str "REGEX:" v)))

(def the-iobs
  (->> #js {:rootMargin "0px" :threshold #js [0 1]}
       (js/IntersectionObserver.
        (fn [ents me]
          (loop [i 0
                 el nil
                 need-scroll? false]
            
            #_(js/console.log (aget ents i))
            
            (if (= i (alength ents))
              (when need-scroll?
                #_(.unobserve me el)

                (scroll/scroll-to-selected* el))
              (let [^js/IntersectionObserverEntry ioe (aget ents i)
                    tgt (.-target ioe)]
                (recur (inc i) tgt
                       (or need-scroll?
                           (and (.-rootBounds ioe)
                                (> 1 (.-intersectionRatio ioe))
                                #_(not (.-isIntersecting ioe))))))))))))


(def scroll-selected
  {:did-update
   (fn [state]
     (let [sel? (some-> state :rum/args first :form/highlight)
           prev-sel? (some-> state ::prev-sel?)]
       (if-not (or sel? prev-sel?)
         state
         (let [el (rum/dom-node state)
               real-el (if-not (= "S" (.-tagName el) )
                         el
                         (.-nextElementSibling el))]
           (if sel?
             (do (.observe the-iobs real-el)
                 (assoc state ::prev-sel? true))
             (do (.unobserve the-iobs real-el)
                 (assoc state ::prev-sel? nil)))))))})


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
     (rum/with-context [ind cc/*indenter*]
       (when ind
         ^:inline (ind (:form/linebreak e) indent-prop (:form/indent e))))
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
                                   (range 2 9))))))
     
     (when selected? (ml/modeline-nest-next e bus form)))))

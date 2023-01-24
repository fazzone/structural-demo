(ns sted.comp.code
  (:require
   [sted.embed :as e]
   [sted.schema :as s]
   [datascript.core :as d]
   [clojure.string :as string]
   [rum.core :as rum]
   [sted.db-reactive :as dbrx]
   [sted.comp.edit-box :as eb]
   [sted.comp.scroll :as scroll]
   [sted.comp.root :as cr]
   [sted.comp.demo :as cd]
   [sted.cmd.move :as move]
   [sted.cmd.nav :as nav]
   [sted.cmd.mut :as mut]
   [sted.comp.cons :as ccons]
   [sted.comp.common :as cc]
   [sted.comp.modeline :as ml]
   [sted.core :as core
    :refer [get-selected-form
            move-selection-tx]]
   [sted.comp.keyboard :as ck]
   [sted.comp.inspect :as ci]))


(rum.core/set-warn-on-interpretation! true)


(declare form)

(defn iobs-callback
  [ents me]
  (when-not (core/scroll-locked?)
    (let [can-see (some (fn [e]
                          (> 1 (js/Math.abs (- (.-height (.-intersectionRect e)) (.-height (.-boundingClientRect e))))))
                        ents)]
      (when-not can-see
        (loop [[ioe & more] ents
               did-scroll nil]
          #_(when ioe (js/console.log "IR" (.-intersectionRatio ioe) ioe))
          (when ioe
            (if did-scroll
              (js/console.log "What does this mean?")
              (scroll/scroll-to-selected* (.-target ioe) (.-boundingClientRect ioe)))
            (recur more true)))))))


(defn make-iobs
  []
  ;; hack - must specify these in px, ex/em not allowed
  (->> #js {:rootMargin "0px 0px -20px 0px"  :threshold #js [0]}
       (js/IntersectionObserver. iobs-callback)))


(def the-iobs (make-iobs))


(def scroll-selected
  {:did-update
   (fn [state]
     (let [sel? (some-> state :rum/args first :form/highlight)
           prev-sel? (some-> state ::prev-sel?)]
       (when the-iobs
         (if-not (or sel? prev-sel?)
           state
           (let [el (rum/dom-node state)
                 real-el (if-not (= "S" (.-tagName el))
                           el
                           (.-nextElementSibling el))]
             (if sel?
               (do (.observe the-iobs real-el)
                   (assoc state ::prev-sel? true))
               (do (.unobserve the-iobs real-el)
                   (assoc state ::prev-sel? nil))))))))})


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
     {:onClick (fn [^js ev]
                 (when-let [eid (some-> (.-target ev)
                                        (.-dataset)
                                        (aget "eid")
                                        (js/parseInt))]
                   (.stopPropagation ev)
                   (core/send! bus [:click eid ]))
                 false)}
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
     #_[:div.modeline-outer {:ref ml-ref}]]))


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


(defonce secret-chain-scroll-position-cache (atom {}))


(def remember-scroll-position
  "This is for when shadow-cljs reloads us"
  {:will-unmount (fn [state]
                   (let [el (rum/dom-node state)
                         eid (-> state
                                 :rum/args
                                 first
                                 :db/id)]
                     (when eid
                       (swap! secret-chain-scroll-position-cache assoc
                         eid
                         {:top (.-scrollTop el)  :left (.-scrollLeft el)})))
                   state)
   :after-render (fn [state]
                   (if-some [{:keys [top left]} (some->> state
                                                         :rum/args first
                                                         :db/id (get @secret-chain-scroll-position-cache))]
                     (let [el (rum/dom-node state)]
                       (set! (.-scrollTop el) top)
                       (set! (.-scrollLeft el) left)))
                   state)})


(rum/defcs lazy-children
  < rum/reactive
  {:init (fn [state props]
           (assoc state
                  ::phase (atom :lazy)
                  ::end (atom 8)
                  ::last-e (-> state :rum/args first #? (:cljs (js/WeakRef.)))))
   :before-render (fn [st]
                    (when-not (some-> st ::last-e #? (:cljs (.deref))
                                      (identical? (-> st :rum/args first)))
                      (reset! (::phase st) nil))
                    st)
   :after-render (fn [st]
                   (when (= :lazy (deref (::phase st)))
                     (-> #(swap! (::end st) + 8)
                         (js/requestIdleCallback #js {:timeout 10})))
                   st)}
  [{::keys [phase end] :as myst} e bus]
  (let [children (e/seq->vec e)]
    (case @phase
      nil (for [f children]
            (-> (top-level-form f bus nil)
                (rum/with-key (:db/id f))))
      :lazy (let [iend (rum/react end)]
              (when (>= iend (count children))
                (reset! phase nil))
              (println "Lazy children" (:coll/type e) iend)
              (for [f (.slice children 0 (rum/react end))]
                (-> (top-level-form f bus nil)
                    (rum/with-key (:db/id f))))))))


(rum/defc chain
  < remember-scroll-position
  [ch bus classes]
  [:div.chain.hide-scrollbar
   {:key (:db/id ch)
    :class classes
    :id (str "c" (:db/id ch))}
   #_(cd/demo {:form [1 2 3 4]} form)
   #_(lazy-children ch bus)
   (for [f (e/seq->vec ch)]
     (-> (top-level-form f bus nil)
         (rum/with-key (:db/id f))))])


(rum/defc grid
  [ch bus classes]
  [:div.ct-grid
   {:key (:db/id ch)
    :class classes}
   (for [f (e/seq->vec ch)]
     ^:inline (form f bus 0 nil))])

(rum/defc bar
  [b bus classes]
  (let [ref-me (rum/create-ref)]
    [:div.bar.hide-scrollbar {:ref ref-me
                              :class classes}
     
     (rum/use-layout-effect!
      (fn []
        (core/send! bus [:update-bar-ref ref-me])
        (fn cleanup []))
      [bus])
     
     #_(lazy-children b bus)
     (for [chain-head (e/seq->vec b)]
       (-> (form chain-head bus 0 nil)
           (rum/with-key (:db/id chain-head))))]))


(rum/defc aliasc [{:alias/keys [of] :as a} bus classes]
  [:div.alias {:class classes}
   [:div.form-title "Alias " (str (:db/id a)) " of " (str (:db/id of))]
   [:div.alternate-reality {}
    (when of
      (rum/bind-context [cc/*modeline-ref* nil]
                        ^:inline (form of bus 0 nil)))]])


(rum/defc delimited-coll*
  [e bus open close cc ec classes indent proply]
  (let [children (e/seq->vec e)]
    [:span {:class ["c" cc ec classes]}
     #_[:span.inline-tag.debug
      #_(str (swap! render-counter inc))
      (str (:db/id e))]
     ;; requires checking propcs in dbrx shouldcompunentupdate
     #_(when-some [p (get proply (:db/id e))]
       [:span.inline-tag-outer [:span.inline-tag-inner ^String (str p)]])
     (cond ec    [:span.d.pfc ^String open]
           open  [:span.d ^String open]
           :else nil)
     (for [c children] ^:inline (form c bus indent proply))
     (when close [:span.d ^String close])]))


(defn code-coll
  [ct e b c i p]
  (case ct
    :list             (delimited-coll* e b "("  ")" "dl" nil c i p)
    :vec              (delimited-coll* e b "["  "]" "dl" nil c i p)
    :map              (delimited-coll* e b "{"  "}" "dl" nil c i p)
    :set              (delimited-coll* e b "#{" "}" "dl" nil c i p)
    :fn               (delimited-coll* e b "#(" ")" "dl" nil c i p)
    :tear             (delimited-coll* e b "«"  "»" "dl" nil c i p)
    :meta             (delimited-coll* e b "^"  nil  nil "pf" c i p)
    :deref            (delimited-coll* e b "@"  nil  nil "pf" c i p)
    :quote            (delimited-coll* e b "'"  nil  nil "pf" c i p)
    :syntax-quote     (delimited-coll* e b "`"  nil  nil "pf" c i p)
    :unquote          (delimited-coll* e b "~"  nil  nil "pf" c i p)
    :unquote-splicing (delimited-coll* e b "~@" nil  nil "pf" c i p)
    :reader-macro     (delimited-coll* e b "#"  nil  nil "pf" c i p)
    :uneval           (delimited-coll* e b "#_" nil  nil "unev" c i p)
    nil))


(rum/defc hiddenc [{:hidden/keys [coll-type] :as e} bus classes]
  [:div.hidden
   {:class classes}
   [:div {}
    ^:inline (code-coll coll-type e bus classes 0 nil)]
   [:div {:style {:width "1500px"}}
    #_(ccons/testing e core/blackhole)
    (ccons/testing e bus (fn [n] (form n bus 0 nil)))
    #_[:div {:style {:width "800px"}}
     (ccons/svg-viewbox e core/blackhole)]
    #_(cnext/test-image)]
   [:pre
    (with-out-str
      (ccons/my-traversal e)
      #_(ccons/asdf-layout e))]
   [:span {}
    #_(e/open-delim coll-type)
    #_(for [c children] ^:inline (form c bus indent proply))
    #_(e/close-delim coll-type)]])


(rum/defc para
  [ch bus classes]
  [:p {:key (:db/id ch)
       :class classes}
   (for [f (e/seq->vec ch)]
     ^:inline (form f bus 0 nil))])


(rum/defc mdroot
  [ch bus classes]
  [:div {:key (:db/id ch)
         :class (str "md-root prose-font " classes)}
   (for [f (e/seq->vec ch)]
     ^:inline (form f bus 0 nil))])


(rum/defc mdlist
  [ch bus classes]
  [:ul {:key (:db/id ch)
        :class (str "md-list " classes)}
   (for [f (e/seq->vec ch)]
     ^:inline (form f bus 0 nil))])


(rum/defc mdli
  [ch bus classes]
  [:li {:key (:db/id ch)
        :class (str "md-li " classes)}
   (for [f (e/seq->vec ch)]
     ^:inline (form f bus 0 nil))])


(rum/defc mdh
  [ch bus classes]
  [:h2 {:key (:db/id ch)
        :class (str "md-h " classes)}
   (for [f (e/seq->vec ch)]
     ^:inline (form f bus 0 nil))])


(rum/defc mdem
  [ch bus classes]
  [:em {:key (:db/id ch)
        :class (str "md-em " classes)}
   (for [f (e/seq->vec ch)]
     ^:inline (form f bus 0 nil))])


(rum/defc mda
  [a bus classes]
  [:span {:key (:db/id a)
          :class (str "md-link " classes)}
   (for [f (e/seq->vec a)]
     ^:inline (form f bus 0 nil))])


(rum/defc mdbq
  [a bus classes]
  [:span {:key (:db/id a)
          :class (str "md-blockquote " classes)}
   (for [f (e/seq->vec a)]
     ^:inline (form f bus 0 nil))])


(defn any-coll
  [e b c i p]
  (let [ct (:coll/type e)]
    (or
     (code-coll ct e b c i p)
     (case ct
       :chain         (chain e b c i p)
       ;; :inspect       (ci/inspect-portal)

       :demo          (cd/demo* form c) 
       :grid          (grid e b c i p)
       :bar           (bar e b c i p)
       :hidden        (hiddenc e b c i p)
       :keyboard      (ck/keyboard-diagram e b c i p)
       :eval-result   (erc e b c i p)
       :alias         (aliasc e b c i p)
       :md/root       (mdroot e b c i p)
       :md/para       (para e b c i p)
       :md/list       (mdlist e b c i p)
       :md/li         (mdli e b c i p)
       :md/heading    (mdh e b c i p)
       :md/strong     (mdem e b c i p)
       :md/emphasis   (mdem e b c i p)
       :md/link       (mda e b c i p)
       :md/blockquote (mdbq e b c i p)
       (do
         (prn "???????????" ct)
         (str "What are you? " ct))))))


(defn token-class
  [t v]
  (case t
    :symbol         (case v
                      ("defn" "let" "when" "and" "or" "if" "do" "for" "some->"
                       "when-not" "if-not" "def" "cond" "case" "->" "->>" "some->>"
                       "if-let" "when-let" "recur" "try" "catch" "nil" "defmacro")
                      "tk m"
                      ("first" "map" "filter" "apply" "reset!" "swap!"
                       "get" "assoc" "update" "cons" "conj" "seq" "next"
                       "prn" "println" "into" "set" "vector"
                       "take" "drop" "take-while" "drop-while" "reduce"
                       "concat")
                      "tk s"
                      "tk v")
    :keyword        "tk k"
    (:string :char) "tk l"
    :number         "tk n"
    :regex          "tk re"
    :verbatim       "tk verbatim"
    :comment        "comment"
    :md/text        "md"
    :md/code        "md-code code-font"
    :md/inline-code "md-inline-code code-font"))


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
    :string #_v #_(pr-str v) (js/JSON.stringify v)
    :verbatim v
    :number (str v)
    :comment v
    :char v
    :regex (str "REGEX:" v)
    (:md/text :md/code :md/inline-code) v))


#_(defn form-onclick
  [bus ^js ev]
  (.stopPropagation ev)
  (core/send! bus [:click (:db/id e)])
  false)

(rum/defc form
  < dbrx/ereactive scroll-selected
  {:key-fn (fn [e b i p] (:db/id e))}
  [e bus indent-prop proply]
  (let [selected? (:form/highlight e)]
    (rum/fragment
     (rum/with-context [ind cc/*indenter*]
       (when ind
         ^:inline (ind (:form/linebreak e) indent-prop (:form/indent e))))
     (cond (:form/editing e) (eb/edit-box e bus form)
           (:token/type e)
           (let [tt (:token/type e)
                 tv (:token/value e)
                 tc (token-class tt tv)
                 it (token-text tt tv)]
             [:span
              {:key (:db/id e)
               :class (if-not selected?
                        tc
                        (str tc " selected"))
               :data-eid (:db/id e)
               #_ #_:onClick (fn [ev]
                          (.stopPropagation ev)
                          (js/console.log "click" (:db/id e) bus)
                          (core/send! bus [:click (:db/id e)])
                          false)}
              ^String it])
           (:coll/type e)
           (case (:coll/type e)
             nil (comment
                   "Probably a retracted entity, do nothing")
             ^:inline (any-coll e
                                bus
                                (when selected? "selected")
                                (+ 2 indent-prop)
                                proply
                                #_(if-not (:form/highlight e)
                                    proply
                                    (zipmap (map :db/id
                                                 (next (mut/get-numeric-movement-vec e)))
                                            (range 2 9))))))
     (when selected?
       (ml/modeline-nest-next e bus form)))))

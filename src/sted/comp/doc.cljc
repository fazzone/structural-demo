(ns sted.comp.doc
  (:require
   [sted.embed :as e]
   [sted.schema :as s]
   [sted.embed.data :as sed]
   [datascript.core :as d]
   [sted.comp.root :as cr]
   [sted.df.async :as a]
   [sted.teleport :as t]
   [clojure.string :as string]
   [rum.core :as rum]
   [sted.sys.keyboard :as sk]
   [sted.sys.handle :as sh]
   [sted.sys.eval.sci :as eval-sci]
   [sted.comp.keyboard :as kc]
   [sted.cmd.mut :as mut]
   [sted.db-reactive :as dbrx]
   [sted.comp.common :as cc]
   [sted.comp.cons :as ccons]
   [sted.comp.debug :as cd]
   [sted.core :as core
    :refer [get-selected-form
            move-selection-tx]]))


(rum/defc mcn
  [rec classes state-map left right {:keys [view style label] :as opts}]
  (let [view (or (some-> (get state-map view)
                         :db-after
                         core/get-selected-form)
                 (some-> (get state-map left)
                         :db-after
                         (d/entity :sted.page/state)
                         :state/bar
                         :seq/first
                         :seq/first))
        eid         (:db/id view)
        before-left (:db-before (state-map left))
        after-left  (:db-after (state-map left))
        after-right (when right
                      (:db-after (state-map right)))]
    [:div {:class (str "alternate-reality " classes)
           :style (-> {:display               :grid
                       ;; :width "min-content"
                       :grid-template-columns "1fr 12ch 1fr"
                       :grid-template-rows "1fr 4ex calc(1fr - 3ex)"
                       :justify-items         "center"}
                      (merge style))}
     [:span {:style {:grid-area "1 / 1 / 4"}}
      (rec (d/entity (if-not right before-left after-left) eid)
           core/blackhole 0 nil)]
     [:span {:style {:grid-area "2 / 2 / 2"
                     :text-align "center"}}
      "->"
      (when label [:div {} (str label)])]
     [:span {:style {:grid-area "1 / 3 / 4"}}
      (rec (d/entity (if right after-right after-left) eid)
           core/blackhole 0 nil)]]))


(rum/defc teleport-doc
  [rec state-map {:keys [view style label body] :as opts}]
  (let [view-ent (some-> (get state-map view)
                         :db-before
                         core/get-selected-form)
        eid         (:db/id view-ent)]
    (println "View????" view (some? (get state-map view)) (keys state-map))
    [:div {:class "alternate-reality"
           :style (-> {:display               :grid
                       :grid-template-columns "1fr 1fr 3ex 1fr"}
                      (merge style))}
     (for [[label sk] body
           k [:lbl :db-before :arrow :db-after]]
       (case k
         :lbl [:span {:key (str k sk)} label]
         :arrow [:span {:key (str k sk)
                        :style {:justify-self "center"}}
                 "->"]
         (:db-before :db-after)
         [:div {:key (str k sk)
                :style {:justify-self "center"}}
          (rec (d/entity (-> state-map (get sk) (get k)) eid)
               core/blackhole 0 nil)]))]))

(rum/defc bar-and-chain
  [r c]
  (let [t-chain (t/run-steps-next '[(ns left)
                                    (def x 1)
                                    (ns right)
                                    (def y 2)]
                                  [[:flow-right]
                                   [:splice]
                                   [:select-chain]
                                   {:tag :bar  :do [:parent]}
                                   [:flow-right]
                                   [:flow-right]
                                   [:drag-left]
                                   [:select-chain]
                                   [:next]
                                   [:flow-right]
                                   [:drag-left]
                                   [:sink]
                                   [:select-chain]
                                   {:tag :next-chain  :do [:next]}
                                   {:tag :select-bar  :do [:parent]}
                                   [:flow-right]
                                   {:tag :sink-chain  :do [:sink]}
                                   [:float]
                                   {:tag :flow-into  :do [:flow-right]}
                                   {:tag :parent-nop  :do [:parent]}
                                   [:next]
                                   [:tail]
                                   [:prev]
                                   {:tag :select-chain  :do [:select-chain]}
                                   {:tag :select-chain-memory  :do [:select-chain]}
                                   {:tag :hop-right  :do [:hop-right]}
                                   [:next]
                                   {:tag :hop-left  :do [:hop-left]}
                                   [:parent]
                                   {:tag :drag-right  :do [:drag-right]}
                                   {:tag :drag-right-new  :do [:drag-right]}])]
    [:div.mutdoc
     [:div.form-title
      "Bar & chain"]
     [:p "The root of the display is a pair of special forms."]
     [:p "The 'chain' displays forms vertically, within a horizontal 'bar'."]
     (mcn r
          c
          t-chain
          :select-bar
          nil
          {:view :bar
           :label "parent"
           :style {:height "12ex"}})
     [:p "Most commands treat the bar and chains normally."]
     [:p "For example, swap the display order with " [:code "sink"] ":"]
     (mcn r
          c
          t-chain
          :sink-chain
          nil
          {:view :bar
           :label "sink"
           :style {:height "12ex"}})
     [:p "Flow into the chain to edit the contents:"]
     (mcn r
          c
          t-chain
          :flow-into
          nil
          {:view :bar
           :label "flow-right"
           :style {:height "12ex"}})
     [:p "For convenience, the bar and chains are handled specially by some commands."]
     [:p [:code "parent"] " does not move to chains, even though it is in fact the parent:"]
     (mcn r
          c
          t-chain
          :parent-nop
          nil
          {:view :bar
           :label "parent"
           :style {:height "12ex"}})
     [:p "Instead, use " [:code "select-chain"] " to get there from anywhere:"]
     (mcn r
          c
          t-chain
          :select-chain
          nil
          {:view :bar
           :label "select-chain"
           :style {:height "12ex"}})
     [:p "When the chain is already selected, " [:code "select-chain"] " tries to take you back:"]
     (mcn r
          c
          t-chain
          :select-chain-memory
          nil
          {:view :bar
           :label "select-chain"
           :style {:height "12ex"}})
     [:p "Move between chains with hop-left and hop-right:"]
     (mcn r
          c
          t-chain
          :hop-right
          nil
          {:view :bar
           :label "hop-right"
           :style {:height "12ex"}})
     [:p "These commands remember your selection too:"]
     (mcn r
          c
          t-chain
          :hop-left
          nil
          {:view :bar
           :label "hop-left"
           :style {:height "12ex"}})
     [:p "Move forms between chains with drag-left and drag-right:"]
     (mcn r
          c
          t-chain
          :drag-right
          nil
          {:view :bar
           :label "drag-right"
           :style {:height "12ex"}})
     [:p "Dragged forms are inserted before the destination chain's memory position."]
     [:p "Dragging past the end creates a new chain:"]
     (mcn r
          c
          t-chain
          :drag-right-new
          nil
          {:view :bar
           :label "drag-right"
           :style {:height "12ex"}})]))

(rum/defc doc-movement
  [r c]
  (let [t-flow (t/build t/flow-left-right)
        t-next-prev (t/build t/next-prev)]
    [:div.mutdoc {:style {:width "55em"}}
     [:div.form-title.mutdoc-title [:span "-->"] [:span "next"]]
     [:div {:style {:margin "0 0 1ex 0"}}
      "Advances cursor"]
     (teleport-doc r
                   t-next-prev
                   {:view :start
                    :body [
                           ["Goes to next form" :nextform]
                           ["Steps over" :stepover]]})
     [:div.form-title.mutdoc-title [:span "==>"] [:span "flow-right"]]
     [:div {:style {:margin "0 0 1ex 0"}}
      "Traverses structure"]
     (teleport-doc r
                   t-flow
                   {:view :start
                    :body [["On tokens, select next form" :like-next]
                           ["On lists, step in one level" :into-colls]
                           ["At the end, step out" :outof-colls]
                           ["Steps out of deep nesting" :outof-many]]})
     [:div.form-title.mutdoc-title [:span "<=="] [:span "flow-left"]]
     [:div {:style {:margin "0 0 1ex 0"}}
      "Inverse of " [:code "flow-right"]]
     (teleport-doc r
                   t-flow
                   {:view :start
                    :body [["Steps in to deep nesting" :left-into-many]
                           ["Steps out one level" :left-outof-one]]})]))

(rum/defc mutations-reference
  [r c]
  (let [t-tail (t/run-steps-next
                 '(a (b) (c (d e) f))
                 [{:tag :tail  :do [:tail]}
                  {:tag :parent  :do [:parent]}
                  [:tail]
                  {:tag :tl-b  :do [:toplevel]}])
        t-float (t/run-steps-next
                  '(c (d e) f)
                  [[:tail]
                   {:tag :float  :do [:float]}
                   {:tag :sink  :do [:sink]}])
        t-del (t/run-steps-next
                '(a L x R b)
                [[:flow-right] [:flow-right] [:flow-right]
                 {:tag :dla  :do [:delete-left]}])
        t-delr (t/run-steps-next
                 '(a L x R b)
                 [[:flow-right] [:flow-right] [:flow-right]
                  {:tag :dr  :do [:delete-right]}])
        t-raise (t/run-steps-next
                  '(f (if a x y))
                  [[:flow-right] [:flow-right] [:flow-right] [:flow-right]
                   {:tag :bonk  :do [:raise]}])
        t-comp (t/run-steps-next
                 '(f x)
                 [[:flow-right]
                  [:flow-right]
                  {:tag :create-partial  :do [:compose]}
                  {:tag :finish  :do [:edit/finish-and-move-up "g"]}])
        t-tear (t/run-steps-next ["/usr/bin/env"]
                                 [[:flow-right]
                                  [:flow-right]
                                  {:tag :tear  :do [:tear]}
                                  {:tag :stitch  :do [:tear]}])
        t-offer (t/run-steps-next '(f val)
                                  [[:tail]
                                   {:tag :offer-tk  :do [:offer]}
                                   [:edit/finish "x"]
                                   [:parent]
                                   {:tag :offer-coll  :do [:offer]}
                                   [:edit/finish "g"]])
        t-insl (t/run-steps-next '(x y z)
                                 [[:flow-right]
                                  [:flow-right]
                                  {:tag :insl  :do [:insert-left]}])
        t-insr (t/run-steps-next '(x y z)
                                 [[:flow-right]
                                  [:flow-right]
                                  {:tag :insr  :do [:insert-right]}])
        t-wrap (t/run-steps-next '(f x y)
                                 [[:flow-right]
                                  [:flow-right]
                                  {:tag :wrap  :do [:wrap]}])
        t-newlist (t/run-steps-next '(f x y)
                                    [[:flow-right]
                                     [:flow-right]
                                     {:tag :newlist  :do [:new-list]}
                                     [:edit/finish "g"]
                                     {:tag :newvec  :do [:new-vec]}])
        t-split (t/run-steps-next '(f [1 2 3 4])
                                  [[:flow-right] [:flow-right] [:flow-right] [:flow-right]
                                   {:tag :split  :do [:split]}])
        t-splice (t/run-steps-next '(f [1 2 3 4])
                                   [[:flow-right] [:flow-right] [:flow-right] [:flow-right]
                                    {:tag :splice  :do [:splice]}])
        t-slurp (t/run-steps-next '(f (g a) b c)
                                  [[:flow-right] [:flow-right] [:flow-right] [:flow-right]
                                   {:tag :slurp-tk  :do [:slurp-right]}
                                   [:parent]
                                   {:tag :slurp-coll  :do [:slurp-right]}
                                   {:tag :barf-coll  :do [:barf-right]}
                                   [:tail]
                                   {:tag :barf-tk  :do [:barf-right]}])]
    (rum/bind-context
      [cc/*modeline-ref* nil]
      [:div
       {:style {:display :flex
                :flex-direction :column
                :width "55em"}}
       (doc-movement r c)
       [:div.mutdoc {:style {:width "30em"}}
        [:p.form-title "Movement"]
        [:div.form-title.mutdoc-title
         [:span {:style {:transform "rotate(45deg)"}} "==>"]
         [:span "tail"]]
        [:div "Selects the last and most-nested node"]
        (mcn r c t-tail :tail)
        [:div.form-title.mutdoc-title
         [:span {:style {:transform "rotate(45deg)"}} "<--"]
         [:span "parent"]]
        [:div "One level up"]
        (mcn r c t-tail :parent)
        [:div.form-title.mutdoc-title
         [:span {:style {:transform "rotate(45deg)"}} "<=="]
         [:span "toplevel"]]
        [:div "Straight to the top"]
        (mcn r c t-tail :tl-b)]
       [:div.form-title.mutdoc-title
        [:span {} ")->"]
        [:span "slurp-right"]]
       [:div "on collections, add next to end"]
       (mcn r c t-slurp :slurp-coll)
       [:div "on tokens, apply to parent"]
       (mcn r c t-slurp :slurp-tk)
       [:div.mutdoc
        [:div.form-title.mutdoc-title
         [:span {} "<-)"]
         [:span "barf-right"]]
        [:div "on collections, move last element to next"]
        (mcn r c t-slurp :barf-coll)
        [:div "on tokens, apply to parent"]
        (mcn r c t-slurp :barf-tk)]
       [:div.mutdoc
        [:div.form-title.mutdoc-title
         [:span {} " "]
         [:span "splice"]]
        [:div "remove nesting"]
        (mcn r c t-splice :splice)]
       [:div.mutdoc
        [:div.form-title.mutdoc-title
         [:span {} " "]
         [:span "split"]]
        [:div "new list with rest"]
        (mcn r c t-split :split)]
       [:div.mutdoc
        [:div.form-title.mutdoc-title
         [:span {} "+()"]
         [:span "new-list"]]
        [:div "create new list after selected"]
        (mcn r c t-newlist :newlist)
        [:div "also new-vec"]
        (mcn r c t-newlist :newvec)]
       [:div.mutdoc
        [:div.form-title.mutdoc-title
         [:span {} "(+)"]
         [:span "wrap"]]
        [:div "create new list around selected"]
        (mcn r c t-wrap :wrap)]
       [:div.mutdoc
        [:div.form-title.mutdoc-title
         [:span {} ""]
         [:span "insert-left"]]
        [:div "add token before selected"]
        (mcn r c t-insl :insl)]
       [:div.mutdoc
        [:div.form-title.mutdoc-title
         [:span {} ""]
         [:span "insert-right"]]
        [:div "add token after selected"]
        (mcn r c t-insr :insr)]
       [:div.mutdoc
        [:div.form-title.mutdoc-title
         [:span {} ""]
         [:span "offer"]]
        [:div "on tokens, overwrite"]
        (mcn r c t-offer :offer-tk)
        [:div "on collections, add to end"]
        (mcn r c t-offer :offer-coll)]
       [:div.mutdoc
        [:div.form-title.mutdoc-title
         [:span {} "«⋯»"]
         [:span "tear/stitch"]]
        [:div "split the selected token"]
        (mcn r c t-tear :tear)
        [:div "or put it back together"]
        (mcn r c t-tear :stitch)]
       [:div.mutdoc
        [:div.form-title.mutdoc-title
         [:span {} "◯"]
         [:span "compose"]]
        [:div "Wrap selected, then edit first of created list"]
        (mcn r c t-comp :create-partial)
        [:div "When the edit is completed, the parent is selected"]
        (mcn r c t-comp :finish)]
       [:div.mutdoc
        [:div.form-title.mutdoc-title
         [:span {:style {:transform "rotate(45deg)"}} "<<-"]
         [:span "raise"]]
        [:div "replace parent with selected"]
        (mcn r c t-raise :bonk)]
       [:div.mutdoc
        [:div.form-title.mutdoc-title
         [:span {:style {:transform "scale(-1,1)"}} "⌦"]
         [:span "delete-left"]]
        [:div "Delete selected, select previous"]
        (mcn r c t-del :dla)]
       [:div.mutdoc
        [:div.form-title.mutdoc-title
         [:span "⌦"]
         [:span "delete-right"]]
        [:div "Delete selected, select next"]
        (mcn r c t-delr :dr)]
       [:div.mutdoc
        [:div.form-title.mutdoc-title
         [:span {:style {:transform "rotate(-90deg)"}} "->>"]
         [:span "float"]]
        [:div "Swap with previous"]
        (mcn r c t-float :float)]
       [:div.mutdoc
        [:div.form-title.mutdoc-title
         [:span {:style {:transform "rotate(90deg)"}} "->>"]
         [:span "sink"]]
        [:div "Swap with next"]
        (mcn r c t-float :sink)]
       (bar-and-chain r c)])))


(rum/defc root
  [r]
  [:div.top-level-form.code-font {}
   (mutations-reference r nil)])
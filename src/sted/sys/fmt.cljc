(ns sted.sys.fmt
  (:require
   [zprint.core :as z]
   [sted.cmd.nav :as nav]
   [sted.embed :as e]
   [sted.core :as core :refer [get-selected-form
                               move-selection-tx]]))

(defn mutatef
  [app]
  (fn [sel]
    (z/set-options! {:style :fast-hang
                     :map {:sort? nil
                           :respect-nl? true}
                     :set {:sort? nil
                           :respect-nl? true}
                     :list {:respect-nl? true}
                     :vector {:respect-nl? true}
                     :width 120})
    (let [_ (js/console.time "formatting")
          _ (js/console.time "preparing")
          q (if (= :chain (:coll/type sel))
              sel
              (peek (nav/parents-vec  sel)))
          _ (js/console.timeEnd "preparing")
          _ (js/console.time "stringifying")
          my-string (e/->string q)
          _ (js/console.timeEnd "stringifying")
          _ (js/console.time "zprint")
          p (z/zprint-file-str my-string (:db/id q))
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
                             (println (e/->string a) "\n" (pr-str a))
                             (println "B:")
                             (println (e/->string b) "\n" (pr-str b))
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
      ans)))

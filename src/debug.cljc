(ns debug
  (:require
   [clojure.edn :as edn]
   [cljs.pprint :as pprint]
   [datascript.core :as d]
   [rum.core :as rum]
   [cljs.core.async :as async])
  (:require-macros
   [cljs.core.async.macros :refer [go
                                   go-loop]]))

(comment

  (rum/defc example < rum/static
    [init-form muts]
    (let [conn (d/create-conn s/schema)
          form-txdata (e/->tx init-form)
          {:keys [db-after tempids] :as init-report}
          (d/transact! conn
                       [{:db/ident ::state
                         :state/bar {:db/id "bar"
                                     :coll/type :bar
                                     :seq/first {:coll/type :chain
                                                 :coll/_contains "bar"
                                                 :coll/contains #{(:db/id form-txdata)}
                                                 :seq/first form-txdata}}}])
          toplevel-eid (get tempids (:db/id form-txdata))
          reports (reductions
                   (fn [{:keys [db-after]} [m & args]]
                     (if-let [mut-fn (get mut/dispatch-table m)]
                       (try
                         (let [tx (apply mut-fn db-after args)]
                           (assoc (d/with db-after tx) :input-tx tx))
                         (catch :default e
                           (reduced
                            {:failure
                             [:div [:p.form-title [:span {:style {:color "tomato"}}
                                                   "Exception"]
                                    " "
                                    (ex-message e)]
                              [:p {} (with-out-str (cljs.pprint/pprint (ex-data e)))]
                              "Mutation"
                              [:p {} (pr-str (into [m] args))]
                              "At selected form"
                              [:p {} (with-out-str (cljs.pprint/pprint (d/touch (get-selected-form db-after))))]]})))
                       (throw (ex-info "No mutation" {:m m}))))
                   init-report
                   muts)]
      [:div {:style {:display "flex" :flex-direction "row" :width "1200px"}}
       [:div {:style {:display "flex" :flex-direction "column" :width "100%"}}
        (for [[i m {:keys [failure input-tx db-after tx-data]} other] (map vector (range) (cons "initial" muts) reports (cons nil reports))]
          (if failure
            [:div {} failure]
            [:div {:key i :style {:display :flex
                                  :flex-direction :column
                                  :margin-top "2ex"
                                  :border "1px solid #ae81ff"}}
             [:span (str "#" i " " (pr-str m))]
             [:div {:style {:border "1px solid #777"}}
              (when other (root-component (:db-after other) core/blackhole))]
             [:div {:style {:border "1px solid #777"}}
              (root-component db-after core/blackhole)]
             (when (< 0 i)
               [:div
                #_(pr-str (e/->form (get-selected-form db-after)))
                #_(pr-str (invar/check-all (:state/bar (d/entity db-after ::state))))
                #_[:div ;; :details [:summary "SVG"]
                   [:div {:style {:display :flex :flex-direction :row}}
                    (cc/svg-viewbox (:state/bar (d/entity (:db-after other) ::state)) core/blackhole)
                    (cc/svg-viewbox (:state/bar (d/entity db-after ::state)) core/blackhole)]]
                #_(pr-str input-tx)
                [:div ;; :details[:summary "txdata"]
                 ^:inline (debug/datoms-table-eavt* tx-data)]
                #_[:div
                   (debug/datoms-table-eavt* (d/datoms db-after :eavt))]])]))]]))

  (defn some-random-mutations
    [n]
    (->> #_[[[:insert-right] [:edit/finish "x"]]
            [[:insert-right] [:edit/finish "y"]]
            [[:flow-right]]
            [[:flow-right]]
            [[:flow-left]]
            [[:flow-left]]
            [[:raise]]
            #_[[:clone]]
            [[:next]]
            [[:prev]]
            [[:float]]
            [[:float]]
            [[:sink]]
            [[:sink]]
            #_[[:parent]]
            [[:tail]]
            [[:tail]]
            [[:wrap]]
            [[:delete-left]]
            [[:delete-left]]
            [[:wrap]]
            [[:wrap]]
            [[:delete-right]]
            [[:delete-right]]
            [[:insert-left] [:edit/finish "b"]]
            [[:insert-left] [:edit/finish "a"]]
            [[:slurp-right]] [[:slurp-right]] [[:slurp-right]] [[:slurp-right]] [[:slurp-right]] [[:slurp-right]]
            #_[[:delete-right]]
            #_[[:delete-left]]]
         [
          [[:slurp-right]] [[:slurp-right]] [[:slurp-right]] [[:slurp-right]] [[:slurp-right]] [[:slurp-right]]
          [[:barf-right]] [[:barf-right]] [[:barf-right]] [[:barf-right]] [[:barf-right]] [[:barf-right]]]
         cycle
         (take n)
         vec
         (shuffle)
         (apply concat)
         (vec)))

  (rum/defc player-mutation-view < rum/reactive
    [a]
    (let [v (rum/react a)]
      (when v [:pre ^String v])))

  (rum/defc player
    [init-form muts]
    (let [form-txdata (e/->tx init-form)
          {:keys [conn bus]} (setup-app (doto (d/create-conn s/schema)
                                          (d/transact! [{:db/ident ::state
                                                         :state/bar {:db/id "bar"
                                                                     :coll/type :bar
                                                                     :seq/first {:coll/type :chain
                                                                                 :coll/_contains "bar"
                                                                                 :coll/contains #{(:db/id form-txdata)}
                                                                                 :seq/first form-txdata}}}])))
          ;; se (d/entity @conn ::state)
          hz 44
          mv (atom nil)
          zch (core/zchan bus)]
      (rum/use-effect!
       (fn setup []
         (println "Setup")
         (async/go-loop [i 0
                         [m & more] muts
                         t nil]
           (if (and t)
             (swap! mv #(str "#" i " "
                             (/ (inc i)
                                (- (js/performance.now) t)
                                (/ 1 1000))
                             " "
                             (pr-str m)
                             "\n"
                             %)))
           (case m
             ::random-insert (do (core/send! bus [:insert-right])
                                 (core/send! bus [:edit/finish (str "m" i)]))
             (core/send! bus m))
           #_(async/<! (async/timeout (/ 1000 hz)))
           (async/<! (async/timeout 0))
           (if-not more
             (let [dms (- (js/performance.now) t)]
               (println "Finished" (inc i) "iter "
                        "Total ms" dms
                        " Hertz"
                        (* 1000 (/ (inc i) dms))))
             (recur (inc i) more (or t (js/performance.now)))))
         (fn cleanup []
           (println "Cleanup")))
       [])
      [:div
       (root-component @conn bus)
       #_(player-mutation-view mv)]))

  (rum/defc debug-component
    []
    [:div {:style {:margin-top "2ex"}}
     #_(player
        '[a b c ^:form/highlight [a s d f] [l e l] [O] d]
        (some-random-mutations 10000))
     (example
      '[a ^:form/highlight [] b c]
      #_[[:slurp-right] [:slurp-right]]
      [[:delete-right] [:barf-right] [:barf-right] [:delete-right] [:flow-left] [:barf-right]])
     #_(example
        '[a ^:form/highlight b c [a a a]]
        [[:float] [:delete-left] [:flow-right] [:float]]
        #_[[:last]])]))

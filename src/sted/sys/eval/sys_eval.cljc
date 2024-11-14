(ns sted.sys.eval.sys-eval
  (:require [datascript.core :as d]
            [sted.cmd.nav :as nav]
            [sted.embed :as e]
            [sted.cmd.edit :as edit]
            [sted.core :as core]))

(def default-state
  {:backend :eval-sci})

;; do-nrepl-eval [conn state db bus]
;; mutatef  [{:keys [conn bus]  :as app} app-atom-circular-ref]


(defn get-action-and-target
  [sel default]
  (let [up (nav/parents-vec sel)
        top (peek up)
        chain (some-> top :coll/_contains first)]
    #_(println "Up=" up)
    
    (reduce (fn [a e]
              (let [ea (:eval/action e)]
                #_(println "EvalTarget Checking"
                           (:db/id e)
                           (let [s (e/->string e)]
                             (subs s (min 128 (count s)))))
                (cond
                  ea    (reduced [ea e])
                  :else a)))
            
            [(or (:eval/action chain) default)
             (peek up)]
            
            up)))


(defn setup!
  [{:keys [conn bus] :as app}]
  (let [state (atom default-state)]
    (d/transact! conn [{:db/ident :sted.page/state ::state state}])
    (-> app
        (core/register-mutation! :eval
                                 (fn [_ db bus]
                                   (let [{:keys [backend]} @state
                                         sel               (core/get-selected-form db) 
                                         [emut target]     (get-action-and-target sel backend)]
                                     (println "Eval@" (:db/id sel) "Send" [emut (:db/id target)])
                                     (core/send! bus [emut (:db/id target)]))))
        
        (core/register-mutation! :set-eval-backend
                                 (fn [[_ be] db bus]
                                   (swap! state assoc :backend be)))
        
        (core/register-simple! :delete-last-result
                               (fn [sel]
                                 (let [db         (d/entity-db sel)
                                       a&t        (get-action-and-target sel ::dummy)
                                       [_ target] a&t
                                       mine       (d/datoms db :avet :eval/of (:db/id target))
                                       _          (do (println "Me=" (:db/id target) "Mine=")
                                                      (run! prn mine))
                                       ds         (or (not-empty mine)
                                                      (not-empty (d/datoms db :avet :eval/of)))
                                       [e _ _ _]  (apply min-key (fn [[_ _ _ t] -t]) ds)]
                                   (println "Delete?" e)
                                   (cond (not e)
                                         (println "Nothing to delete")
                                         
                                         (= e (:db/id sel))
                                         (println "Cannot delete yourself")

                                         (some #(= e %) (map :db/id (nav/parents-vec sel)))
                                         (println "Cannot delete a parent of yourself either")

                                         :else (edit/form-delete-tx (d/entity db e)))))))))

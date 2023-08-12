(ns sted.teleport
  (:require
   [datascript.core :as d]
   [sted.schema :as s]
   [sted.embed :as e]
   [sted.cmd.mut :as mut]
   [sted.core :as core
    :refer [get-selected-form
            move-selection-tx]]))


(defn create-conn*
  [form]
  (doto (d/create-conn s/schema)
    (d/transact! [{:db/ident :sted.page/state
                   :state/bar {:db/id "bar"
                               :coll/type :bar
                               :coll/contains "chain"
                               :seq/first {:db/id "chain"
                                           :coll/type :chain
                                           :seq/first (assoc form
                                                             :coll/_contains "chain"
                                                             :form/highlight true)}}}])))


(defn create-conn
  [form]
  (create-conn* (e/->tx form)))


(defn run-steps-next
  [init-form steps]
  (let [conn (create-conn init-form)
        mut-map (merge mut/editing-commands
                       (zipmap (keys mut/movement-commands)
                               (map core/movement->mutation
                                    (vals mut/movement-commands))))]
    (loop [[step & more] steps
           db @conn
           reports []
           ret {}]
      (if (and (nil? step) (nil? more))
        (assoc ret :history reports)
        (let [[mut-name & args :as mut] (if (map? step) (:do step) step)
              tag (if (map? step) (:tag step) nil)
              sel (core/get-selected-form db)
              mut-fn (get mut-map mut-name)
              _ (when-not mut-fn
                  (println "No mutation  " mut-name))
              r (d/with db (some-> mut-fn (apply sel args)))]
          (recur more (:db-after r)
                 (conj reports r)
                 (cond-> ret tag (assoc tag r))))))))


(defn build
  [{:keys [init do]}]
  (run-steps-next init do))


(def next-prev
  {:init '(a (f x) c)
   :do [{:tag :start :do [:flow-right]}
        {:tag :nextform :do [:next]}
        {:tag :stepover :do [:next]}]})


(def flow-left-right
  {:init '(a (b) c)
   :do [{:tag :start :do [:flow-right]}
        {:tag :like-next :do [:flow-right]}
        {:tag :into-colls :do [:flow-right]}
        {:tag :outof-colls :do [:flow-right]}
        [:flow-left]
        [:wrap]
        {:tag :outof-many :do [:flow-right]}
        {:tag :left-into-many :do [:flow-left]}
        {:tag :left-outof-one :do [:flow-left]}]})
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
              tag (cond (map? step) (:tag step)
                        
                        (and (meta step)
                             (= 1 (count (meta step))))
                        (first (keys (meta step)))
                        
                        (meta step)
                        (:tag (meta step)))
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
        {:tag :stepover :do [:next]}
        ;; ^:stepover [:next]
        ]})

(comment
  (run-steps-next '(a b c) next-prev))

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


(defn check-db-invariants!
  [db]
  (let [avet-contains (d/datoms db :avet :coll/contains)]
    (->> (map (fn [[_ _ va _] [_ _ vb _]] (when (= va vb) va))
              avet-contains
              (next avet-contains))
         (dedupe)
         (take-while some?)
         (run! (fn [e]
                 (throw (ex-info (str e " in >1 coll")
                                 (into {} (d/touch (d/entity db e))))))))))


(comment
  ;; 
  (check-db-invariants!
   (deref
    (doto (d/create-conn s/schema)
      (d/transact! [[:db/add "c" :db/ident "bad"]
                    [:db/add "a" :coll/contains "c"]
                    [:db/add "b" :coll/contains "c"]])))))


#_(e/->string
 (d/entity
  (:db-after
   (peek
    (:history
     (run-steps-next
      '((a b c) (i j k) (u v w))
      [[:flow-right-coll] [:flow-right-coll]
       [:flow-right]
       [:flow-right]
       [:split]
       [:splice]]))))
  4))

(defn permutations [v]
  (if (empty? v)
    [[]]
    (for [i (range (count v))
          rec (permutations (into (subvec v 0 i) (subvec v (inc i))))]
      (cons (nth v i) rec))))

(comment
  (doseq [steps (permutations
                 [[:flow-right-coll]
                  [:flow-right-coll]
                  [:flow-right]
                  [:flow-right]
                  [:split]
                  [:splice]])]
  
    (let [form '((a b c) (i j k) (u v w))
          {:keys [db-after]} (peek (:history (run-steps-next form steps)))]
  
      (println "================================")
      (prn form)
      (run! prn steps) (println "================")
      (println (e/->string (d/entity db-after 4)))
      (check-db-invariants! db-after))))

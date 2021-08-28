(ns embed
  (:require
   [datascript.core :as d]))

(def form-schema
  {:symbol/value {:db/index true}
   :number/value {}
   :string/value {}
   :keyword/value {:db/index true}
   :whitespace/value {}
   :coll/type {}
   :coll/contains {:db/valueType :db.type/ref
                   :db/cardinality :db.cardinality/many}
   :seq/first {:db/valueType :db.type/ref}
   :seq/next {:db/valueType :db.type/ref}})

(defn seq-tx
  [xs]
  (if-let [x (first xs)]
    (cond-> {:seq/first x}
      (next xs) (assoc :seq/next (seq-tx (next xs))))))

(def tempid-counter (atom 0) )
(defn new-tempid [] (swap! tempid-counter dec))

(defn flatten-map
  [m]
  (reduce-kv
   (fn [a k v]
     (-> a (conj k) (conj v)))
   []
   m))

(defn ->tx
  [e]
  (letfn [(coll-tx [coll-type xs]
            (let [id (new-tempid)]
              (cond-> {:db/id id :coll/type coll-type}
                (seq xs) (merge (seq-tx (for [x xs]
                                          (assoc (->tx x) :coll/_contains id)))))))]
   (cond 
     (symbol? e)    {:symbol/value (str e)}
     (keyword? e)   {:keyword/value e}
     (string? e)    {:string/value e}
     (number? e)    {:number/value e}
     (list? e)      (coll-tx :list e)
     (vector? e)    (coll-tx :vec e)
     (map? e)       (coll-tx :map (flatten-map e)))))

(defn seq->vec
  ([e]
   (seq->vec e []))
  ([e a]
   (if-let [f (:seq/first e)]
     (recur (:seq/next e) (conj a f))
     a)))

(defn ->form
  [e]
  (or (some-> (:symbol/value e) symbol)
      (when-let [ct (:coll/type e)]
        (let [elems (some->> (seq->vec e) (map ->form))]
          (or (case ct
                :list elems
                :vec  (vec elems)
                :map  (apply array-map elems))
              (case ct :list () :vec [] :map {}))))
      (:keyword/value e)
      (:string/value e)
      (:number/value e)))

(defn roundtrip
  [data]
  (let [tx-entity (update (->tx data)
                          :db/id #(or % "top"))
        {:keys [db-after tempids]}
        (d/with @(d/create-conn form-schema)
                [tx-entity])]
    (->form (d/entity db-after (get tempids (:db/id tx-entity))))))

(defn test-roundtrip
  [data]
  (= data (roundtrip data)))

(comment
  (test-roundtrip '( :a :b {:keys [db-after tempids]}))
  (test-roundtrip
   '(defn test-roundtrip
      [data]
      []
      (let [tx-entity (update (->tx data)
                              :db/id #(or % "top"))
            {:keys [db-after tempids]}
            (d/with @(d/create-conn form-schema)
                    [tx-entity])]
        (prn 'ds (count (d/datoms db-after :eavt)))
        (= data (->form (d/entity db-after (get tempids (:db/id tx-entity))))))))
)







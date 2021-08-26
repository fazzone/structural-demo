(ns embed
  (:require
   [datascript.core :as d]
   [rewrite-clj.zip :as z]
   [rewrite-clj.node :as n]
   [rewrite-clj.parser :as p]))

(def form-schema
  {:symbol/value {:db/index true}
   :number/value {}
   :string/value {}
   :keyword/value {:db/index true}
   :whitespace/value {}
   :coll/type {}
   :coll/elements {:db/valueType :db.type/ref}
   :coll/contains {:db/valueType :db.type/ref
                   :db/cardinality :db.cardinality/many}
   :seq/first {:db/valueType :db.type/ref}
   :seq/next {:db/valueType :db.type/ref}
   :map-entry/key {:db/valueType :db.type/ref}
   :map-entry/val {:db/valueType :db.type/ref}
   
   })

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
                (seq xs) (assoc :coll/elements
                                (seq-tx (for [x xs]
                                         (assoc (->tx x) :coll/_contains id)))))))]
   (cond 
     (symbol? e)    {:symbol/value (str e)}
     (keyword? e)   {:keyword/value e}
     (string? e)    {:string/value e}
     (number? e)    {:number/value e}
     ;; (map-entry? e) {:map-entry/key (->tx (key e))
     ;;                 :map-entry/val (->tx (val e))}
     (list? e)      (coll-tx :list e)
     (vector? e)    (coll-tx :vec e)
     (map? e)       (coll-tx :map (flatten-map e)))))

(defn rn->tx
  [n]
  (letfn [(coll-tx [coll-type xs]
            (let [id (new-tempid)]
              (cond-> {:db/id id :coll/type coll-type}
                (seq xs) (assoc :coll/elements
                                (seq-tx (for [x xs]
                                          (assoc (rn->tx x) :coll/_contains id)))))))
          (token-tx [{:keys [value lines] :as t}]
            #_(println "Token-tx" t)
            (cond
              (symbol? value)  {:symbol/value (n/string value)}
              (keyword? value) {:keyword/value value}
              (number? value)  {:number/value value}
              lines            {:string/value (apply str lines)}))]
    #_(println "Ntag" (n/tag n))
    (case (n/tag n)
      :token      (token-tx n)
      :whitespace {:whitespace/value (n/string n)}
      :map        (coll-tx :map (n/children n))
      :list       (coll-tx :list (n/children n))
      :vector     (coll-tx :vec (n/children n))

      )))






(comment
  (run! prn (d/datoms (d/entity-db (in-new-db (rn->tx (p/parse-string "(defn seq-tx [xs] 1)")))) :eavt)))


(defn seq->vec
  ([e] (seq->vec e []))
  ([e a]
   (if-not e
     a
     (recur (:seq/next e) (conj a (:seq/first e))))))

(defn ->form
  [e]
  (or (some-> (:symbol/value e) symbol)
      (when-let [ct (:coll/type e)]
        (or (case ct
              :list (some->> (:coll/elements e) (seq->vec) (map ->form))
              :vec  (some->> (:coll/elements e) (seq->vec) (mapv ->form))
              :map  (some->> (:coll/elements e)
                            (seq->vec)
                            (into {}
                                  (map (juxt (comp ->form :map-entry/key)
                                             (comp ->form :map-entry/val))))))
            (case ct :list () :vec [] :map {})))
      (:keyword/value e)
      (:string/value e)
      (:number/value e)))

(defn in-new-db
  [txe]
  (let [tx-entity (update txe :db/id #(or % "top"))
        {:keys [db-after tempids]} (d/with @(d/create-conn form-schema) [tx-entity])]
    (d/entity db-after (get tempids (:db/id tx-entity)))))

#_(in-new-db (rn->tx (p/parse-string "[a b c ]")))

(defn test-roundtrip
  [data]
  (let [tx-entity (update (->tx data)
                          :db/id #(or % "top"))
        {:keys [db-after tempids]}
        (d/with @(d/create-conn form-schema)
                [tx-entity])]
    (prn 'ds (count (d/datoms db-after :eavt)))
    (= data (->form (d/entity db-after (get tempids (:db/id tx-entity)))))))


(test-roundtrip
 (list :a :b :c))

(comment
  (run! prn
        (sort-by first
                 (:tx-data
                  (d/with @(d/create-conn form-schema)
                          [{:db/id -1
                            :coll/type :vec
                            :coll/elements {:seq/first {:string/value "ok" :coll/_contains -1}}}])))))

(test-roundtrip
 '(defn test-roundtrip
    [data]
    (let [{:keys [db-after tempids]}
          (d/with @(d/create-conn schema)
                  [(assoc (->tx data) :db/id "top")])]
      (= data (->form (d/entity db-after (get tempids "top")))))))


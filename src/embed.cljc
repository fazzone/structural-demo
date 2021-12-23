(ns embed
  (:require
   [rewrite-clj.parser :as p]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [rewrite-clj.node.protocols :as np]
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

(comment
  (p/parse-string
   "(defn flatten-map
  [m]
  (reduce-kv
   (fn [a k v]
     (-> a (conj k) (conj v)))
   []
   m))"))

(take-while (comp not neg?) (iterate dec 9))
(range 9 0 -1)
(comment
  (clojure.pprint/pprint (->tx [:a :b :c])))

(defn n->tx
  [n]
  (letfn [(coll-tx [coll-type xs]
            (let [id (new-tempid)
                  linebreak? (atom nil)]
              (cond-> {:db/id id :coll/type coll-type}
                (seq xs) (merge (seq-tx (for [x     xs
                                              :when (case (n/tag x)
                                                      (:comma :whitespace :newline)
                                                      (do (when (n/linebreak? x)
                                                            (reset! linebreak? true))
                                                          false)
                                                      true)]
                                          (cond-> (n->tx x)
                                            true (assoc :coll/_contains id)
                                            (deref linebreak?) (assoc :form/linebreak
                                                                      (do (reset! linebreak? nil)
                                                                          true)))))))))]
    (case (n/tag n)
      (:token :multi-line)
      (case (np/node-type n)
        :symbol  {:symbol/value (n/string n)}
        :keyword {:keyword/value (n/sexpr n)}
        :string  {:string/value (n/string n)}
        {:number/value (n/string n)})
      
      :list   (coll-tx :list (n/children n))
      :vector (coll-tx :vec (n/children n))
      :map    (coll-tx :map (n/children n))
      :set    (coll-tx :set (n/children n))
      :forms  (coll-tx :vec (n/children n))
      :comma  nil
      :comment {:string/value (n/string n)}
      :meta   (let [[mta-n val & more] (filter (comp not #{:whitespace :newline} n/tag) (n/children n))
                    mta                (n/sexpr n)]
                (when more (throw (ex-info "Cannot understand meta" {:meta [mta-n val more]})))
                (cond
                  (symbol? mta)
                  (assoc (n->tx val) :tag {:symbol/value mta})
                
                  (map? mta)
                  (merge (n->tx val) mta)

                  :else (throw (ex-info "What meta is this" {:mta mta}))))
      
      :namespaced-map (coll-tx :map (n/children n))
      (:uneval :fn :quote)
      (coll-tx :list (n/children n))
      
      (throw (ex-info  "Cannot decode" {:tag (n/tag n)})))))



(defn string->tx
  [s]
  (n->tx (p/parse-string s )))

(defn string->tx-all
  [s]
  (n->tx (p/parse-string-all s )))

(declare ->tx)

(defn ->tx*
  [e]
  (letfn [(coll-tx [coll-type xs]
            (let [id (new-tempid)]
              (cond-> {:db/id id :coll/type coll-type}
                (seq xs) (merge (seq-tx (for [x xs]
                                          (assoc (->tx x) :coll/_contains id)))))))]
    (cond 
      (symbol? e)  {:symbol/value (str e)}
      (keyword? e) {:keyword/value e}
      (string? e)  {:string/value e}
      (number? e)  {:number/value e}
      (boolean? e) {:symbol/value (str e)}
      (list? e)    (coll-tx :list e)
      (vector? e)  (coll-tx :vec e)
      (map? e)     (coll-tx :map (flatten-map e))
      (set? e)     (coll-tx :set e))))

(defn ->tx
  [e]
  (merge (meta e) (->tx* e)))

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
                :map  (apply array-map elems)
                :set  (set elems))
              (case ct :list () :vec [] :map {}))))
      (:keyword/value e)
      (:string/value e)
      (:number/value e)))

(defn roundtrip
  [data]
  (let [tx-entity (update (->tx data)
                          :db/id #(or % "top"))
        {:keys [db-after tempids]}
        (d/with (deref (d/create-conn form-schema))
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
            (d/with (deref (d/create-conn form-schema))
                    [tx-entity])]
        (prn 'ds (count (d/datoms db-after :eavt)))
        (= data (->form (d/entity db-after (get tempids (:db/id tx-entity)))))))))






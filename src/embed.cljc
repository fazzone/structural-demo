(ns embed
  (:require
   [rewrite-clj.parser :as p]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [rewrite-clj.node.protocols :as np]
   [schema :as s]
   [datascript.core :as d]))

(defn seq-tx
  [xs]
  (if-let [x                               (first xs)]
    (cond-> {:seq/first x}
      (next xs) (assoc :seq/next (seq-tx (next xs))))))

(def some-map
  {:this :is
   :my :map
   #_ #_ :ignored :value})

(defonce tempid-counter (atom 0) )

(defn new-tempid [] (swap! tempid-counter dec))

(defn flatten-map
  [m]
  (reduce-kv
   (fn [a k v]
     (-> a (conj k) (conj v)))
   []
   m))

(defn n->tx
  ([n] (n->tx n 0))
  ([n i]
   (letfn [(coll-tx [coll-type xs]
             (let [id         (new-tempid)
                   linebreak? (atom nil)
                   isf        (atom 0)]
               (cond-> {:db/id id :coll/type coll-type}
                 (seq xs) (merge (seq-tx (for [x     xs
                                               :when (case (n/tag x)
                                                       (:comma :whitespace)
                                                       (do (swap! isf + (count (n/string x)))
                                                           false)
                                                       :newline
                                                       (do (when (n/linebreak? x)
                                                             (reset! linebreak? true)
                                                             (reset! isf 0))
                                                           false)
                                                       true)]
                                           (let [i @isf]
                                             (reset! isf 0)
                                             (cond-> (n->tx x)
                                               true        (assoc :coll/_contains id)
                                               (< 1 i)     (assoc :form/indent i)
                                               @linebreak? (assoc :form/linebreak (do (reset! linebreak? false)
                                                                                      true))))))))))]
     (case (n/tag n)
       (:token :multi-line)
       (case (np/node-type n)
         :symbol  {:symbol/value (n/string n)}
         :keyword {:keyword/value (n/string n)}
         :string  {:string/value (n/sexpr n)}
         (if (= nil (n/sexpr n))
           {:symbol/value "nil"}
           {:number/value (n/sexpr n)}))
       
       :list    (coll-tx :list (n/children n))
       :vector  (coll-tx :vec (n/children n))
       :map     (coll-tx :map (n/children n))
       :set     (coll-tx :set (n/children n))
       :forms   (coll-tx :vec (n/children n))
       :deref   (coll-tx :deref (n/children n))
       :comma   nil
       :comment {:string/value (n/string n)}
       :meta    (let [[mta-n val & more] (filter (comp not #{:whitespace :newline} n/tag) (n/children n))
                      mta                (n/sexpr mta-n)]
                  #_(println "Mta-n" (n/string mta-n)
                             "Mta" mta
                             "Val" (n/string val)
                             "More" more)
                  (when more (throw (ex-info "Cannot understand meta" {:meta [mta-n val more]})))
                  (cond
                    (symbol? mta)
                    (assoc (n->tx val) :tag {:symbol/value mta})
                    
                    (map? mta)
                    (merge (n->tx val) mta)
                    
                    (keyword? mta)
                    (assoc (n->tx val) mta true)
                    
                    (string? mta)
                    (assoc (n->tx val) :tag {:string/value mta})
                    :else (do
                            (println "What meat?" mta)
                            (println "Type" (type mta))
                            (println "N string" (n/string n))
                            (throw (ex-info (str "What meta is this") {})))))
       

       
       ;; (n/children (first (n/children (p/parse-string-all "#?(:cljs 1 :clj 4)"))))
       ;; => (<token: ?> <list: (:cljs 1 :clj 4)>)
       :reader-macro
       (let [[rmt & body] (n/children n)]
         #_(coll-tx :map (n/children n))
         (-> (coll-tx :reader-macro body)
             (assoc :reader-macro/dispatch (n/string rmt))))
       
       :namespaced-map (coll-tx :map (n/children n))
       :uneval         (coll-tx :uneval (n/children n))
       
       :fn               (coll-tx :fn (n/children n))
       :quote            (coll-tx :quote (n/children n))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
       :syntax-quote     (coll-tx :syntax-quote (n/children n))
       :unquote          (coll-tx :unquote (n/children n))
       :unquote-splicing (coll-tx :unquote-splicing (n/children n))
       :var              {:symbol/value (n/string n)}
       :regex            {:regex/value (n/string n)}
       
       (throw (ex-info  (str "Cannot decode " (n/string n) (pr-str n)) {:tag (n/tag n)}))))))



(defn string->tx
  [s]
  (n->tx (p/parse-string s )))



(declare ->tx)

(defn ->tx*
  [e]
  (letfn [(coll-tx [coll-type xs]
            (let [id (new-tempid)]
              (cond-> {:db/id id :coll/type coll-type}
                (seq xs) (merge (seq-tx (for [x xs]
                                          (assoc (->tx x) :coll/_contains id)))))))]
    (cond 
      (symbol? e)     {:symbol/value (str e)}
      (keyword? e)    {:keyword/value e}
      (string? e)     {:string/value e}
      (number? e)     {:number/value e}
      (boolean? e)    {:symbol/value (str e)}
      (list? e)       (coll-tx :list e)
      (vector? e)     (coll-tx :vec e)
      (map? e)        (coll-tx :map (flatten-map e))
      (set? e)        (coll-tx :set e)
      (sequential? e) (coll-tx :list e)
      :else           (throw (ex-info (str "What is this" (type e) (pr-str e)) {})))))


(defn ->tx
  [e]
  (merge (meta e) (->tx* e)))

(defn seq->vec
  ([e]
   ;; #?(:cljs (js/performance.mark "svs"))
   (let [a (seq->vec e [])]
     ;; #?(:cljs (js/performance.measure "seq->vec" "svs"))
     a))
  ([e a]
   (if-let [f (:seq/first e)]
     (recur (:seq/next e) (conj a f))
     a)))

(defn seq->seq
  ([top]
   ((fn iter [e]
      (when-let [f (:seq/first e)]
        (cons f (lazy-seq (iter (:seq/next e))))))
    top)))



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



(defn ->string
  ([e] (->string e 0))
  ([e i]
   (letfn [(sep [{:form/keys [linebreak indent]}]
             (str (when linebreak "\n")
                  (let [ci (or indent 0)]
                    (when (< 0 ci)
                      (apply str (repeat ci " "))))))]
     (or (some-> (:symbol/value e) str)
         (some-> (:keyword/value e) str)
         (some-> (:string/value e) pr-str)
         (some-> (:number/value e) str)
         (when-let [ct (:coll/type e)]
           (let [[x & xs] (seq->vec e)]
             (str
              (case ct :list "(" :vec "[" :map "{" :set "#{" :chain nil :uneval "#_" :fn "#(" :deref "@" :quote "'")
              (cond
                (nil? x) nil
                
                (nil? xs) (str (sep x) (->string x (+ 2 i)))
                
                :else
                (loop [p nil
                       [y & ys] xs
                       s (str (sep x) (->string x (+ 2 i)))]
                  (if (nil? y)
                    s
                    (let []
                      (recur y ys (str s
                                       (case ct :chain "\n\n"
                                             (when (not (or (:form/linebreak y)
                                                            (:form/indent y)))
                                               " "))
                                       (sep y)
                                       (->string y (+ 2 i))))))))
              (case ct :list ")" :vec "]" (:set :map) "}" :fn  ")" (:chain :uneval :quote :deref) nil))))))))




(defn string->tx-all
  [s]
  (n->tx
   (n/forms-node
    (filter (comp not #{:whitespace :newline} n/tag)
            (n/children (p/parse-string-all s)))))
  #_(n->tx (p/parse-string-all s )))

(defn- ->chain
  [text]
  (let [txe (assoc (string->tx-all text)
                   :coll/type :chain)
        r (d/with (deref (d/create-conn s/form-schema))
                  [txe])]
    (d/entity (:db-after r) (get (:tempids r) (:db/id txe)))))


(defn- ->entity
  [data]
  (let [txe (update (->tx data) :db/id #(or % "top"))
        r (d/with (deref (d/create-conn s/form-schema))
                  [txe])]
    (d/entity (:db-after r) (get (:tempids r) (:db/id txe)))))


(defn roundtrip
  [data]
  (let [tx-entity (update (->tx data)
                          :db/id #(or % "top"))
        {:keys [db-after tempids]}
        (d/with (deref (d/create-conn s/form-schema))
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
            (d/with (deref (d/create-conn s/form-schema))
                    [tx-entity])]
        (prn 'ds (count (d/datoms db-after :eavt)))
        (= data (->form (d/entity db-after (get tempids (:db/id tx-entity)))))))))





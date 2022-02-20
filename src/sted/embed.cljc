(ns sted.embed
  (:require
   [sted.schema :as s]
   [sted.embed.common :as ec]
   [rewrite-clj.parser :as p]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [rewrite-clj.reader :as r]
   [rewrite-clj.node.protocols :as np]
   [clojure.string :as string]
   [datascript.core :as d]
   [datascript.impl.entity :as di]))

(defn flatten-map
  [m]
  (reduce-kv
   (fn [a k v]
     (-> a (conj k) (conj v)))
   []
   m))


(defn svtx
  [input]
  (loop [acc      (transient [])
         [x & xs] input
         p        nil]
    (if-not x
      (persistent! acc)
      (let [c (ec/new-tempid)]
       (recur
        (cond-> acc
          true (conj! [:db/add c :seq/first x])
          p (conj! [:db/add p :seq/next c]))
        xs
        c)))))

(defn in-new-db
  [tx-data]
  (d/with (deref (d/create-conn s/form-schema))
          tx-data))




(defn n->tx
  ([n] (n->tx n 0))
  ([n i]
   (letfn [(seq-ws-tx [[x & xs] id isf nl acc]
             (if-not x
               acc
               (case (n/tag x)
                 (:comma :whitespace)
                 (recur xs id (+ isf (count (n/string x))) nl acc)
                 :newline
                 (recur xs id 0 true acc)
                 :comment
                 (recur xs id 0 true
                        (conj acc
                              (cond-> (n->tx x isf)
                                true      (assoc :coll/_contains id)
                                nl        (assoc :form/linebreak nl)
                                (< 0 isf) (assoc :form/indent isf))))
                 (recur xs id 0 nil
                        (conj acc (cond-> (n->tx x isf)
                                    true      (assoc :coll/_contains id)
                                    (< 1 isf) (assoc :form/indent isf)
                                    nl        (assoc :form/linebreak true)))))))
           (coll-tx [coll-type xs]
             (let [id (ec/new-tempid)]
               (cond-> {:db/id id :coll/type coll-type}
                 (seq xs) (merge (ec/seq-tx (seq-ws-tx xs id 0 nil []))))))]
     (case (n/tag n)
       (:token :multi-line)
       (case (np/node-type n)
         :symbol  {:token/type :symbol :token/value (n/string n)}
         :keyword {:token/type :keyword :token/value (n/string n)}
         :string  {:token/type :string :token/value (n/sexpr n)}
         (let [v (n/sexpr n)]
           (cond
             (nil? v)    {:token/type :symbol :token/value "nil"}
             (number? v) {:token/type :number :token/value v}
             (true? v)   {:token/type :symbol :token/value "true"}
             (false? v)  {:token/type :symbol :token/value "false"}
             (char? v)   {:token/type :char :token/value v}
             :else       (throw (ex-info (str "What token is this? " (pr-str n)) {})))))
       :list             (coll-tx :list (n/children n))
       :vector           (coll-tx :vec (n/children n))
       :map              (coll-tx :map (n/children n))
       :set              (coll-tx :set (n/children n))
       :forms            (coll-tx :chain (n/children n))
       :deref            (coll-tx :deref (n/children n))
       :comma            nil
       :comment          {:token/type :comment :token/value (string/trimr (n/string n))}
       :meta             (let [[mta-n val & more] (filter (comp not #{:whitespace :newline} n/tag) (n/children n))
                               mta                (n/sexpr mta-n)]
                  #_(println "Mta-n" (n/string mta-n)
                             "Mta" mta
                             "Val" (n/string val)
                             "More" more)
                  (when more (throw (ex-info "Cannot understand meta" {:meta [mta-n val more]})))
                  (coll-tx :meta (n/children n))
                  #_{:coll/type     :meta
                     :coll/contains #{"m" "v"}
                     :seq/first     9}
                  #_(n->tx val))
       ;; (n/children (first (n/children (p/parse-string-all "#?(:cljs 1 :clj 4)"))))
       ;; => (<token: ?> <list: (:cljs 1 :clj 4)>)
       :reader-macro
       (let [[rmt & body] (n/children n)]
         (coll-tx :reader-macro (n/children n))
         #_(-> (coll-tx :reader-macro body)
               (assoc :reader-macro/dispatch (n/string rmt))))
       :namespaced-map   (coll-tx :map (n/children n))
       :uneval           (coll-tx :uneval (n/children n))
       :fn               (coll-tx :fn (n/children n))
       :quote            (coll-tx :quote (n/children n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       :syntax-quote     (coll-tx :syntax-quote (n/children n))
       :unquote          (coll-tx :unquote (n/children n))
       :unquote-splicing (coll-tx :unquote-splicing (n/children n))
       :var              {:token/type :symbol :token/value (n/string n)}
       :regex            {:token/type :regex :token/value (n/string n)}
       (throw (ex-info  (str "Cannot decode " (n/string n) (pr-str n)) {:tag (n/tag n)}))))))

(defn string->tx
  [s]
  (n->tx (p/parse-string s)))

#_(declare ->tx)

(def ^:dynamic *left* nil)
(defn ->tx*
  [e rec]
  (letfn [(coll-tx [coll-type xs]
            (let [id (ec/new-tempid)]
              (cond-> {:db/id id :coll/type coll-type}
                (seq xs) (merge (ec/seq-tx (for [x xs]
                                             (assoc (rec x) :coll/_contains id)))))))]
    (some-> *left* (vswap! dec))
    (cond
      (and *left* (> 0  @*left*)) {:token/type :symbol :token/value "..."}
      (symbol? e)                 {:token/type :symbol :token/value (str e)}
      (keyword? e)                {:token/type :keyword :token/value (str e)}
      (string? e)                 {:token/type :string :token/value e}
      (number? e)                 {:token/type :number :token/value e}
      (boolean? e)                {:token/type :symbol :token/value (str e)}
      (list? e)                   (coll-tx :list e)
      (vector? e)                 (coll-tx :vec e)
      (map? e)                    (coll-tx :map (flatten-map e))
      (set? e)                    (coll-tx :set e)
      (sequential? e)             (coll-tx :list e)
      #?@ (:cljs [(instance? js/Date e) {:token/type :string :token/value (str e)}
                  (instance? js/URL e)  {:token/type :string :token/value (str e)}])
      (nil? e)                    {:token/type :symbol :token/value "nil"}
      (instance? #?(:clj datascript.impl.entity.Entity :cljs di/Entity) e)
      (coll-tx :map
               (interleave (cons :db/id (keys e))
                           (cons (:db/id e)
                                 (for [v (vals e)]
                                   (cond
                                     (set? v)         (mapv :db/id v)
                                     (associative? v) {:db/id (:db/id v)}
                                     :else            v)))))
      
      :else
      (do
        #_(js/console.log "Handle" e)
        {:token/type :verbatim :token/value (str "type:" (type e) "\nvalue:\t" e)})
      #_ (throw (ex-info (str "What is this? type:" (type e) "Val: " (pr-str e)) {})))))

(defn ->tx
  [e]
  (when-let [mta (meta e)]
    (prn 'Mta mta))
  #_(merge (meta e) (->tx* e))
  #_(merge (->tx* e) (meta e))
  (->tx* e ->tx))

#? (:clj
    (defn seq->vec
      ([e]
       (seq->vec e []))
      ([e a]
       (if-let [f (:seq/first e)]
         (recur (:seq/next e) (conj a f))
         a))))

#? (:cljs
   (defn seq->vec
     [e] 
     (let [out #js []
           f :seq/first
           n :seq/next]
       (loop [q e]
         (if-some [ff (f q)]
           (do (.push out ff)
               (let [ne (n q)]
                 (if-not (= e ne )
                   (recur ne)
                   (throw (ex-info "Trivial cycle" {:db/id (:db/id e)})))))
           out)))))

(defn seq->seq
  ([top]
   ((fn iter [e]
      (when-let [f (:seq/first e)]
        (cons f (lazy-seq (iter (:seq/next e))))))
    top)))

(defn parse-keyword
  [{:token/keys [value]}]
  (cond
    (string/starts-with? value "::")
    (throw (ex-info "Don't know how to handle these yet" {}))
    
    (string/starts-with? value ":")
    (keyword (subs value 1))
    
    :else (throw (ex-info
                  (str "What sort of keyword is this? " value)
                  {}))))

(defn coll->
  [e ct xs]
  (case ct
    :list xs
    :vec  (vec xs)
    :map  (apply array-map xs)
    :set  (set xs)
    nil   nil
    (recur nil (:coll/data-type e) xs)))

(defn ->form
  [e]
  (or (when-let [ct (:coll/type e)]
        (coll-> e ct (some->> (seq->vec e) (map ->form))))
      (case (:token/type e)
        :symbol (symbol (:token/value e))
        :keyword (parse-keyword e)
        :number (:token/value e)
        :string (:token/value e)
        (throw (ex-info (str "Bad form " (:db/id e))
                        {:token-type (:token/type e)
                         :coll-type (:coll/type e)})))))

(defn open-delim
  [ct]
  (case ct
    :list             "("
    :vec              "["
    :map              "{"
    :set              "#{"
    :fn               "#("
    :tear             "«"
    :uneval           "#_"
    :meta             "^"
    :deref            "@"
    :quote            "'"
    :syntax-quote     "`"
    :unquote          "~"
    :unquote-splicing "~@"
    :reader-macro     "#"
    nil))

(defn close-delim
  [ct]
  (case ct
    :list ")"
    :vec  "]"
    :map  "}"
    :set  "}"
    :fn   ")"
    :tear "»"
    nil))

(defn ->string
  ([e] (->string e 0 nil))
  ([e i last?]
   (letfn [(sep [{:form/keys [linebreak indent]}]
             (str (when linebreak "\n")
                  (let [ci (or indent 0)]
                    (when (< 0 ci)
                      (apply str (repeat ci " "))))))]
     (or
      (when-let [tt (:token/type e)]
        (case tt
          :symbol  (str (:token/value e))
          :keyword (str (:token/value e))
          :string  (pr-str (:token/value e))
          :number  (str (:token/value e))
          :regex   (:token/value e)
          :comment (if-not last?
                     (:token/value e)
                     (str (:token/value e) "\n"))))
      (when-let [ct (:coll/type e)]
        (let [[x & xs] (seq->vec e)]
          (str
           (open-delim ct)
           (cond
             (nil? x)  nil
             (nil? xs) (str (sep x) (->string x (+ 2 i) true))
             :else
             (loop [p        nil
                    [y & ys] xs
                    s        (str (sep x) (->string x (+ 2 i) (nil? y)))]
               (if (nil? y)
                 s
                 (recur y ys (str s
                                  (case ct :chain "\n\n"
                                        (when (not (or (:form/linebreak y)
                                                       (:form/indent y)))
                                          " "))
                                  (sep y)
                                  (->string y (+ 2 i) (nil? ys)))))))
           (close-delim ct))))))))

(defn string->tx-all
  [s]
  (n->tx
   (n/forms-node
    (filter (comp not #{:whitespace
                        #_:newline} n/tag)
            (n/children (p/parse-string-all s)))))
  #_(n->tx (p/parse-string-all s)))

(defn parse->lazy-seq
  [s]
  ((fn iter [r]
     (when-let [form (p/parse r)]
       (case (n/tag form)
         (:whitespace :newline) (recur r)
         (cons form (lazy-seq (iter r))))))
   (r/string-reader s)))

(defn ->chain
  [text]
  (let [txe (assoc (string->tx-all text)
                   :coll/type :chain)
        r (d/with (deref (d/create-conn s/form-schema))
                  [txe])]
    (d/entity (:db-after r) (get (:tempids r) (:db/id txe)))))

(defn ->entity
  [data]
  (let [txe (update (->tx data) :db/id #(or % "top"))
        r (d/with (deref (d/create-conn s/schema))
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
  (test-roundtrip '(:a :b {:keys [db-after tempids]}))
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

(defn parse-token-tx
  [s]
  (try
    (string->tx s)
    (catch #? (:cljs js/Error :clj Exception) e
      {:token/type :verbatim :token/value s})))

(comment
  (let [n 5
        recpart (fn [e]
                  (if-not (and (sequential? e) (< n (count e)))
                    e
                    (recur (partition-all n e))))]
    (recpart (range 99))))

(comment
  (time
   (count
    (d/datoms (:db-after (in-new-db
                          #_[(string->tx-all
                              (slurp (clojure.java.io/resource "clojure/core.clj")))]
                          (mapv (comp string->tx-all slurp)
                                (filter
                                 (fn [s]
                                   (prn s)
                                   (let [ss (str s)]
                                     (or (clojure.string/ends-with? ss ".cljc")
                                         (clojure.string/ends-with? ss ".cljs")
                                         (clojure.string/ends-with? ss ".cljc"))))
                                 (file-seq (clojure.java.io/file "src"))))
                       
                          #_[(string->tx-all (slurp "src/sted/page.cljs"))]))
              :avet
              ))))


(do 0x7374656473746564)

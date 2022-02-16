(ns sted.embed.data-test
  (:require [sted.embed.data :as sed]
            [datascript.core :as d]
            [sted.embed :as e]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(defn find-token
  [db tv]
  (->> tv
       (d/datoms db :avet :token/value)
       (first)
       (first)
       (d/entity db)))

#_(defn with-nop-tokenizer
  [db]
  (d/db-with db [{:db/ident :tokenize :db/fn (constantly nil)}]))



;; => [attach (0 1 2 ...) please]



(t/deftest laziness
  (let [evaled   (atom #{})
        tracking (partial map
                          (fn [i]
                            (swap! evaled conj i)
                            i))
        limit    3
        outer    (e/->entity '[attach "here" please])
        rplacad  (find-token (d/entity-db outer) "here") 
        rcell    (first (:seq/_first rplacad))
        tx-data  (->> (sed/go (tracking (range))
                              limit
                              (:db/id outer)
                              (:db/id rcell))
                      (filter (comp not #{:tokenize} first)))
        {:keys [db-after]} (d/with (d/entity-db outer) tx-data)]
    
    (t/testing "did not evaluate too much"
      (t/is (=  #{0 1 2} @evaled)))
    (t/testing "transacted everything we did evaluate"
      (t/is
       (= (quote [attach (0 1 2 ...) please])
          (e/->form (d/entity db-after (:db/id outer))))))))

(t/deftest leaf
 (let [limit    3
       outer    (e/->entity '[attach "here" please])
       rplacad  (find-token (d/entity-db outer) "here") 
       rcell    (first (:seq/_first rplacad))
       tx-data  (->> (sed/go :me
                             limit
                             (:db/id outer)
                             (:db/id rcell))
                     (filter (comp not #{:tokenize} first)))
       {:keys [db-after]} (d/with (d/entity-db outer) tx-data)]
   (t/is
    (= (quote [attach :me please])
       (e/->form (d/entity db-after (:db/id outer)))))))

(t/deftest rplacd
 (let [limit   3
       outer   (e/->entity '[attach "end" deleted lol])
       rplacdd (find-token (d/entity-db outer) "end") 
       rcell   (first (:seq/_first rplacdd))
       tx-data (->> (sed/rplacd (range) limit (:db/id outer) (:db/id rcell))
                    (filter (comp not #{:tokenize} first)))
       {:keys [db-after]} (d/with (d/entity-db outer) tx-data)]

   (t/is
    (= (quote [attach 0 1 2 ...])
     (e/->form (d/entity db-after (:db/id outer)))))))


(comment
  "Nils"
  (let [limit    8
        outer    (e/->entity '[attach "here" please])
        rplacad  (find-token (d/entity-db outer) "here") 
        rcell    (first (:seq/_first rplacad))
        tx-data  (->> (sed/go {:kn nil :nn [:ok nil :y]}
                              limit
                              (:db/id outer)
                              (:db/id rcell))
                      (filter (comp not #{:tokenize} first)))
        {:keys [db-after]} (d/with (d/entity-db outer) tx-data)]
    (e/->form (d/entity db-after (:db/id outer)))))
;; => [attach {:kn nil, :nn [:ok nil :y]} please]

(comment
  (for [[n f] (ns-publics *ns*)
        :when (:test (meta f))]
    (do (f) n)))

(def map-test-data
  {:a :b
   :small-list (list 1 2 3)
   :medium-list (list :a
                      :b
                      {:nested :maps
                       :with [:list :values]}
                      :d)})

#_(let [limit 2
      data {:a :b :c :d}]
 (let [outer    (e/->entity '[attach "here" please])
       rplacad  (find-token (d/entity-db outer) "here") 
       rcell    (first (:seq/_first rplacad))
       tx-data  (sed/go data
                        limit
                        (:db/id outer)
                        (:db/id rcell))
       report (d/with (d/entity-db outer) tx-data)
       ]
   (run! prn tx-data)
   (run! prn (:tempids report))
   (prn (e/->string (:seq/first (d/entity
                                 (:db-after report)
                                 (:db/id rcell)))))
   (prn (e/->form (:seq/first (d/entity
                               (:db-after report)
                               (:db/id rcell)))))))

(defn ->ingested-entity
  [limit data]
  (let [outer    (e/->entity '[attach "here" please])
        rplacad  (find-token (d/entity-db outer) "here") 
        rcell    (first (:seq/_first rplacad))
        tx-data  (sed/go data
                         limit
                         (:db/id outer)
                         (:db/id rcell))]
    (:seq/first (d/entity
                 (d/db-with (d/entity-db outer) tx-data)
                 (:db/id rcell)))))

(t/deftest maps-valid
  (t/is
   (= (e/->form (->ingested-entity 99 map-test-data))
      map-test-data)))

#_(maps-valid)

(t/deftest traversal-order
  (doall
   (map
    (fn [a b] (t/is (= a b)))
    (mapv (fn [limit] (e/->string (->ingested-entity (+ 1 limit) map-test-data)))
          (range 17))
    ;; have to use strings due to incomplete maps
    ["{}"
     "{:a :b ...}"
     "{:a :b ...}"
     "{:a :b :small-list () ...}"
     "{:a :b :small-list (1 ...) :medium-list (:a ...)}"
     "{:a :b :small-list (1 2 ...) :medium-list (:a ...)}"
     "{:a :b :small-list (1 2 ...) :medium-list (:a :b ...)}"
     "{:a :b :small-list (1 2 3 ...) :medium-list (:a :b ...)}"
     "{:a :b :small-list (1 2 3 ...) :medium-list (:a :b {} ...)}"
     "{:a :b :small-list (1 2 3) :medium-list (:a :b {} ...)}"
     "{:a :b :small-list (1 2 3) :medium-list (:a :b {[] ...} :d)}"
     "{:a :b :small-list (1 2 3) :medium-list (:a :b {:nested :maps ...} :d)}"
     "{:a :b :small-list (1 2 3) :medium-list (:a :b {:nested :maps ...} :d)}"
     "{:a :b :small-list (1 2 3) :medium-list (:a :b {:nested :maps :with [:list ...]} :d)}"
     "{:a :b :small-list (1 2 3) :medium-list (:a :b {:nested :maps :with [:list :values ...]} :d)}"
     "{:a :b :small-list (1 2 3) :medium-list (:a :b {:nested :maps :with [:list :values]} :d)}"
     "{:a :b :small-list (1 2 3) :medium-list (:a :b {:nested :maps :with [:list :values]} :d)}"])))

(comment
  (dotimes [i 17]
    (let [limit    (+ 2 i)
          _ (do
              (println "Limit=" limit)
              (reset! sted.embed.common/tempid-counter 0))
          outer    (e/->entity '[attach "here" please])
          rplacad  (find-token (d/entity-db outer) "here") 
          rcell    (first (:seq/_first rplacad))
          tx-data  (->> (sed/go {:a :b
                                 :small-list (list 1 2 3)
                                 :medium-list (list :a
                                                    :b
                                                    {:nested :maps
                                                     :with [:list :values]}
                                                    :d)}
                                limit
                                (:db/id outer)
                                (:db/id rcell))
                        (filter (comp not #{:tokenize} first)))
          {:keys [db-after]} (d/with (d/entity-db outer) tx-data)]
      #_(run! prn tx-data)
      (prn (e/->string (:seq/first (d/entity db-after (:db/id rcell))))))))

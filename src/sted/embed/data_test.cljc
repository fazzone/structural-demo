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


(t/deftest laziness
  (let [evaled   (atom #{})
        tracking (partial map
                          (fn [i]
                            (swap! evaled conj i)
                            i))
        limit    2
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
  (let [limit   2
        outer   (e/->entity '[attach "end" deleted lol])
        rplacdd (find-token (d/entity-db outer) "end") 
        rcell   (first (:seq/_first rplacdd))
        tx-data (->> (sed/rplacd (range) limit (:db/id outer) (:db/id rcell))
                     (filter (comp not #{:tokenize} first)))
        {:keys [db-after]} (d/with (d/entity-db outer) tx-data)]

    (t/is
     (= (quote [attach 0 1 2 ...])
        (e/->form (d/entity db-after (:db/id outer)))))))

(def map-test-data
  (vec
   (for [i (range 4)]
     {:a :b
      :xs (vec (range (+ 3 i)))})))


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

(t/deftest nil-is-a-symbol
  (let [snil (symbol "nil")]
    (= `{:kn ~snil, :nn [:ok ~snil :y]}
       (e/->form (->ingested-entity 99 {:kn nil :nn [:ok nil :y]})))))

(t/deftest maps-valid
  (t/is
   (= (e/->form (->ingested-entity 99 map-test-data))
      map-test-data)))

(t/deftest traversal-order
  (doall
   (map
    (fn [a b] (t/is (= a b)))
    (mapv (fn [limit]
            (e/->string (->ingested-entity (+ 3 limit) map-test-data)))
          (range 28))
    ;; have to use strings due to incomplete maps
   ["[{:a :b :xs [0 ...]} {...} ...]"
     "[{:a :b :xs [0 ...]} {:a :b ...} {...} ...]"
     "[{:a :b :xs [0 ...]} {:a :b ...} {...} ...]"
     "[{:a :b :xs [0 ...]} {:a :b :xs [0 ...]} {...} ...]"
     "[{:a :b :xs [0 1 ...]} {:a :b :xs [0 ...]} {...} ...]"
     "[{:a :b :xs [0 1 ...]} {:a :b :xs [0 ...]} {:a :b ...} {...}]"
     "[{:a :b :xs [0 1 ...]} {:a :b :xs [0 ...]} {:a :b ...} {...}]"
     "[{:a :b :xs [0 1 ...]} {:a :b :xs [0 ...]} {:a :b :xs [0 ...]} {...}]"
     "[{:a :b :xs [0 1 ...]} {:a :b :xs [0 1 ...]} {:a :b :xs [0 ...]} {...}]"
     "[{:a :b :xs [0 1 2 ...]} {:a :b :xs [0 1 ...]} {:a :b :xs [0 ...]} {...}]"
     "[{:a :b :xs [0 1 2 ...]} {:a :b :xs [0 1 ...]} {:a :b :xs [0 ...]} {:a :b ...}]"
     "[{:a :b :xs [0 1 2 ...]} {:a :b :xs [0 1 ...]} {:a :b :xs [0 ...]} {:a :b ...}]"
     "[{:a :b :xs [0 1 2 ...]} {:a :b :xs [0 1 ...]} {:a :b :xs [0 ...]} {:a :b :xs [0 ...]}]"
     "[{:a :b :xs [0 1 2 ...]} {:a :b :xs [0 1 ...]} {:a :b :xs [0 1 ...]} {:a :b :xs [0 ...]}]"
     "[{:a :b :xs [0 1 2 ...]} {:a :b :xs [0 1 2 ...]} {:a :b :xs [0 1 ...]} {:a :b :xs [0 ...]}]"
     "[{:a :b :xs [0 1 2]} {:a :b :xs [0 1 2 ...]} {:a :b :xs [0 1 ...]} {:a :b :xs [0 ...]}]"
     "[{:a :b :xs [0 1 2]} {:a :b :xs [0 1 2 ...]} {:a :b :xs [0 1 ...]} {:a :b :xs [0 1 ...]}]"
     "[{:a :b :xs [0 1 2]} {:a :b :xs [0 1 2 ...]} {:a :b :xs [0 1 2 ...]} {:a :b :xs [0 1 ...]}]"
     "[{:a :b :xs [0 1 2]} {:a :b :xs [0 1 2 3 ...]} {:a :b :xs [0 1 2 ...]} {:a :b :xs [0 1 ...]}]"
     "[{:a :b :xs [0 1 2]} {:a :b :xs [0 1 2 3 ...]} {:a :b :xs [0 1 2 ...]} {:a :b :xs [0 1 2 ...]}]"
     "[{:a :b :xs [0 1 2]} {:a :b :xs [0 1 2 3 ...]} {:a :b :xs [0 1 2 3 ...]} {:a :b :xs [0 1 2 ...]}]"
     "[{:a :b :xs [0 1 2]} {:a :b :xs [0 1 2 3]} {:a :b :xs [0 1 2 3 ...]} {:a :b :xs [0 1 2 ...]}]"
     "[{:a :b :xs [0 1 2]} {:a :b :xs [0 1 2 3]} {:a :b :xs [0 1 2 3 ...]} {:a :b :xs [0 1 2 3 ...]}]"
     "[{:a :b :xs [0 1 2]} {:a :b :xs [0 1 2 3]} {:a :b :xs [0 1 2 3 4 ...]} {:a :b :xs [0 1 2 3 ...]}]"
     "[{:a :b :xs [0 1 2]} {:a :b :xs [0 1 2 3]} {:a :b :xs [0 1 2 3 4 ...]} {:a :b :xs [0 1 2 3 4 ...]}]"
     "[{:a :b :xs [0 1 2]} {:a :b :xs [0 1 2 3]} {:a :b :xs [0 1 2 3 4]} {:a :b :xs [0 1 2 3 4 ...]}]"
     "[{:a :b :xs [0 1 2]} {:a :b :xs [0 1 2 3]} {:a :b :xs [0 1 2 3 4]} {:a :b :xs [0 1 2 3 4 5 ...]}]"
     "[{:a :b :xs [0 1 2]} {:a :b :xs [0 1 2 3]} {:a :b :xs [0 1 2 3 4]} {:a :b :xs [0 1 2 3 4 5]}]"])))

(comment
  (reset! sted.embed.common/tempid-counter 0)
  (doall
   (for [[n f] (ns-publics *ns*)
         :when (:test (meta f))]
     (do (f) n))))

(comment
  (do
    (reset! sted.embed.common/tempid-counter 0)
    (println (e/->string (->ingested-entity
                          3
                          {:a "A"
                           :c "C"
                           :e "E"
                           :g "G"
                           :i "I"})))))

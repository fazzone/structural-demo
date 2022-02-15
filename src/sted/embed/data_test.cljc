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
  (for [[n f] (ns-publics *ns*)
        :when (:test (meta f))]
    (do (f) n)))

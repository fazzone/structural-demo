(ns sted.teleport-test
  (:require [sted.teleport :refer [avet-dupe-dual]]
            [clojure.test :as t]))

(defn valid
  [ds]
  (or (empty? ds)
      (let [as (for [[_ a _ _ ] ds] a) 
            vs (for [[_ _ v _ ] ds] v)]
        (and (apply <= vs)
             (= 1 (count (distinct as)))))))

(t/deftest test-avet-dupe-dual
  (t/are [avet1 avet2 expected] 
      (and (valid avet1)
           (valid avet2)
           (= expected (avet-dupe-dual avet1 avet2)))
    
    [[1 :x 101] [2 :x 101] [3 :x 102]]
    [[4 :y 103] [5 :y 104]]
    101
    
    [[1 :x 101] [2 :x 102]]
    [[3 :y 103] [4 :y 103]]
    103

    [[1 :x 101] [2 :x 102]]
    [[3 :y 101] [4 :y 103]]
    101
    
    [[1 :x 101] [2 :x 102]]
    [[3 :y 103] [4 :y 104]]
    nil
    
    [[1 :x 101]]
    [[2 :y 101]]
    101
    
    [[1 :x 101] [2 :x 102]]
    []
    nil
    
    
    []
    []
    nil
    
    [[1 :x 101] [2 :x 102] [3 :x 103] [4 :x 103]]
    [[5 :y 104] [6 :y 105]]
    103
    
    [[1 :x 101] [2 :x 102] [3 :x 104]]
    [[4 :y 102] [5 :y 103]]
    102
    
    [[1 :x 101] [2 :x 102] [3 :x 102]]
    [[4 :y 102] [5 :y 103] [6 :y 104]]
    102))


(comment
  (time
   (dotimes [i 1000]
     (let [length 10
           [aev bev] (->> (range (* 2 length))
                          (shuffle)
                          (map #(+ % 1001))
                          (map vector (next (range)))
                          (shuffle)
                          (split-at length))
           as (for [[e v] aev] [e :a v])
           bs (for [[e v] bev] [e :b v])
           
           aa (rand-nth as)
           bb (rand-nth bs)
           
           as+ (sort-by #(nth % 2) (cons aa as))
           bs+ (sort-by #(nth % 2) (cons bb bs))]
       
       (when (or (not= (nth aa 2) (avet-dupe-dual as+ bs))
                 (not= (nth bb 2) (avet-dupe-dual as bs+))
                 (not= nil (avet-dupe-dual as bs)))
         (run! println as+)
         (run! println bs+)
         (throw (ex-info "Bad" {:aa aa :bb bb})))))))


(t/run-tests)

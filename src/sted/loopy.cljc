(ns sted.loopy
  (:require [sted.embed :as e]
            [sted.embed.common :as ec]
            [datascript.core :as d]))



(def empty-list {:coll/type :list})
(def empty-vec {:coll/type :vec})
(def empty-map {:coll/type :map})
(def empty-set {:coll/type :set})

(defn nonrec-attrs
  [e]
  (cond
    (nil? e)        {:token/type :symbol :token/value "nil"}
    (symbol? e)     {:token/type :symbol :token/value (str e)}
    (keyword? e)    {:token/type :keyword :token/value (str e)}
    (string? e)     {:token/type :string :token/value e}
    (number? e)     {:token/type :number :token/value e}
    (boolean? e)    {:token/type :symbol :token/value (str e)}
    (list? e)       empty-list
    (vector? e)     empty-vec 
    (map? e)        empty-map
    (set? e)        empty-set
    (sequential? e) empty-list
    
    :else
    (do (prn "Handle e")
        {:token/type :verbatim :token/value (str "type:" (type e) "\nvalue:\t" e)})))


(defrecord LoopState
    [container
     my-cons
     rest
     exp])

#_(doit
 #_(let [n 3
         recpart (fn [e]
                   (if-not (and (sequential? e) (< n (count e)))
                     e
                     (recur (mapv vec (partition-all n e)))))]
     (recpart (range 15))))

(def test-data
  {0 999
   :nest {:layer :ok}
   :something [::stop]
   :lastkeyinoutermap 4}
  #_[1 [2 3] 4])

(defn ->token-type
  [e]
  (cond
    (symbol? e)                 :symbol
    (keyword? e)                :keyword
    (string? e)                 :string
    (number? e)                 :number
    (boolean? e)                :symbol
    #?@ (:cljs [(instance? js/Date e) :string
                (instance? js/URL e)  :string])))

(defn ->token-value
  [tt e]
  (case tt
    (:symbol :keyword) (str e)
    (:string :number) e
    (str e)))

(defn ->coll-type
  [e]
  (cond
    (list? e)                   :list
    (vector? e)                 :vec
    (map? e)                    :map
    (set? e)                    :set
    (sequential? e)             :list))

(defn emit-tk
  [out eid t v]
  (-> out
      (conj! [:db/add eid :token/type t])
      (conj! [:db/add eid :token/value v])))

(defn emit1
  [out eid coll-type v]
  (if coll-type
    (conj! out [:db/add eid :coll/type coll-type])
    (let [tt (or (->token-type v)
                 :verbatim)]
      (emit-tk out eid tt (->token-value tt v)))))

(defn go
  [root maxnodes]
  (loop [ncount maxnodes
         stk [(LoopState. nil nil (list root) nil)]
         out (transient [])]
    (cond
      (empty? stk)
      (persistent! out)
      
      (>= 1 ncount)
      (persistent!
       (reduce
        (fn [acc susp]
          (let [hid (str (d/squuid))
                car (ec/new-tempid)
                fst (first (:rest susp))]
            (-> (emit1 acc car (->coll-type fst) fst)
                (conj! {:db/id (:my-cons susp)
                        :seq/first {:db/id car
                                    :coll/_contains (:container susp)}
                        :seq/next {:seq/first {:handle/token hid
                                               :token/type :symbol
                                               :token/value "..."}}})
                (conj! [:tokenize hid (:rest susp)]))))
        out
        stk))
      
      :else
      (let [st          (first stk)
            popped      (subvec stk 1)
            [me & more] (:rest st)    ; note: forces TWO lazy seq elems
            cell        (:my-cons st)
            outer       (:container st)
            ncell       (when more (ec/new-tempid))
            map-ent?    (map-entry? me)
            coll-type   (when-not map-ent? (->coll-type me))]
        (cond
          (nil? me)
          (recur ncount popped out) 
          
          map-ent?
          (let [vcell (ec/new-tempid)
                vsimple? (or (nil? (val me))
                             (->token-type (val me)))]
            (recur
             ncount
             (-> [(LoopState. outer cell (list (key me)) "Kcell")]
                 (cond-> vsimple? (conj (LoopState. outer vcell (list (val me)) "Vcell")))
                 (cond-> more (conj (LoopState. outer ncell more "MapNcell")))
                 (into popped)
                 (cond-> (not vsimple?) (conj (LoopState. outer vcell (list (val me)) "Vcell"))))
             (cond-> out
               ncell (conj! [:db/add vcell :seq/next ncell])
               true (conj! [:db/add cell :seq/next vcell]))))
          
          coll-type
          (let [car   (ec/new-tempid)
                nrest (seq me)]
            (recur
             ncount
             (cond-> popped
               nrest (conj (LoopState. car car nrest nil))
               more  (conj (LoopState. outer ncell more nil)))
             (cond-> (emit1 out car coll-type me)
               cell (conj! [:db/add cell :seq/first car])
               ncell (conj! [:db/add cell :seq/next ncell])
               outer (conj! [:db/add outer :coll/contains car]))))
          
          :else
          (let [car (ec/new-tempid)]
            (recur
             (dec ncount)
             (cond-> popped
               more (conj (LoopState. outer ncell more nil)))
             (cond-> (emit1 out car coll-type me)
               cell (conj! [:db/add cell :seq/first car])
               ncell (conj! [:db/add cell :seq/next ncell])
               outer (conj! [:db/add outer :coll/contains car])))))))))

(comment
  (let [mseq (fn [n]
               (take n
                     (map
                      (fn [i]
                        (println "evaluating" i)
                        i)
                      (range))))
        n 99
        limit 3]
    (println
     (e/->string
      (let [txd (go (mseq n) limit)
            r (e/in-new-db txd)]
        (d/entity (:db-after r)
                  (get (:tempids r) (second (first txd)))))))
    (println "================ Print ")
    (prn
     (vec (take limit (mseq n))))))



(comment
  (go 1)
  [[:db/add -920090 :token/type :number]
   [:db/add -920090 :token/value "1"]
   [:db/add nil :coll/contains -920090]])



#_(defn doit
  ([] (doit test-data))
  ([td]
   (let [tx (go td)]
     #_(println "Tx" )
     (run! prn tx)
     (println
          (e/->string
           (d/entity (:db-after (e/in-new-db
                                tx
                                 #_(vec
                                    (for [[e a v t] (filter some? tx)]
                                      [e a v t]))))
                     1
                     ))))))


(comment
  (let [n 3
        recpart (fn [e]
                  (if-not (and (sequential? e) (< n (count e)))
                    e
                    (recur (mapv vec (partition-all n e)))))
        dd (recpart (range 99))]
    (prn "Coun" (count dd))

    (go dd 33)
    #_(dotimes [i 10]
        (println "=============================" i "===================================" )
        (println "Go:")
        (time
         (count
          (:tx-data (e/in-new-db (time (go dd))))))
        (println "Arr")
        (time
         (count
          (:tx-data (e/in-new-db [(time (e/->tx dd))]))
          #_(e/->tx dd))))
    :ok
  
    #_(recpart (range 15))))






(comment
  
  (a/let [resp (js/fetch "https://raw.githubusercontent.com/mdn/content/main/files/en-us/web/javascript/reference/global_objects/promise/index.md")
          text (.text resp)]
    (ingest (js->clj (mdast/parse text)))))


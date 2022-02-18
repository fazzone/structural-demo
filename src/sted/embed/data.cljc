(ns sted.embed.data
  (:require [sted.embed :as e]
            [sted.embed.common :as ec]
            [datascript.core :as d]))

(defn ->token-type
  [e]
  (cond
    (nil? e)     :symbol
    (symbol? e)  :symbol
    (keyword? e) :keyword
    (string? e)  :string
    (number? e)  :number
    (boolean? e) :symbol
    #?@ (:cljs [(instance? js/Date e) :string
                (instance? js/URL e)  :string])))

(defn ->token-value
  [tt e]
  (if (nil? e)
    "nil"
    (case tt
      (:symbol :keyword) (str e)
      (:string :number) e
      (str e))))

(defn ->coll-type
  [e]
  (cond
    (list? e)       :list
    (map-entry? e)  :vec
    (vector? e)     :vec
    (map? e)        :map
    (set? e)        :set
    (sequential? e) :list))

(defn zconj! [c v] (conj c v))

(defn emit-tk
  [out eid t v]
  (-> out
      (zconj! [:db/add eid :token/type t])
      (zconj! [:db/add eid :token/value v])))

(defn emit1
  [out eid coll-type v]
  (if coll-type
    (zconj! out [:db/add eid :coll/type coll-type])
    (let [tt (or (->token-type v)
                 :verbatim)]
      (emit-tk out eid tt (->token-value tt v)))))

(defrecord LoopState [
                      outer      ; eid which coll/contains us
                      cell       ; cons cell allocated for us to fill
                      rest       ; (cons us after-us)
                      lazy?      ; everything is lazy except map-entry
                      ])
(defn ^:dynamic *store-reference?* [_])

(defn ^:dynamic *store-reference!* [_ _])

(defn drain*
  [acc {:keys [rest outer cell lazy?]}]
  (let [hid (str (d/squuid))
        car (ec/new-tempid)
        fst (first rest)
        cont {:coll/_contains outer
              :handle/token hid
              :token/type :symbol
              :token/value (str "..." cell)}
        ct (->coll-type fst)]
    (*store-reference!* hid rest)
    (prn "Drain" cell lazy? rest)
    (-> acc (zconj! {:db/id cell :seq/first cont}))
    #_(cond
      ct (-> acc
             (emit1 car (->coll-type fst) fst)
             (conj! {:db/id cell
                     :seq/first {:db/id car
                                 :coll/_contains outer
                                 :seq/first 9
                                 }
                     :seq/next {:seq/first cont}}))
      )
    
    #_(if (= :lazy-map-tail? lazy?)
        (-> acc (zconj! {:db/id cell :seq/first cont}))
        (do
          (println "Creating coll?" (->coll-type fst))
          (-> acc
              (emit1 car (->coll-type fst) fst)
              (zconj! {:db/id cell
                       :seq/first {:db/id car :coll/_contains outer}
                       :seq/next {:seq/first cont}}))))
    #_(if (= :lazy-map-tail lazy?)
        (-> acc (conj! {:db/id cell :seq/first cont}))
        (-> acc
            (emit1 car (->coll-type fst) fst)
            (conj! {:db/id cell
                    :seq/first {:db/id car :coll/_contains outer}
                    :seq/next {:seq/first cont}})))))

#_(defn bigloop
  [ ncount stk out]
  (println "\nNc" ncount)
  (clojure.pprint/print-table stk)
  (clojure.pprint/print-table
   (for [o out]
     (zipmap [:op :e :a :v] o)))
  (if (empty? stk)
    out
    #_(persistent! out)
    (let [{:keys [rest outer cell lazy?]} (first stk)
          popped (subvec stk 1)]
      (cond
        (empty? rest)
        (recur ncount popped out)
        
        (and lazy? (>= 1 ncount))
        #_(persistent! (reduce drain* out stk))
        (reduce drain* out stk)
        
        :else
        ;; note: this forces TWO lazy seq elems
        (let [[me & more] rest]
          ;; hack to explore maps in the order we like
          (println "Me" (type me) me)
          (if (map-entry? me) 
            (let [ncell    (when more (ec/new-tempid))
                  vcell    (ec/new-tempid)
                  vsimple? (or (nil? (val me))
                               (->token-type (val me)))]
              (recur ncount
                     (cond-> [(LoopState. outer cell (list (key me)) nil)]
                       vsimple?       (conj (LoopState. outer vcell (list (val me)) nil))
                       more           (conj (LoopState. outer ncell more :lazy-map-tail))
                       true           (into popped)
                       (not vsimple?) (conj (LoopState. outer vcell (list (val me)) nil)))
                     (do
                       (println "Linking"
                                "\n\t" cell "->" vcell
                                "\n\t" vcell "->" ncell)
                       (cond-> out
                         true  (zconj! [:db/add cell :seq/next vcell])
                         ncell (zconj! [:db/add vcell :seq/next ncell])))))
            (let [car       (ec/new-tempid)
                  ncell     (when more (ec/new-tempid))
                  sref?     (*store-reference?* me)
                  nout      (cond-> out
                              cell  (zconj! [:db/add cell :seq/first car])
                              ncell (zconj! [:db/add cell :seq/next ncell])
                              outer (zconj! [:db/add outer :coll/contains car])
                              sref? (zconj! [:db/add car :handle/token sref?]))
                  npopped   (cond-> popped more (conj (LoopState. outer ncell more :lazy-tail)))
                  coll-type (->coll-type me)]
              (when sref?
                (*store-reference!* sref? me))
              (if-not coll-type
                (recur (dec ncount) npopped (emit1 nout car nil me))
                (let [nrest (seq me)]
                  (recur ncount
                         (cond-> npopped nrest (conj (LoopState. car car nrest
                                                                 (if (= :map coll-type)
                                                                   :lazy-map-head
                                                                   :lazy-enter))))
                         (emit1 nout car coll-type me)))))))))))

(defn ^:dynamic *store*
  ;; generate a key unconditionally
  ([])
  ;; generate a key optionally
  ([ptr])
  ;; store ptr under key
  ([key ptr]))

(defn emit-cont
  [out cell ptr force?]
  (let [k (if force?
            (*store*)
            (*store* ptr))]
    (if-not k
      out
      (do (*store* k ptr)
          (zconj! out [:db/add cell :handle/token k])))))

(defn sdloop
  [ncount stk out]
  (if (empty? stk)
    out
    (let [{:keys [rest outer cell lazy?]} (first stk)
          popped                          (subvec stk 1)
          me                              (first rest)]
      (cond
        (empty? rest)
        (recur ncount popped out)
        
        ;; unwind our stack - do not call next on any conses
        (> 1 ncount)                   
        (let [car  (ec/new-tempid)
              ct (->coll-type me)
              nout (-> out
                       (zconj! [:db/add cell :seq/first car])
                       (zconj! [:db/add cell :seq/first car]))]
          
          (recur 0 popped
                 (if (->coll-type me)
                   (-> nout
                       (emit1 car nil '...)
                       (emit-cont car me true))
                   (let [ccell (ec/new-tempid)]
                     (-> nout
                         (emit1 car nil me)
                         (emit-cont ccell rest true)
                         (zconj! [:db/add outer :coll/contains car])
                         (zconj! [:db/add outer :coll/contains ccell])
                         (zconj! {:db/id cell
                                  :seq/next {:seq/first {:db/id ccell
                                                         :token/type :symbol
                                                         :token/value "..."}} }))))))
        
        ;; reduce one cons, allocate tempids for its pointers
        :else
        (let [;; _ (println "NextRest {")
              more  (next rest)
              ;; _ (println "}")
              vcell (ec/new-tempid)
              ncell (when more (ec/new-tempid))]
          ;; splice map entries directly into parent
          (if (map-entry? me)
            (recur ncount
                   (cond-> [(LoopState. outer cell (list (key me)) nil)]
                     ncell (conj (LoopState. outer ncell more :lazy-map-tail))
                     true  (conj (LoopState. outer vcell (list (val me)) nil))
                     true  (into popped))
                   (cond-> out
                     ;; vcell is a cons for the map value
                     true  (zconj! [:db/add cell :seq/next vcell])
                     ncell (zconj! [:db/add vcell :seq/next ncell])))
            
            (let [nout (cond-> out
                         ;; vcell is a value cell for this coll/token node
                         cell  (zconj! [:db/add cell :seq/first vcell])
                         ncell (zconj! [:db/add cell :seq/next ncell])
                         true  (emit-cont vcell me nil)
                         outer (zconj! [:db/add outer :coll/contains vcell]))
                  npopped (cond-> popped ncell
                                  (conj (LoopState. outer ncell more :lazy-tail)))
                  coll-type (->coll-type me)]
              (if-not coll-type
                (recur (dec ncount) npopped (emit1 nout vcell nil me))
                (let [nrest (seq me)]
                  (recur ncount
                         (cond-> npopped nrest (conj (LoopState. vcell vcell nrest :lazy-enter)))
                         (emit1 nout vcell coll-type me)))))))))))
(defn go
  ([root limit]
   (go root limit nil nil))
  ([root limit in at]
   #_(bigloop limit
            [(LoopState. in at (list root) :lazy-root-i-guess)]
            []
            #_(transient []))
   (sdloop limit
           [(LoopState. in at (list root) :lazy-root-i-guess)]
           []
           #_(transient []))))

(defn rplacd
  "`root` must be a seq. cells after `at` summarily unlinked"
  [root limit in at]
  (sdloop limit
           [(LoopState. in at root :lazy-root-i-guess)]
           []
           #_(transient [])))

(defn find-token
  [db tv]
  (->> tv
       (d/datoms db :avet :token/value)
       (first)
       (first)
       (d/entity db)))

(defn ->ingested-entity
  [limit data]
  (let [outer    (e/->entity '[attach "here" please])
        rplacad  (find-token (d/entity-db outer) "here") 
        rcell    (first (:seq/_first rplacad))
        tx-data  (go data
                         limit
                         (:db/id outer)
                         (:db/id rcell))]
    (:seq/first (d/entity
                 (d/db-with (d/entity-db outer) tx-data)
                 (:db/id rcell)))))

#_(println (e/->string (->ingested-entity 4 [1 2 3 4 5])))

(comment
  (println
   (time
    (with-out-str
      (run! (fn [limit]
              (println
               'limit
               limit
               (e/->string (->ingested-entity (+ 2 limit)
                                              #_{:a :b :c :d :e :f}
                                              #_(take 44 (range))
                                              (for [i (range 4)]
                                                #_{i i :x :y}
                                                {:i i :xs (range (inc i))}))))
              #_(println))
            (range 21))))))

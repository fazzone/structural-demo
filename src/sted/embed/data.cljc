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

(defrecord LoopState [
                      outer      ; eid which coll/contains us
                      cell       ; cons cell allocated for us to fill
                      rest       ; (cons us after-us)
                      lazy?      ; everything is lazy except map-entry
                      ])

(defn ^:dynamic *store-reference?* [_])
(defn ^:dynamic *store-reference!* [_ _])
(defn ^:dynamic *mapper* [e] e)

(defn drain*
  [acc {:keys [rest outer cell lazy?]}]
  (let [hid (str (d/squuid))
        car (ec/new-tempid)
        fst (first rest)
        cont {:coll/_contains outer
              :handle/token hid
              :token/type :symbol
              :token/value "..."}]
    #_(println "Draining " lazy? cell hid "===" rest (->coll-type fst) (type fst))
    #_(when (= :lazy-map-tail lazy?)
        (println "Draining " lazy? cell hid "===" rest (->coll-type fst) (type fst)))
    (*store-reference!* hid rest)
    (if (= :lazy-map-tail lazy?)
      (-> acc (conj! {:db/id cell :seq/first cont}))
      (-> acc
          (emit1 car (->coll-type fst) fst)
          (conj! {:db/id cell
                  :seq/first {:db/id car :coll/_contains outer}
                  :seq/next {:seq/first cont}})))
    #_(-> acc
        (cond-> (not= :lazy-map-tail lazy?) (emit1 car (->coll-type fst) fst))
        (conj! {:db/id cell
                :seq/first {:db/id car :coll/_contains outer}
                :seq/next {:seq/first {:coll/_contains outer
                                       :handle/token hid
                                       :token/type :symbol
                                       :token/value "..."}}}))))

(defn bigloop
  [ ncount stk out]
  ;; (>= 1 ncount) (persistent! (reduce drain* out stk))
  (if (empty? stk)
    (persistent! out)
    (let [{:keys [rest outer cell lazy?]} (first stk)
          popped (subvec stk 1)]
      (cond
        (empty? rest) (recur ncount popped out)
        
        (and lazy? (>= 1 ncount))
        (persistent! (reduce drain* out stk))
        
        :else
        ;; note: this forces TWO lazy seq elems
        (let [[me & more] rest]
          ;; hack to explore maps in the order we like
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
                     (cond-> out
                       ncell (conj! [:db/add vcell :seq/next ncell])
                       true  (conj! [:db/add cell :seq/next vcell]))))
            (let [car       (ec/new-tempid)
                  ncell     (when more (ec/new-tempid))
                  sref?     (*store-reference?* me)
                  nout      (cond-> out
                              cell  (conj! [:db/add cell :seq/first car])
                              ncell (conj! [:db/add cell :seq/next ncell])
                              outer (conj! [:db/add outer :coll/contains car])
                              sref? (conj! [:db/add car :handle/token sref?]))
                  npopped   (cond-> popped more (conj (LoopState. outer ncell more :lazy-tail)))
                  coll-type (->coll-type me)]
              (when sref?
                (*store-reference!* sref? me))
              (if-not coll-type
                (recur (dec ncount) npopped (emit1 nout car nil me))
                (let [nrest (seq me)]
                  (recur ncount
                         (cond-> npopped nrest (conj (LoopState. car car nrest :lazy-enter)))
                         (emit1 nout car coll-type me)))))))))))
#_(defn bigloop
  [ ncount stk out]
  (cond
    (empty? stk)  (persistent! out)
    (>= 1 ncount) (persistent! (reduce drain* out stk))
    :else
    (let [{:keys [rest outer cell]} (first stk)
          popped                    (subvec stk 1)
          ;; note: this forces TWO lazy seq elems
          [me & more]             rest]
      #_(println "Meta" (meta me))
      (cond
        #_(nil? me)
        (empty? rest)
        (recur ncount popped out) 
        
        ;; hack to explore maps in the order we like
        (map-entry? me) 
        (let [ncell    (when more (ec/new-tempid))
              vcell    (ec/new-tempid)
              vsimple? (or (nil? (val me))
                           (->token-type (val me)))]
          (println "Ncell" ncell "Vcell" vcell)
          (recur ncount
                 ;; transfer our pre-allocated cell into the next iter 
                 (cond-> [(LoopState. outer cell (list (key me)) nil)]
                   vsimple?       (conj (LoopState. outer vcell (list (val me)) nil))
                   more           (conj (LoopState. outer ncell more :lazy-map-tail))
                   true           (into popped)
                   (not vsimple?) (conj (LoopState. outer vcell (list (val me)) nil)))
                 (cond-> out
                   ;; link the rest (if any) after the value 
                   ncell (conj! [:db/add vcell :seq/next ncell])
                   ;; always link the key to the value
                   true  (conj! [:db/add cell :seq/next vcell]))))
        
        :else
        (let [car       (ec/new-tempid)
              ncell     (when more (ec/new-tempid))
              sref?     (*store-reference?* me)
              nout      (cond-> out
                          ;; fill in our cell
                          cell  (conj! [:db/add cell :seq/first car])
                          ;; link it to the next cell we just created
                          ncell (conj! [:db/add cell :seq/next ncell])
                          outer (conj! [:db/add outer :coll/contains car])
                          sref? (conj! [:db/add car :handle/token sref?]))
              npopped   (cond-> popped more (conj (LoopState. outer ncell more :lazy-tail)))
              coll-type (->coll-type me)]
          (when sref?
            (*store-reference!* sref? me))
          (if-not coll-type
            (recur (dec ncount) npopped (emit1 nout car nil me))
            (let [nrest (seq me)]
              (recur ncount
                     (cond-> npopped nrest (conj (LoopState. car car nrest :lazy-enter)))
                     (emit1 nout car coll-type me)))))))))
(defn go
  ([root limit]
   (go root limit nil nil))
  ([root limit in at]
   (bigloop limit
            [(LoopState. in at (list root) :lazy-root-i-guess)]
            (transient []))))

(defn rplacd
  "`root` must be a seq. cells after `at` summarily unlinked"
  [root limit in at]
  (bigloop limit
           [(LoopState. in at root :lazy-root-i-guess)]
           (transient [])))

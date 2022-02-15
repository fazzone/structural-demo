(ns sted.embed.data
  (:require [sted.embed :as e]
            [sted.embed.common :as ec]
            [datascript.core :as d]))



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

(defrecord LoopState [
                      outer             ; eid which coll/contains us
                      cell              ; cons cell allocated for us
                      rest              ; (cons us after-us)
                      ])


(def ^:dynamic *store-reference!* (fn [_ _]))

(defn drain*
  [acc {:keys [rest outer cell]}]
  (let [hid (str (d/squuid))
        car (ec/new-tempid)
        fst (first rest)]
    (*store-reference!* hid rest)
    (-> (emit1 acc car (->coll-type fst) fst)
        (conj! {:db/id cell
                :seq/first {:db/id car :coll/_contains outer}
                :seq/next {:seq/first {:handle/token hid
                                       :token/type :symbol
                                       :token/value "..."}}}))))

(defn bigloop
  [ncount stk out]
  (cond
    (empty? stk)  (persistent! out)
    (>= 1 ncount) (persistent! (reduce drain* out stk))
    :else
    (let [{:keys [rest outer cell]} (first stk)
          popped                    (subvec stk 1)
          ;; note: this forces TWO lazy seq elems
          [me & more]               rest]
      (cond
        (nil? me)
        (recur ncount popped out) 
        
        ;; hack to explore maps in the order we like
        (map-entry? me) 
        (let [ncell    (when more (ec/new-tempid))
              vcell    (ec/new-tempid)
              vsimple? (or (nil? (val me))
                           (->token-type (val me)))]
          (recur ncount
                 (cond-> [(LoopState. outer cell (list (key me)))]
                   vsimple?       (conj (LoopState. outer vcell (list (val me))))
                   more           (conj (LoopState. outer ncell more))
                   true           (into popped)
                   (not vsimple?) (conj (LoopState. outer vcell (list (val me)))))
                 (cond-> out
                   ncell (conj! [:db/add vcell :seq/next ncell])
                   true  (conj! [:db/add cell :seq/next vcell]))))
        
        :else
        (let [car       (ec/new-tempid)
              ncell     (when more (ec/new-tempid))
              nout      (cond-> out
                          cell  (conj! [:db/add cell :seq/first car])
                          ncell (conj! [:db/add cell :seq/next ncell])
                          outer (conj! [:db/add outer :coll/contains car]))
              npopped   (cond-> popped more (conj (LoopState. outer ncell more)))
              coll-type (->coll-type me)]
          (if-not coll-type
            (recur (dec ncount) npopped (emit1 nout car nil me))
            (let [nrest (seq me)]
              (recur ncount
                     (cond-> npopped nrest (conj (LoopState. car car nrest)))
                     (emit1 nout car coll-type me)))))))))
(defn go
  ([root limit]
   (go root limit nil nil))
  ([root limit in at]
   (bigloop limit
            [(LoopState. in at (list root))]
            (transient []))))

(defn rplacd
  "`root` must be a seq. cells after `at` summarily unlinked"
  [root limit in at]
  (bigloop limit
           [(LoopState. in at root)]
           (transient [])))

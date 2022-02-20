(ns sted.embed.data
  (:require [sted.embed :as e]
            [datascript.db :as db #?@(:cljs [:refer [Datom]])]
            [sted.embed.common :as ec]
            [datascript.core :as d])
  #?(:clj (:import [datascript.db Datom])))

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
      (str "# "
           (or (some-> e (.-constructor ) (.-name))
               "object")))))

(defn ->coll-type
  [e]
  (cond
    (list? e)           :list
    (map-entry? e)      :vec
    (vector? e)         :vec
    (map? e)            :map
    (set? e)            :set
    (sequential? e)     :list
    (instance? Datom e) :vec))

(defn zconj! [c v] (conj c v))

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
                      outer       ; eid which coll/contains us
                      cell        ; cons cell allocated for us to fill
                      rest        ; (cons us after-us)
                      lazy?       ; everything is lazy except for map entries
                      ])


(defn ^:dynamic *store*
  ;; generate a key unconditionally
  ([])
  ;; generate a key optionally
  ([ptr])
  ;; store ptr under key
  ([key ptr]))

(defn emit-cont
  [out cell ptr k]
  (if-not k
    out
    (do (*store* k ptr)
        (conj! out [:db/add cell :handle/token k]))))





(defn sdloop
  [ncount stk out]
  #_(println "Sdloop" (first stk))
  (if (empty? stk)
    (persistent! out)
    (let [{:keys [rest outer cell lazy?]} (first stk)
          popped                    (subvec stk 1)
          me                        (first rest)]
      #_(println "Sdloop" ncount rest)
      (cond
        (empty? rest)
        (recur ncount popped out)
        
        ;; unwind our stack - do not call next on any conses
        (and lazy? (> 1 ncount))                   
        (let [car  (ec/new-tempid)
              ct   (->coll-type me)
              nout (-> out
                       (conj! [:db/add cell :seq/first car])
                       #_(conj! [:db/add cell :seq/first car]))]
          (recur 0 popped
                 (if (->coll-type me)
                     (-> out
                         (conj! [:db/add cell :seq/first car])
                         (conj! [:db/add outer :coll/contains car])
                         (emit1 car nil '...C)
                         (emit-cont car (cons nil rest) (*store*)))
                     (let [ccell (ec/new-tempid)]
                       (-> nout
                           (emit1 car nil me)
                           (emit-cont ccell rest (*store*))
                           (conj! [:db/add outer :coll/contains car])
                           (conj! [:db/add outer :coll/contains ccell])
                           (conj! {:db/id    cell
                                   :seq/next {:seq/first {:db/id       ccell
                                                          :token/type  :symbol
                                                          :token/value "...R"}} }))))))
        
        ;; reduce one cons, allocate tempids for its pointers
        :else
        (let [ ;; _ (println "NextRest {")
              more  (next rest)
              ;; _ (println "}")
              vcell (ec/new-tempid)
              ncell (when more (ec/new-tempid))]
          ;; splice map entries directly into parent
          (if (and (map-entry? me) (not= :root lazy?))
            (recur ncount
                   (cond-> [(LoopState. outer cell (list (key me)) nil)]
                     ncell (conj (LoopState. outer ncell more :lazy-map-tail))
                     true  (conj (LoopState. outer vcell (list (val me)) nil))
                     true  (into popped))
                   (cond-> out
                     ;; vcell is a cons for the map value
                     true  (conj! [:db/add cell :seq/next vcell])
                     ncell (conj! [:db/add vcell :seq/next ncell])))
            
            (let [nout (cond-> out
                         ;; vcell is a value cell for this coll/token node
                         cell  (conj! [:db/add cell :seq/first vcell])
                         ncell (conj! [:db/add cell :seq/next ncell])
                         true  (emit-cont vcell me (*store* me))
                         outer (conj! [:db/add outer :coll/contains vcell]))
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
   (sdloop limit
           [(LoopState. in at (list root) :root)]
           (transient []))))


(defn rplacd
  "`root` must be a seq. cells after `at` summarily unlinked"
  [root limit in at]
  (sdloop limit
          [(LoopState. in at root :root)]
          (transient [])))

(defn continue
  [root limit coll cell-eid]
  (sdloop limit
          [(LoopState. (:db/id coll)
                       cell-eid
                       root
                       (when (not= :map (:coll/type coll))
                         :root))]
          (transient [])))


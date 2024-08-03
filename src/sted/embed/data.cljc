(ns sted.embed.data
  (:require
   ;; [datascript.db :as db #?@(:cljs [:refer [Datom]])]
   [sted.embed.common :as ec])
  #?(:clj (:import [java.util UUID]))
  ;; #?(:clj (:import [datascript.db Datom]))
  
  )

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
           (or #?(:cljs (not-empty (some-> e (.-constructor ) (.-name)))
                  :clj (str (class e)))
               (do
                 #?(:cljs (js/console.log "What are you?"  e))
                 "object"))))))

(defn ->coll-type
  [e]
  (cond
    (list? e)           :list
    (map-entry? e)      :vec
    (vector? e)         :vec
    (map? e)            :map
    (set? e)            :set
    (sequential? e)     :list
    
    ;; (instance? Datom e) :vec
    ))

(defrecord LoopState [
                      outer       ; eid which coll/contains us
                      cell        ; cons cell allocated for us to fill
                      rest        ; (cons us after-us)
                      lazy?       ; everything is lazy except for map entries
                      ])

#?(:cljs
   (defn- to-hex-string [n l]
     (let [s (.toString n 16)
           c (count s)]
       (cond
         (> c l) (subs s 0 l)
         (< c l) (str (apply str (repeat (- l c) "0")) s)
         :else   s))))

(defn- rand-bits [pow]
  (rand-int (bit-shift-left 1 pow)))

(defn squuid
  ([]
   (squuid #?(:clj  (System/currentTimeMillis)
              :cljs (.getTime (js/Date.)))))
  ([msec]
   #?(:clj
      (let [uuid     (UUID/randomUUID)
            time     (int (/ msec 1000))
            high     (.getMostSignificantBits uuid)
            low      (.getLeastSignificantBits uuid)
            new-high (bit-or (bit-and high 0x00000000FFFFFFFF)
                             (bit-shift-left time 32))]
        (UUID. new-high low))
      :cljs
      (uuid
       (str
        (-> (int (/ msec 1000))
            (to-hex-string 8))
        "-" (-> (rand-bits 16) (to-hex-string 4))
        "-" (-> (rand-bits 16) (bit-and 0x0FFF) (bit-or 0x4000) (to-hex-string 4))
        "-" (-> (rand-bits 16) (bit-and 0x3FFF) (bit-or 0x8000) (to-hex-string 4))
        "-" (-> (rand-bits 16) (to-hex-string 4))
        (-> (rand-bits 16) (to-hex-string 4))
        (-> (rand-bits 16) (to-hex-string 4)))))))


(def ref-storage (atom {}))

(defn h
  [k]
  (get @ref-storage k))

(defn ^:dynamic *store*
  ;; generate a key unconditionally
  ([] (squuid))
  ;; generate a key optionally
  ([v] (when (meta v) (squuid)))
  ;; store ptr under key
  ([key ptr]
   (println "Store" key ptr)
   (swap! ref-storage assoc key ptr)))

(defn emit-tk
  [out eid t v]
  (-> out
      (conj! [:db/add eid :token/type t])
      (conj! [:db/add eid :token/value v])))

(defn emit1
  [out eid coll-type v]
  (if coll-type
    (conj! out [:db/add eid :coll/type coll-type])
    
    (do 
     (println "Emit1!" "v=" v "tt" (->token-type v))
     (if-let [tt (->token-type v)]
       ;; this is something we understand like a number or something
       (emit-tk out eid tt (->token-value tt v))
       ;; something weird
      
       ;; store it with a handle
       #_(let [tt :verbatim
               k (*store*)]
        
           (*store* k v)
        
           (-> out
               (emit-tk eid tt (->token-value tt v))
               (conj! [:db/add eid :handle/token k ])))
      
       ;; emit normal tokens  
       (do
         (println "NoHandle" v)
         (emit-tk out eid :verbatim (->token-value :verbatim v)))))))

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
        ;; finished with input
        (empty? rest)
        (recur ncount popped out)
        
        ;; reached the limit - we need to:
        ;; - unwind the stack
        ;; - store breadcrumbs  
        ;; - NOT evaluate anything we shouldn't
        (and lazy? (> 1 ncount))
        (let [car  (ec/new-tempid)
              ct   (->coll-type me)
              nout (-> out
                       (conj! [:db/add cell :seq/first car])
                       #_(conj! [:db/add cell :seq/first car]))]
          (recur 0 popped
                 (if (->coll-type me)
                   ;; save a continuation for the coll tail
                   (-> out
                       (conj! [:db/add cell :seq/first car])
                       (conj! [:db/add outer :coll/contains car])
                       (emit1 car nil '...C)
                       (emit-cont car (cons nil rest) (*store*)))
                   ;; recursive case
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
        (let [more  (next rest)
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
            ;; all other coll types
            (let [_ (println "Nout NormalColl")
                  nout (cond-> out
                         ;; vcell is a value cell for this coll/token node
                         cell  (conj! [:db/add cell :seq/first vcell])
                         ncell (conj! [:db/add cell :seq/next ncell])
                         ;; add a handle/token if the one-argument store-fn is interested 
                         true  (emit-cont vcell me (*store* me))
                         outer (conj! [:db/add outer :coll/contains vcell]))
                  npopped (cond-> popped
                            ncell (conj (LoopState. outer ncell more :lazy-tail)))
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

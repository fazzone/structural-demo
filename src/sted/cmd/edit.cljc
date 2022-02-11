(ns sted.cmd.edit
  (:require
   [datascript.core :as d]
   [sted.embed :as e]
   [sted.core :refer [get-selected-form move-selection-tx]]
   [sted.cmd.move :as move]))

(defn exactly-one
  [[x & more :as xs]]
  (when more
    (throw (ex-info (str "More than one: " (pr-str (map :db/id xs)))
                    {:xs xs}))
    #_(println "More than one" xs))
  x)

(defn form-delete*
  ([e] (form-delete* e true))
  ([e r?]
   (let [spine (first (:seq/_first e))
         next  (some-> spine :seq/next)
         prev  (some-> spine :seq/_next exactly-one)]
     (into [(if r?
              [:db/retractEntity (:db/id e)]
              (let [coll (exactly-one (:coll/_contains e))]
                [:db/retract (:db/id coll) :coll/contains (:db/id e)]))
            [:db/retract (:db/id spine) :seq/first (:db/id e)]]
           (cond
             (and prev next)            ; middle element
             [[:db/retract (:db/id spine) :seq/next (:db/id next)]
              [:db/add (:db/id prev) :seq/next (:db/id next)]]
             prev                       ; last element
             [[:db/retract (:db/id prev) :seq/next (:db/id spine)]
              [:db/retract (:db/id spine) :seq/next (:db/id next)]]
             next                       ; first element
             [[:db/add (:db/id spine) :seq/first (:db/id (:seq/first next))]
              ;; keep our spine, retract next spine - preserve coll/type etc
              [:db/retractEntity (:db/id next)]
              (if-let [n (:seq/next next)]
                [:db/add (:db/id spine) :seq/next (:db/id n)]
                [:db/retract (:db/id spine) :seq/next (:db/id next)])])))))

(defn form-delete-tx [e] (form-delete* e true))

(defn form-unlink-tx [e] (form-delete* e false))

;; overwrite with something that has not existed before


;; if you have a tempid, you want this one


(defn form-overwrite-tx
  "rplaca"
  [e replacement-eid]
  (let [spine (first (:seq/_first e))
        coll (first (:coll/_contains e))]
    #_(println "Overwrite" spine coll)
    (when (and spine coll)
      [[:db/retract (:db/id coll) :coll/contains (:db/id e)]
       [:db/add (:db/id coll) :coll/contains replacement-eid]
       [:db/add (:db/id spine) :seq/first replacement-eid]])))

;; replace with something from somewhere else


;; if you have a second entity, you want this one


(defn form-replace-tx
  [e r]
  (let [old-parent (exactly-one (:coll/_contains r))
        old-spine (exactly-one (:seq/_first r))]
    (into (form-overwrite-tx e (:db/id r))
          [(when old-spine
             [:db/retract (:db/id old-spine) :seq/first (:db/id r)])
           (when old-parent
             [:db/retract (:db/id old-parent) :coll/contains (:db/id r)])])))




(defn insert-after-tx
  [target new-node]
  (when-let [spine (first (:seq/_first target))]
    (when-let [coll (exactly-one (:coll/_contains target))]
      [{:db/id (:db/id spine)
        :seq/next {:db/id "cons"
                   :seq/first (:db/id new-node)}}
       (when-let [old-next (:seq/next spine)]
         [:db/add "cons" :seq/next (:db/id old-next)])
       {:db/id (:db/id coll)
        :coll/contains (:db/id new-node)}])))

(defn insert-before-tx
  [target new-node]
  (let [spine (exactly-one (:seq/_first target))
        coll (exactly-one (:coll/_contains target))]
    #_(println "IBTX" spine coll)
    (when (and spine coll)
      [{:db/id (:db/id spine)
        :seq/first (:db/id new-node)
        :seq/next {:db/id "insert-before-cons"
                   :seq/first (:db/id target)}}
       [:db/add (:db/id coll) :coll/contains (:db/id new-node)]
       (when-let [next (:seq/next spine)]
         [:db/add "insert-before-cons" :seq/next (:db/id next)])])))

(defn insert-into-empty-tx
  [target new-node]
  (when (:seq/first target)
    (throw (ex-info "Not empty" {})))
  [[:db/add (:db/id target) :coll/contains (:db/id new-node)]
   [:db/add (:db/id target) :seq/first (:db/id new-node)]])



(defn insert-editing*
  [inserter target from props]
  (let [new-node (merge {:db/id "newnode"
                         :form/editing true
                         :form/edit-initial ""}
                        props)]
    (into [new-node]
          (concat (inserter target new-node)
                  (move-selection-tx (:db/id from) (:db/id new-node))))))

(defn exchange-with-previous-tx
  [sel]
  #_(println "XWP" (:db/id sel))
  (let [spine  (exactly-one (:seq/_first sel))
        prev   (some-> spine :seq/_next exactly-one)
        next   (some-> spine :seq/next)
        parent (exactly-one (:coll/_contains sel))]
    (when (and prev parent)
      [[:db/add (:db/id prev) :seq/first (:db/id sel)]
       [:db/add (:db/id spine) :seq/first (:db/id (:seq/first prev))]
       [:db/add (:db/id parent) :form/edited-tx :db/current-tx]])))

(defn exchange-with-next-tx
  [sel]
  (let [spine  (exactly-one (:seq/_first sel))
        next   (some-> spine :seq/next)
        parent (exactly-one (:coll/_contains sel))]
    (when (and next parent)
      [[:db/add (:db/id next) :seq/first (:db/id sel)]
       [:db/add (:db/id spine) :seq/first (:db/id (:seq/first next))]
       [:db/add (:db/id parent) :form/edited-tx :db/current-tx]])))

(def duplicated-attrs [:form/indent
                       :form/linebreak])

(defn form-duplicate-tx
  [e]
  (letfn [(dup-spine [parent head]
            (if-let [x (:seq/first head)]
              (cond-> {:seq/first (assoc (form-duplicate-tx x) :coll/_contains parent)}
                (:seq/next head) (assoc :seq/next (dup-spine parent (:seq/next head))))))]
    
    (merge (select-keys e duplicated-attrs)
           (cond
             (:token/type e) {:token/type  (:token/type e)
                              :token/value (:token/value e)}
             (:coll/type e)  (let [us (e/new-tempid)]
                               (merge {:db/id us :coll/type (:coll/type e)}
                                      (dup-spine us e)))))))

(defn duplicate-alias
  [e]
  (when (= :alias (:coll/type e))
    {:coll/type :alias
     :alias/of (:db/id (:alias/of e))}))



(defn insert-duplicate-tx
  [sel]
  (let [new-node (-> (or (duplicate-alias sel)
                         (form-duplicate-tx sel))
                     (update :db/id #(or % "dup-leaf")))]
    (into [new-node]
          (insert-after-tx sel new-node))))

(defn form-wrap-tx
  [e ct]
  (let [{:form/keys [linebreak indent]} e]
    (into [(cond-> {:db/id "newnode"
                    :coll/type ct
                    :coll/contains (:db/id e)
                    :seq/first (:db/id e)}
             linebreak (assoc :form/linebreak linebreak)
             indent (assoc :form/indent indent))
           (when linebreak
             [:db/retract (:db/id e) :form/linebreak linebreak])
           (when indent
             [:db/retract (:db/id e) :form/indent indent])]
          (form-overwrite-tx e "newnode"))))

(defn begin-edit-existing-node-tx
  [e]
  [[:db/add (:db/id e) :form/editing true]])

(defn edit-new-wrapped*
  [inserter target from ct init opts]
  (let [new-node (merge opts
                        {:db/id "newnode"
                         :coll/type ct
                         :coll/contains #{"inner"}
                         :seq/first {:db/id "inner"
                                     :form/edit-initial (or init  "")
                                     :form/editing true}})]
    (into [new-node]
          (concat (inserter target new-node)
                  (move-selection-tx (:db/id from) "inner")))))

(defn edit-new-wrapped-tx
  ([sel ct init opts]
   (edit-new-wrapped* insert-after-tx sel sel ct init opts))
  ([target from ct init opts]
   (edit-new-wrapped* insert-after-tx target from ct init opts)))

(defn move-last*
  [e]
  (if-let [n (:seq/next e)]
    (recur n)
    (:seq/first e)))

(defn slurp-right-tx
  [e]
  (let [spine     (some-> e :seq/_first exactly-one)
        coll      (some-> e :coll/_contains exactly-one)
        next      (some-> spine :seq/next)
        nnext     (some-> next :seq/next)
        last-cons (loop [s e]
                    (if-let [n (:seq/next s)]
                      (recur n)
                      s))]
    (cond
      (nil? (:coll/type e)) (recur coll)
      (nil? next)           nil
      :else
      [[:db/retract (:db/id coll) :coll/contains (:db/id (:seq/first next))]
       [:db/add (:db/id e) :coll/contains (:db/id (:seq/first next))]
       (when-not (:seq/first e)
         [:db/retractEntity (:db/id next)])
       (if-not (:seq/first e)
         [:db/add (:db/id e) :seq/first (:db/id (:seq/first next))]
         [:db/add (:db/id last-cons) :seq/next (:db/id next)])
       (when nnext
         [:db/retract (:db/id next) :seq/next (:db/id nnext)])
       (if nnext
         [:db/add (:db/id spine) :seq/next (:db/id nnext)]
         [:db/retract (:db/id spine) :seq/next (:db/id next)])])))

(defn barf-right-tx
  [e]
  (let [spine     (some-> e :seq/_first exactly-one)
        coll      (some-> e :coll/_contains exactly-one)
        next      (some-> spine :seq/next)
        last-cons (loop [s e]
                    (if-let [n (:seq/next s)]
                      (recur n)
                      s))
        penult    (some-> last-cons :seq/_next exactly-one)]
    (cond
      (nil? (:coll/type e)) (recur coll)
      (= :bar (:coll/type coll)) nil
      (= (:db/id e) (:db/id last-cons))
      (when (:seq/first e)
        [[:db/retract (:db/id e) :coll/contains (:db/id (:seq/first e))]
         [:db/retract (:db/id e) :seq/first (:db/id (:seq/first e))]
         [:db/add (:db/id coll) :coll/contains (:db/id (:seq/first e))]
         {:db/id (:db/id spine)
          :seq/next {:db/id     "bnc"
                     :seq/first (:db/id (:seq/first e))}}
         (when next
           [:db/add "bnc" :seq/next (:db/id next)])])
      :else
      [[:db/add (:db/id coll) :coll/contains (:db/id (:seq/first last-cons))]
       [:db/retract (:db/id e) :coll/contains (:db/id (:seq/first last-cons))]
       [:db/add (:db/id spine) :seq/next (:db/id last-cons)]
       (when penult
         [:db/retract (:db/id penult) :seq/next (:db/id last-cons)])
       (if next
         [:db/add (:db/id last-cons) :seq/next (:db/id next)])])))

(defn insert-editing-before
  ([sel] (insert-editing-before sel sel))
  ([target from] (insert-editing-before target from nil))
  ([target from props]
   (let [parent (exactly-one (:coll/_contains target))]
     (if (= :chain (:coll/type parent))
       (edit-new-wrapped* insert-before-tx target from :list "" props)
       (let [indent-props (select-keys target [:form/linebreak :form/indent])]
         (insert-editing* insert-before-tx target from
                          (merge indent-props props)))))))

(defn insert-editing-after
  ([sel] (insert-editing-after sel sel))
  ([target from] (insert-editing-after target from nil))
  ([target from props]
   (let [parent (exactly-one (:coll/_contains target))]
     (if (= :chain (:coll/type parent))
       (edit-new-wrapped* insert-after-tx target from :list "" props)
       (let [indent-props (select-keys target [:form/linebreak :form/indent])]
         (insert-editing* insert-after-tx target from
                          (cond->> props
                            (:coll/type target) (merge indent-props))))))))


(defn form-split-tx
  [e]
  ;; split the parent, making sel the first of a new coll of the same type
  (let [spine       (some-> e :seq/_first exactly-one)
        next        (some-> spine :seq/next)
        coll        (some-> e :coll/_contains exactly-one)
        outer-coll  (some-> coll :coll/_contains exactly-one)
        outer-spine (some-> coll :seq/_first exactly-one)
        prev        (some-> spine :seq/_next exactly-one)
        tail        (e/seq->vec spine)]
    (when outer-coll
      (if (nil? prev)
        (if-let [n (:seq/first next)]
          (recur n))
        (into [{:db/id          "cons"
                :seq/first (:db/id spine)}
               [:db/add (:db/id spine) :coll/type (:coll/type coll)]
               [:db/add (:db/id outer-coll) :coll/contains (:db/id spine)]
               [:db/add (:db/id outer-spine) :seq/next "cons"]
               (when prev
                 [:db/retract (:db/id prev) :seq/next (:db/id spine)])
               (when-let [sn (:seq/next spine)]
                 [:db/add "cons" :seq/next (:db/id sn)])]
              (concat
               (for [t tail] [:db/retract (:db/id coll) :coll/contains (:db/id t)])
               (for [t tail] [:db/add (:db/id spine) :coll/contains (:db/id t)])))))))

(defn form-splice-tx
  ([e] (form-splice-tx (move/move :move/up e) e))
  ([e from]
   (let [spine (some-> e :seq/_first exactly-one)
         coll (some-> e :coll/_contains exactly-one)
         prev (some-> spine :seq/_next exactly-one)
         next (some-> spine :seq/next)
         elems (e/seq->vec e)
         last-cons (loop [s e]
                     (if-let [n (:seq/next s)]
                       (recur n)
                       s))]
     (cond
       (= :chain (:coll/type e)) (do (println "Cannot splice chain") nil)
       (nil? (:coll/type e)) (recur coll (or from e))
       :else
         (into
           [[:db/retractEntity (:db/id e)]
            (when-let [f (:seq/first e)]
              [:db/add (:db/id spine) :seq/first (:db/id f)])
            (when-let [n (:seq/next e)]
              [:db/add (:db/id spine) :seq/next (:db/id n)])
            (when next [:db/add (:db/id last-cons) :seq/next (:db/id next)])]
           (concat
             (for [x elems] [:db/add (:db/id coll) :coll/contains (:db/id x)])
             (move-selection-tx
               (:db/id from)
               (:db/id (or (when (not= (:db/id from) (:db/id e)) from)
                           (:seq/first e)
                           prev
                           coll)))))))))

(defn offer-tx
  [sel]
  (cond
    (:token/type sel)
    [[:db/retract (:db/id sel) :token/value (:token/value sel)]
     [:db/add (:db/id sel) :form/editing true]]
    (nil? (:seq/first sel))
    (insert-editing* insert-into-empty-tx sel sel nil)
    :else
    (let [last-cons (loop [s sel]
                      (if-let [n (:seq/next s)]
                        (recur n)
                        s))]
      (insert-editing-after (:seq/first last-cons) sel))))

(defn form-cons-tx
  [elem coll]
  (if-let [f (:seq/first coll)]
    (insert-before-tx f elem)
    [[:db/add (:db/id coll) :coll/contains (:db/id elem)]
     [:db/add (:db/id coll) :seq/first (:db/id elem)]]))

(defn m-to-dc
  [sel]
  (when-let [dch (d/entity (d/entity-db sel) :sted.page/command-chain)]
    (form-cons-tx sel dch)))

(defn form-raise-tx
  [e]
  (when-let [parent (some-> (:coll/_contains e) exactly-one)]
    (when-not (= :chain (:coll/type parent))
      (let [{:form/keys [linebreak indent]} parent
            ts {:db/id "tombstone" :coll/type :tear}]
        (into [ts
               [:db/add (:db/id parent) :form/edited-tx :db/current-tx]
               (when linebreak [:db/add (:db/id e) :form/linebreak linebreak])
               (when indent [:db/retract (:db/id parent) :form/indent indent])
               (when indent [:db/add (:db/id e) :form/indent indent])]
              (concat (form-overwrite-tx e (:db/id ts))
                      (form-replace-tx parent e)
                      (m-to-dc parent)))))))


(defn unraise-tx
  [sel]
  (let [find-tombstone #(->> (e/seq->vec %)
                             (filter (comp #{:tear} :coll/type))
                             (first))
        ts (some->> :sted.page/command-chain
                    (d/entity (d/entity-db sel))
                    (e/seq->vec)
                    (some find-tombstone))
        p (some-> ts :coll/_contains exactly-one)]
    
    (when (and p ts)
      (concat (form-unlink-tx p)
              (form-replace-tx ts sel)
              (form-replace-tx sel p)))))


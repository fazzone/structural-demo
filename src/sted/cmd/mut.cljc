(ns sted.cmd.mut
  (:require
   [sted.embed.common :as ec]
   [clojure.string :as string]
   [datascript.core :as d]
   [sted.embed :as e]
   [sted.cmd.move :as move]
   [sted.cmd.edit :as edit]
   [sted.cmd.insert :as insert]
   [sted.sys.handle :as sh]
   [sted.cmd.nav :as nav]
   [sted.cmd.tear :as tear]
   [sted.embed.data :as sed]
   [sted.embed.except :as embed-except]
   #_[sted.embed.md :as emd]
   [sted.core :as core :refer [get-selected-form
                               move-selection-tx]]))


(defn select-form-tx
  [db eid]
  (when eid
   (move-selection-tx (:db/id (get-selected-form db))
                      eid)))


;; second movement type is plan B in case we are asked to delete first/last of chain


(defn move-and-delete-tx
  [src mva mvb]
  (when-let [dst (or (mva src)
                     (and mvb (mvb src)))]
    (concat (edit/form-delete-tx src)
            [[:db/add (:db/id dst) :form/highlight true]])))


(defn indent-selected-form-tx
  [sel delta]
  [[:db/add (:db/id sel)
    :form/indent (+ delta
                    (-> (:form/indent sel)
                        (or 0)))]])


(defn linebreak-selected-form-tx
  [sel]
  (let [pre-indent (:form/indent sel)
        parent-indent (:form/indent (move/up sel))
        pre-lb (:form/linebreak sel)]
    (println "Linebreak" "Parent indent" parent-indent)
    (cond
      pre-lb [[:db/retract (:db/id sel) :form/linebreak true]
              [:db/retract (:db/id sel) :form/indent]]
      :else [[:db/add (:db/id sel) :form/linebreak true]
             (when parent-indent
               [:db/add (:db/id sel) :form/indent parent-indent])])))


(defn recursive-multiline
  [sel]
  (letfn [(fnlike [tt]
                  (case tt
                    (:symbol :keyword) true
                    false))
          (+txe [acc e lb ind]
                (let [pre-ind (:form/indent e)
                      pre-lb (:form/linebreak e)
                      d-lb (if-not lb nil lb)
                      d-ind (if (zero? ind) nil ind)
                      nop-ind (= d-ind pre-ind)
                      nop-lb (= (some? lb) (some? pre-lb))]
                  (if (and nop-ind nop-lb)
                    acc
                    (cond-> acc
                      (not nop-lb)
                      (conj (if-not d-lb
                              [:db/retract (:db/id e) :form/linebreak]
                              [:db/add (:db/id e) :form/linebreak d-lb]))
                      (and d-lb (not nop-ind))
                      (conj (if (and pre-ind (nil? d-lb) (nil? d-ind))
                              [:db/retract (:db/id e) :form/indent]
                              [:db/add (:db/id e) :form/indent d-ind]))))))
          (go [[e & es] ind idx acc]
              (cond
                (nil? e) acc
                (nil? (:coll/type e))
                (recur es
                       ind
                       (inc idx)
                       (+txe acc e (not (zero? idx)) #_(not (and (zero? idx) (fnlike (:token/type e)))) ind))
                :else
                (let [ch (e/seq->vec e)
                      rec
                      (go ch
                          (+ ind (count (e/open-delim (:coll/type e))))
                          0
                          (+txe [] e (not (zero? idx)) ind))]
                  (if-not es
                    (recur ch
                           (+ 2 ind)
                           0
                           (into acc rec))
                    (recur es ind (inc idx) (into acc rec))))))]
    (go (e/seq->vec sel) 2 0 [])))


(defn recursive-oneline-tx
  [sel]
  (->> sel
       (tree-seq :coll/contains :coll/contains)
       (next)
       (keep (fn [e]
               #_(prn (keys e))
               (when (:form/linebreak e)
                 (cond-> [[:db/retract (:db/id e) :form/linebreak (:form/linebreak e)]]
                   (:form/indent e) (conj [:db/retract (:db/id e) :form/indent])))))
       (reduce into)))


(defn delete-by-eid
  [db eid]
  (let [e (d/entity db eid)]
    (if-not (:form/highlight e)
      (edit/form-delete-tx e)
      (move-and-delete-tx db move/backward-up move/next-sibling))))


(defn insert-txdata
  [et c]
  (let [top-level (peek (nav/parents-vec et))
        prev (move/prev-sibling top-level)
        result-node (update c :db/id #(or % "import-formdata-tx"))]
    (into [result-node]
          (edit/insert-before-tx top-level result-node))))


(defn toggle-hide-show
  [e]
  (let [ct (:coll/type e)
        hct (:hidden/coll-type e)]
    (cond
      hct
      [[:db/retract (:db/id e) :hidden/coll-type hct]
       [:db/add (:db/id e) :coll/type hct]]
      ct
      [[:db/add (:db/id e) :hidden/coll-type ct]
       [:db/add (:db/id e) :coll/type :hidden]])))


(defn replace-with-pr-str
  [sel]
  (let [new-node {:db/id       "eval-result"
                  :token/type  :string
                  :token/value (pr-str (e/->form sel))}]
    (prn "RPPRS" new-node)
    (into [new-node]
          (concat
           (edit/form-overwrite-tx sel "eval-result")
           (move-selection-tx (:db/id sel) "eval-result")))))


(defn unary-arith
  [f e]
  (when (= :number (:token/type e))
    [[:db/add (:db/id e) :token/value (f (:token/value e))]]))


(def plus*  (partial unary-arith inc))


(def minus* (partial unary-arith dec))


(defn hoist-tx
  [sel]
  (let [ps (nav/parents-vec sel)]
    (when (< 1 (count ps))
      (into (edit/form-unlink-tx sel)
            (concat (edit/insert-before-tx (peek ps) sel)
                    (some->> (move/backward-up sel)
                             (:db/id)
                             (move-selection-tx (:db/id sel))))))))

#_(defn hoist-tx
  [sel]
  (let [ps (nav/parents-vec sel)]
    (when (< 1 (count ps))
      (into (edit/form-unlink-tx sel)
            (edit/insert-before-tx (peek ps) sel)))))


(defn gobble*
  [sel gt]
  (into (edit/form-unlink-tx gt)
        (concat (edit/insert-after-tx sel gt)
                (move-selection-tx (:db/id sel) (:db/id gt)))))


(defn gobble-tx
  [sel]
  (some->> sel
           (nav/parents-vec)
           (peek)
           (move/prev-sibling)
           (gobble* sel)))


(defn move-to-deleted-chain
  [sel]
  (when-let [dch (d/entity (d/entity-db sel) :sted.page/command-chain)]
    (when-let [nsel (move/backward-up sel)]
      (concat (edit/form-unlink-tx sel)
              (edit/form-cons-tx sel dch)
              (move-selection-tx (:db/id sel) (:db/id nsel))))))


(defn uneval-and-next
  [sel]
  (let [dst (move/next-sibling sel)]
    (concat (edit/form-wrap-tx sel :uneval)
            (when dst (move-selection-tx (:db/id sel) (:db/id dst))))))


(defn find-next*
  [me ir]
  (loop [[[e a v t] & more] ir]
    (cond
      (nil? e)            nil
      (and more (= e me)) (first (first more))
      :else (recur more))))


(defn find-next
  [sel]
  (when-let [text (:token/value sel)]
    (let [db  (d/entity-db sel)
          ln  (str text "􏿿")
          ir  (filter (fn [[e a v t]] (= v text)) (d/index-range db :token/value text ln))
          fnf (find-next* (:db/id sel) ir)]
      (d/entity
       (d/entity-db sel)
       (or fnf
           (when (next ir)
             (first (first ir))))))))


(defn find-first
  ([sel]
   (when-let [text (:token/value sel)]
     (let [ln  (str text "􏿿")
           ir  (d/index-range (d/entity-db sel) :token/value text ln)
           dst (apply min (map first ir))]
       (when (and dst (not= dst (:db/id sel)))
         (d/entity (d/entity-db sel) dst))))))


(defn make-alias-tx
  [sel a-eid]
  (let [ps (nav/parents-vec sel)
        top-level (peek ps)
        tempid "alias"
        of-eid (or a-eid (:db/id sel))
        new-node {:db/id tempid
                  :coll/type :alias
                  :alias/of of-eid}]
    (cond
      (some #(= % of-eid) (next ps))
      (do (println "alias bad parent")
          nil)
      ;; you could make an alias of an alias,
      ;; but it's more useful to go to the aliased form
      (= :alias (:coll/type sel))
      (move-selection-tx (:db/id sel) (:db/id (:alias/of sel)))
      :else
      (into [new-node]
            (concat
             (edit/insert-before-tx top-level new-node)
             (move-selection-tx (:db/id sel) tempid))))))

(defn new-bar-tx
  [sel]
  (let [new-node {:db/id "newbar"
                  :coll/type :bar
                  :coll/contains #{"newchain"}
                  :seq/first {:db/id "newchain"
                              :coll/contains #{"newnode"}
                              :coll/type :chain
                              :seq/first {:db/id "newnode"
                                          :form/edit-initial ""
                                          :form/editing true}}}]
    (into [new-node]
          (concat (edit/insert-after-tx sel new-node)
                  (move-selection-tx (:db/id sel) "newnode")))))

(defn drag*
  [chain-mover chain-inserter sel]
  (let [top-level   (peek (nav/parents-vec sel))
        chain       (some-> top-level :coll/_contains edit/exactly-one)
        other-chain (chain-mover chain)
        target      (:chain/selection other-chain)]
    (into (edit/form-unlink-tx sel)
          (concat
           (if-let [ncs (or (move/next-sibling sel)
                            (move/prev-sibling sel))]
             [[:db/add (:db/id chain) :chain/selection (:db/id ncs)]]
             [[:db/retract (:db/id chain) :chain/selection]])
           (if-let [top (peek (nav/parents-vec target))]
             (edit/insert-before-tx top sel)
             (if other-chain
               (edit/form-cons-tx sel other-chain)
               (let [nc {:db/id         "newchain"
                         :coll/type     :chain
                         :coll/contains (:db/id sel)
                         :seq/first     (:db/id sel)}]
                 (into [nc] (chain-inserter chain nc)))))))))


(defn chain-from-text
  [sel text props]
  (let [top-level   (peek (nav/parents-vec sel))
        chain       (some-> top-level :coll/_contains edit/exactly-one)
        ;; _ (js/console.time "S->tx")
        stx (e/string->tx-all text)
        ;; _ (js/console.timeEnd "S->tx")
        new-node (-> stx
                     (update :db/id #(or % "cft"))
                     (assoc :coll/type :chain)
                     (merge props))]
    (into [new-node]
          (edit/insert-after-tx chain new-node))))

(defn summon-tx
  [sel eid]
  (let [db       (d/entity-db sel)
        mychain  (some-> sel nav/parents-vec peek :coll/_contains edit/exactly-one)
        mcspine  (some-> mychain :seq/_first edit/exactly-one)
        mbar     (:db/id (edit/exactly-one (:coll/_contains mychain)))
        summoned (d/entity db eid)
        schain   (some-> summoned nav/parents-vec peek :coll/_contains edit/exactly-one)
        scspine  (some-> schain :seq/_first edit/exactly-one)
        sbar     (:db/id (edit/exactly-one (:coll/_contains schain)))]
    (when (and mcspine scspine (= (:db/id mbar) (:db/id sbar)))
      (into (move-selection-tx (:db/id sel) eid)
            (when-not (= (:db/id schain) (:db/id mychain))
              (into [[:db/add (:db/id mychain) :chain/selection (:db/id sel)]]
                    (edit/move-sibling-after-tx mychain schain)))))))


(def movement-commands
  {:select (fn [e eid] (d/entity (d/entity-db e) eid))
   :click  (fn [sel eid]
             (let [e (d/entity (d/entity-db sel) eid)]
               (cond
                 (= eid (:db/id sel))
                 (move/up e)
                 (nil? (:coll/type sel))
                 e
                 :else
                 (let [[_ parent]
                       (->> (nav/parents-vec e)
                            (drop-while
                             (fn [p]
                               (not= (:db/id p) (:db/id sel)))))]
                   (or parent e)))))
   :find-next       find-next
   :find-first      find-first
   :flow-right      move/flow
   :flow-left       move/flow-left
   :flow-right-coll (fn [sel]
                      (->> sel
                           (iterate move/flow)
                           (next)
                           (take-while some?)
                           (filter :coll/type)
                           (first)))
   :parent          move/up
   :toplevel        move/most-upward
   :next            move/next-sibling
   :prev            move/prev-sibling
   :tail            move/most-nested
   :m1              (partial nav/numeric 0)
   :m2              (partial nav/numeric 1)
   :m3              (partial nav/numeric 2)
   :m4              (partial nav/numeric 3)
   :m5              (partial nav/numeric 4)
   :m6              (partial nav/numeric 5)
   :m7              (partial nav/numeric 6)
   :m8              (partial nav/numeric 7)})


(defn new-comment-tx
  [sel]
  (into [[:db/add (:db/id sel) :form/linebreak true]]
        (edit/insert-editing*
         edit/insert-before-tx
         sel
         sel
         (cond-> {:token/type :comment
                  :form/linebreak true
                  :form/edit-initial ";; "}
           (:form/indent sel) (assoc :form/indent (:form/indent sel))))))


#_(defn new-comment-tx
  [sel]
  (edit/edit-new-wrapped-tx sel :md/root "" {}))


(defn store-fn
  ([] (str (d/squuid)))
  ([v]
   (when (meta v) (store-fn))
   
   #_nil
   
   )
  ([k v] (sh/store k v)))


(defn node-limit
  [db]
  (-> db
      (d/entity :sted.page/state)
      :state/limit
      (or 128)))


(defn continue
  [root limit coll spine]
  (binding [sed/*store* store-fn]
    (sed/continue root limit coll (:db/id spine))))


(defn go
  [root limit coll spine]
  (binding [sed/*store* store-fn]
    (sed/go root limit (:db/id coll) (:db/id spine))))


#_(defn ingest-markdown
  [sel md-text]
  (let [top-level   (peek (nav/parents-vec sel))
        chain       (some-> top-level :coll/_contains edit/exactly-one)
        txe (emd/md->tx md-text)
        new-node {:db/id "mdchain"
                  :coll/type :chain
                  :coll/contains (:db/id txe)
                  :seq/first (:db/id txe)}]
    (prn "DBIDTXE" (:db/id txe))
    (into [txe
           new-node]
          (edit/insert-after-tx chain new-node))))


(defn clone-to-before-parent
  [sel]
  (let [top      (peek (nav/parents-vec sel))
        new-node (-> (edit/form-duplicate-tx sel)
                     (update :db/id #(or % "dup-leaf"))
                     (dissoc :form/linebreak :form/indent))]
    (into [new-node]
          (concat (edit/insert-before-tx top new-node)
                  (move-selection-tx (:db/id sel) (:db/id new-node))))))


(defn new-command-do
  [sel]
  (let [new-node {:db/id         "newnode"
                  :coll/type     :list
                  :coll/contains #{"do" #_"comment" "inner"}
                  :eval/action   :eval-sci
                  :seq/first     {:db/id       "do"
                                  :token/type  :symbol
                                  :token/value "sted/do"}
                  :seq/next      {:seq/first {:db/id             "inner"
                                              :form/edit-initial ""
                                              :form/editing      true}}
                  #_             #_:seq/next {:seq/first {:db/id       "comment"
                                                          :token/type  :comment
                                                          :token/value "; cmd"}
                                              :seq/next  {:seq/first {:db/id             "inner"
                                                                      :form/edit-initial ""
                                                                      :form/editing      true}}}}]
    (into [new-node
           (when (:form/linebreak sel)
             [:db/add "newnode" :form/linebreak true])]
          (concat (edit/insert-before-tx sel new-node)
                  (move-selection-tx (:db/id sel) "inner")))))


(defn pop-deleted-chain
  [sel]
  (let [dch (d/entity (d/entity-db sel) :sted.page/command-chain)
        rez (some-> dch :seq/first)]
    (when rez
      (concat (edit/form-unlink-tx rez)
              (edit/insert-after-tx sel rez)
              (move-selection-tx (:db/id sel) (:db/id rez))))))


;; EVAL


(defn eval-result
  [sel et-eid data]
  (let [db                    (d/entity-db sel)
        et                    (d/entity db et-eid)
        target                (peek (nav/parents-vec et))
        spine                 (edit/exactly-one (:seq/_first target))
        coll                  (edit/exactly-one (:coll/_contains target))
        nn                    {:db/id     "ncell"
                               :seq/_next (:db/id spine)}
        nl                    (node-limit db)
        inner                 (go data nl coll nn)
        [_ [_ _ _ inner-eid]] inner]
    (when (and spine coll)
      (->> inner
           (into [nn
                  [:db/add inner-eid :eval/of et-eid]
                  (when-let [old-next (:seq/next spine)]
                    [:db/add (:db/id nn) :seq/next (:db/id old-next)])])))))


(defn eval-result-fat
  [sel et-eid data]
  (let [db                    (d/entity-db sel)
        et                    (d/entity db et-eid)
        target                (peek (nav/parents-vec et))
        spine                 (edit/exactly-one (:seq/_first target))
        coll                  (edit/exactly-one (:coll/_contains target))
        txe                   {:db/id            "ncell"
                               :coll/type        :eval-result
                               :eval/result-type :ok
                               :eval/of          et-eid}
        inner                 (binding [sed/*store* store-fn]
                                (sed/go data (node-limit db) "ncell" "ncell"))
        [_ [_ _ _ inner-eid]] inner]
    (when (and spine coll)
      (into [txe]
            (concat (edit/insert-after-tx et txe)
                    inner)))))


(defn eval-result-pending
  [sel et-eid]
  (let [db     (d/entity-db sel)
        et     (d/entity db et-eid)
        target (peek (nav/parents-vec et))
        spine  (edit/exactly-one (:seq/_first target))
        coll   (edit/exactly-one (:coll/_contains target))
        txe    {:db/id            "ncell"
                :coll/type        :eval-result
                :eval/result-type :pending
                :eval/of          et-eid}]
    (when (and spine coll)
      (into [txe]
            (edit/insert-after-tx et txe)))))


(defn eval-finish
  [sel er-eid data]
  (into [[:db/add er-eid :eval/result-type :ok]]
        (binding [sed/*store* store-fn]
          (sed/rplacd (list data) (node-limit (d/entity-db sel)) er-eid er-eid))))


(defn eval-error [sel er-eid ex]
                            (let [db  (d/entity-db sel)
                                  er  (d/entity db er-eid)
                                  ans (binding [sed/*store* store-fn]
                                        (-> (embed-except/ex->data ex)
                                            (sed/rplacd (node-limit db) er-eid er-eid)))]
                              (into [[:db/add er-eid :eval/result-type :exception]]
                                    ans)))


(defn ingest-replacing
  [sel data]
  (let [db    (d/entity-db sel)
        spine (edit/exactly-one (:seq/_first sel))
        coll  (edit/exactly-one (:coll/_contains sel))]
    (when (and spine coll)
      (let [ans     (go data (node-limit db) coll spine)
            id-hack (some (fn [[_ e]]
                            (when (neg? e) e))
                          ans)]
        (into (select-form-tx db id-hack)
              ans)))))


(defn eval-cont
  [sel target data]
  ;; (js/console.log "Eval-cont" data)
  (let [db    (d/entity-db sel)
        spine (edit/exactly-one (:seq/_first target))
        coll  (edit/exactly-one (:coll/_contains target))]
    (println "Sp" (:db/id spine)
             "Col" (:db/id coll))
    (when (and spine coll)
      (let [sn (next data)]
        (if (nil? sn)
          (into (select-form-tx db (:db/id (move/backward-up target)))
                (edit/form-delete-tx target))
          (let [root    (if-not (:seq/next spine)
                          sn
                          (list sn))
                tx-data (binding [sed/*store* store-fn]
                          (continue root (node-limit db) coll spine))
                id-hack (some (fn [[_dbadd e a]]
                                (when (and (neg? e)
                                           (or (= a :token/type) (= a :coll/type)))
                                  e))
                              tx-data)]
            (into
             (select-form-tx db
                             (or id-hack
                                 (:db/id (move/backward-up target))))
             tx-data)))))))


(defn ->root
  [e]
  (loop [s e]
    (if-let [n (edit/exactly-one (:coll/_contains s))]
      (recur n)
      s)))


#_(defn back-to-edit
  [sel]
  (let [db (d/entity-db sel)
        bar-eid (:db/id (:state/bar (d/entity db :sted.page/state)))
        edit-eid (first (for [[txe e-of eid ttxe] (d/rseek-datoms db :aevt :edit/of)
                              :when (= e-of :edit/of)
                              :let [[[e _ v t]] (d/datoms db :eavt eid :token/value)]
                              :when (and (= e eid)
                                         (= t txe)
                                         (= bar-eid (:db/id (->root (d/entity db eid)))))]
                          eid))]
    (select-form-tx db edit-eid)))


(defn sorted-merged-map
  [f k xs ys]
  ((fn iter [[x :as xs] [y :as ys]]
     (cond
       (and (empty? xs) (empty? ys)) nil
       
       (empty? ys)     (cons (f x) (lazy-seq (iter (next xs) ys)))
       (empty? xs)     (cons (f y) (lazy-seq (iter xs (next ys))))
       (> (k x) (k y)) (cons (f y) (lazy-seq (iter xs (next ys))))
       :else           (cons (f x) (lazy-seq (iter (next xs) ys)))))
   xs
   ys))

(defn back-to-edit
  ([sel] (back-to-edit sel true))
  ([sel outside]
   (let [db (d/entity-db sel)
         sel-eid (:db/id sel)
         bar-eid (:db/id (:state/bar (d/entity db :sted.page/state)))
         ;; WRONG: this is the toplevle and not the chain...
         chain-eid (:db/id (move/most-upward sel))
         ;; max-tx (apply max
         ;;               (for [[_ a _ t] (d/datoms db :eavt (:db/id sel))
         ;;                     :when (not= :form/highlight a)]
         ;;                 t))
         qseq (for [[e a v t] (d/rseek-datoms db :avet :form/edited-tx #_max-tx)
                    :let [efe (d/entity db e)
                          _ (println "QSEQ" e a (.toString ^js v 16))]
                    :when (and (not (= e sel-eid ))
                               (= bar-eid (:db/id (->root efe)))
                               (or (not outside) (not (= chain-eid (:db/id (move/most-upward efe))))))]
                e)]
     (println "BACK!" sel outside "Sel=" sel-eid "Qseq" (first qseq))
     (select-form-tx db (first qseq)))))

(defn select-chain-selection-tx
  [sel]
  (let [top-level (move/most-upward sel)
        ch (some-> top-level :coll/_contains first)
        cs (:chain/selection ch)]
    (when cs
      (into [[:db/add (:db/id ch) :chain/selection (:db/id sel)]]
            (move-selection-tx (:db/id sel) (:db/id cs))))))


(def editing-commands
  {:float                          #'edit/exchange-with-previous-tx
   :sink                           edit/exchange-with-next-tx
   :select-chain                   nav/select-chain-tx
   :select-chain-selection         select-chain-selection-tx
   :hop-left                       nav/hop-left
   :hop-right                      nav/hop-right
   :uneval                         uneval-and-next
   :insert-right                   (fn [sel] (edit/insert-editing-cmd edit/insert-after-tx sel sel nil))
   :insert-left                    (fn [sel] (edit/insert-editing-cmd edit/insert-before-tx sel sel nil))
   :edit/reject                    (fn [sel] (let [db (d/entity-db sel)] (insert/reject-edit-tx db (d/entid db [:form/editing true]))))
   :edit/finish                    (fn [sel text] (let [db (d/entity-db sel)] (insert/finish-edit-tx db (d/entid db [:form/editing true]) text)))
   :edit/finish-and-move-up        (fn [sel text] (let [db (d/entity-db sel)] (insert/finish-edit-and-move-up-tx db (d/entid db [:form/editing true]) text)))
   :edit/finish-and-edit-next-node (fn [sel text] (let [db (d/entity-db sel)] (->> text (insert/finish-edit-and-edit-next-tx (d/entity db [:form/editing true])))))
   :edit/wrap                      (fn [sel ct value] (let [db (d/entity-db sel)] (insert/wrap-edit-tx (d/entity db [:form/editing true]) ct value)))
   :delete-left                    (fn [sel] (move-and-delete-tx sel move/backward-up move/next-sibling))
   :delete-right                   (fn [sel] (move-and-delete-tx sel move/forward-up move/prev-sibling))
   :raise                          edit/form-raise-tx
   :raise-parent                   (comp edit/form-raise-tx move/up)
   :clone                          edit/insert-duplicate-tx
   :clone-parent                   (fn [sel] (concat (edit/insert-duped-parent-before-tx sel) (edit/offer-tx sel)))
   :clone-to-before-parent         clone-to-before-parent
   :linebreak                      linebreak-selected-form-tx
   :wrap                           (fn [sel] (edit/form-wrap-tx sel :list))
   :wrap-vec                       (fn [sel] (edit/form-wrap-tx sel :vec))
   :wrap-map                       (fn [sel] (edit/form-wrap-tx sel :map))
   :indent                         (fn [sel] (indent-selected-form-tx sel 1))
   :dedent                         (fn [sel] (indent-selected-form-tx sel -1))
   :slurp-right                    edit/slurp-right-tx
   :barf-right                     edit/barf-right-tx
   :new-list                       (fn [sel] (edit/edit-new-wrapped-tx sel :list "" {}))
   :new-vec                        (fn [sel] (edit/edit-new-wrapped-tx sel :vec "" {}))
   :new-deref                      (fn [sel] (edit/edit-new-wrapped-tx sel :deref "" {}))
   :new-quote                      (fn [sel] (edit/edit-new-wrapped-tx sel :quote "" {}))
   :new-syntax-quote               (fn [sel] (edit/edit-new-wrapped-tx sel :syntax-quote "" {}))
   :new-unquote                    (fn [sel] (edit/edit-new-wrapped-tx sel :unquote "" {}))
   :new-meta                       (fn [sel] (edit/edit-new-wrapped-tx sel :meta "" {}))
   :new-comment                    new-comment-tx
   :open-chain                     chain-from-text
   :new-bar                        new-bar-tx
   :new-do                         new-command-do
   :hide                           (comp toggle-hide-show peek nav/parents-vec)
   :stringify                      replace-with-pr-str
   :plus                           plus*
   :minus                          minus*
   :insert-txdata                  insert-txdata
   :hoist                          hoist-tx
   :gobble                         gobble-tx
   :move-to-deleted-chain          move-to-deleted-chain
   :cycle-deleted-chain            (fn [sel] (when-let [dch (d/entity (d/entity-db sel) :sted.page/command-chain)] (edit/form-cycle-tx dch)))
   :pop-deleted-chain              pop-deleted-chain
   :tear                           tear/tear-tx
   :alias                          make-alias-tx
   :drag-left                      (partial drag* move/prev-sibling edit/insert-before-tx)
   :drag-left-create               (partial drag* (fn [_]) edit/insert-before-tx) 
   :drag-right                     (partial drag* move/next-sibling edit/insert-after-tx)
   :drag-right-create              (partial drag* (fn [_]) edit/insert-after-tx)
   :split                          edit/form-split-tx
   :splice                         edit/form-splice-tx
   :offer                          edit/offer-tx
   :multiline                      recursive-multiline
   :oneline                        recursive-oneline-tx
   :oneline-all                    (fn [sel] (some->> sel :coll/contains (mapcat recursive-oneline-tx)))
   :eval-result                    eval-result
   :eval-result-fat                eval-result-fat
   :eval-result-pending            eval-result-pending
   :eval-finish                    eval-finish
   :eval-error                     eval-error
   :ingest-replacing               ingest-replacing
   :eval-cont                      eval-cont
   :summon                         summon-tx
   :unraise                        edit/unraise-tx
   :compose                        edit/ffff
   :back-to-edit                   back-to-edit})

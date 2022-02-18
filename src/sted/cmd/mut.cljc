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
   #_[sted.loopy :as loopy]
   [sted.embed.data :as sed]
   [sted.embed.md :as emd]
   [sted.core :as core :refer [get-selected-form
                               move-selection-tx]]))

(defn select-form-tx
  [db eid]
  (when eid
   (move-selection-tx (:db/id (get-selected-form db))
                      eid)))

;; second movement type is plan B in case we are asked to delete first/last of chain
(defn move-and-delete-tx
  [db mva mvb]
  (let [src (get-selected-form db)]
    (when-let [dst (or (mva src)
                       (and mvb (mvb src)))]
      (concat (edit/form-delete-tx src)
              [[:db/add (:db/id dst) :form/highlight true]]))))

(defn indent-selected-form-tx
  [db delta]
  (let [sel (get-selected-form db)]
    [[:db/add (:db/id sel)
      :form/indent (+ delta
                      (-> (:form/indent sel)
                          (or 0)))]]))

(defn linebreak-selected-form-tx
  [db]
  (let [sel           (get-selected-form db)
        parent-indent (:form/indent (move/up sel))
        pre-lb        (:form/linebreak sel)]
    [(if pre-lb
       [:db/retract (:db/id sel) :form/linebreak true]
       [:db/add (:db/id sel) :form/linebreak true])
     (when (not pre-lb)
       [:db/retract (:db/id sel) :form/indent (:form/indent sel)])]))

(defn recursive-multiline
  [db]
  (letfn [(fnlike [tt]
            (case tt (:symbol :keyword) true false))
          (+txe [acc e lb ind]
            (let [pre-ind (:form/indent e)
                  pre-lb  (:form/linebreak e)
                  d-lb    (if-not lb nil lb)
                  d-ind   (if (zero? ind) nil ind)
                  nop-ind (= d-ind pre-ind)
                  nop-lb  (= (some? lb) (some? pre-lb))]
              (if (and nop-ind nop-lb)
                acc
                (cond-> acc
                  (not nop-lb)  (conj (if-not d-lb
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
                (recur es ind (inc idx) (+txe acc e
                                              (not (zero? idx))
                                              #_(not (and (zero? idx)
                                                          (fnlike (:token/type e))))
                                              ind))
                
                :else
                (let [ch  (e/seq->vec e)
                      rec (go ch
                              (+ ind (count (e/open-delim (:coll/type e))))
                              0
                              (+txe [] e (not (zero? idx)) ind))]
                  (if-not es
                    (recur ch (+ 2 ind) 0 (into acc rec))
                    (recur es ind (inc idx) (into acc rec))))))]
    (go (e/seq->vec (get-selected-form db)) 2 0 [])))


(defn recursive-oneline
  [db]
  (->> (get-selected-form db)
       (tree-seq :coll/contains :coll/contains)
       (next)
       (keep (fn [e]
               (prn (keys e))
               (when (:form/linebreak e)
                 (cond-> [[:db/retract (:db/id e) :form/linebreak (:form/linebreak e)]]
                   (:form/indent e) (conj [:db/retract (:db/id e) :form/indent ])))))
       (reduce into)))

(defn el-bfs
  [top limit]
  (loop [out   []
         front [top]]
    (cond
      (empty? front)        out
      (= limit (count out)) out
      :else                 (let [e (first front)]
                              (recur (cond-> out
                                       (:coll/type e) (conj e))
                                     (cond-> (subvec front 1)
                                       (:seq/next e)  (conj (:seq/next e))
                                       (:seq/first e) (conj (:seq/first e))))))))

(defn lazy-bfs
  [top]
  ((fn iter [[e :as front]]
     (when (some? e)
       (cond->> (lazy-seq (iter (cond-> (subvec front 1)
                                  (:seq/next e)  (conj (:seq/next e))
                                  (:seq/first e) (conj (:seq/first e)))))
         (or (:coll/type e) (:token/type e)) (cons e))))
   [top]))

(defn no-double-colls
  [nmv]
  ((fn iter [[x y :as xs] firsts]
     (when x
       (let [fid (:db/id (:seq/first x))
             add? (not (contains? firsts (:db/id x)))]
         (cond->> (lazy-seq (iter (next xs) (conj firsts fid)))
           add? (cons x)))))
   nmv #{}))

(defn get-numeric-movement-vec
  [sel]
  (let [children (next (take 8 (no-double-colls (lazy-bfs sel))))]
    (if (move/up sel)
      (into [(peek (nav/parents-vec sel))] children)
      (into [nil] children))))

(defn numeric-movement
  [n sel]
  (when-let [nmv (get-numeric-movement-vec sel)]
    (when (< -1 n (count nmv))
      (nth nmv n))))

(defn delete-by-eid
  [db eid]
  (let [e (d/entity db eid)]
    (if-not (:form/highlight e)
      (edit/form-delete-tx e)
      (move-and-delete-tx db move/backward-up move/next-sibling))))

(defn clear-one-eval
  [db]
  (let [sel (get-selected-form db)
        top-level (peek (nav/parents-vec sel))
        chain     (some-> top-level :coll/_contains edit/exactly-one)
        ch-set (->> (:db/id chain)
                    (d/datoms db :avet :coll/contains)
                    (into #{} (map first)))]
    (some->> (d/rseek-datoms db :eavt (:db/id chain) :coll/contains)
             (take-while (fn [[e]] (= e (:db/id chain))))
             (keep (fn [[_ _ v t]]
                     (when (seq (d/datoms db :eavt v :eval/of))
                       v)))
             (first)
             (delete-by-eid db))))

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

;; This is junk because of retracting the highlight properly

(defn stitch*
  [sel e]
  (when-let [head (:seq/first e)]
    (let [text (->> (tree-seq :coll/type e/seq->vec e)
                    (keep :token/value)
                    (apply str))]
      (into (vector (assoc (e/parse-token-tx text) :db/id (:db/id head)))
            (concat (edit/form-raise-tx head)
                    (move-selection-tx (:db/id sel) (:db/id head)))))))

(defn restitch
  [sel c]
  (some->> (nav/parents-seq c)
           (filter (comp #{:tear} :coll/type))
           #_(first)
           (last)
           (stitch* sel)))

(defn tear-re
  [delim-re]
  (let [lookahead+ #(str "(?=" % ")" )
        lookback+  #(str "(?<=" % ")")]
    (re-pattern
     (str (lookahead+ delim-re)
          "|" (lookback+ delim-re)))))

(defn tear-preference-order
  [s]
  (letfn [(try-split [[head & more :as groups]]
            (when more groups))]
    (or (try-split (string/split s #"\s+"))
        (try-split (string/split s (tear-re "[:\\.\\-_\\$/<>]")))
        (try-split (string/split s #"(?<=[^A-Z])(?=[A-Z])")))))

(defn tear*
  [sel [head & more]]
  (let [tt (:token/type sel)]
    (into [[:db/add (:db/id sel) :token/value head]
           (when more [:db/add "torn" :seq/next "newtail"])
           (when more
             (-> (for [e more]
                   {:token/value e
                    :token/type tt
                    :coll/_contains "torn"})
                 (ec/seq-tx)
                 (assoc :db/id "newtail")))]
          (concat (edit/form-wrap-tx sel :tear "torn")
                  (move-selection-tx (:db/id sel) "torn")))))

(defn tear-tx
  [sel]
  (if-some [parts (some-> sel :token/value tear-preference-order)]
    (tear* sel parts)
    (restitch sel sel )))

(comment
  (tear-preference-order "https://raw.githubusercontent.com/mdn/content/main/files/en-us/web/javascript/reference/global_objects/promise/index.md"))

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
  [sel]
  (let [top-level (peek (nav/parents-vec sel))
        tempid "alias"
        new-node {:db/id tempid
                  :coll/type :alias
                  :alias/of (:db/id sel)}]
    (println "MAke alias" new-node)
    (into [new-node]
          (concat
           (edit/insert-before-tx top-level new-node)
           (move-selection-tx (:db/id sel) tempid)))))

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

(def drag-left-tx  (partial drag* move/prev-sibling edit/insert-before-tx))

(def drag-right-tx (partial drag* move/next-sibling edit/insert-after-tx))

(defn chain-from-text
  [sel text props]
  (let [top-level   (peek (nav/parents-vec sel))
        chain       (some-> top-level :coll/_contains edit/exactly-one)
        _ (js/console.time "S->tx")
        stx (e/string->tx-all text)
        _ (js/console.timeEnd "S->tx")
        new-node (-> stx
                     (update :db/id #(or % "cft"))
                     (assoc :coll/type :chain)
                     (merge props))]
    (into [new-node]
          (edit/insert-after-tx chain new-node))))

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
   :m1              (partial numeric-movement 0)
   :m2              (partial numeric-movement 1)
   :m3              (partial numeric-movement 2)
   :m4              (partial numeric-movement 3)
   :m5              (partial numeric-movement 4)
   :m6              (partial numeric-movement 5)
   :m7              (partial numeric-movement 6)
   :m8              (partial numeric-movement 7)})

(defn new-comment-tx
  [sel]
  (into [[:db/add (:db/id sel) :form/linebreak true]]
        (edit/insert-editing-before sel sel
          (cond-> {:token/type :comment
                   :form/linebreak true
                   :form/edit-initial ";; "}
            (:form/indent sel) (assoc :form/indent (:form/indent sel))))))

(defn go
  [data coll-eid cell-eid]
  (binding [sed/*store* (fn s
                          ([] (str (d/squuid)))
                          ([v] (when (meta v) (s)))
                          ([k v]
                           (println "Storing" k v)
                           (sh/store k v)))]
    (sed/rplacd data 8 coll-eid cell-eid))
  
  #_(binding [sed/*store-reference!* sh/store
              sed/*store-reference?* (fn [v]
                                       (when-some [m (meta v)]
                                         (str (d/squuid))))]
      (sed/rplacd data
                  64
                  coll-eid
                  cell-eid)))

(def editing-commands
  {:float                          (comp edit/exchange-with-previous-tx get-selected-form)
   :sink                           (comp edit/exchange-with-next-tx get-selected-form)
   :select-chain                   (fn [db] (nav/select-chain-tx (get-selected-form db)))
   :hop-left                       nav/hop-left
   :hop-right                      nav/hop-right
   :uneval                         (comp uneval-and-next get-selected-form)
   :insert-right                   (comp edit/insert-editing-after get-selected-form)
   :insert-left                    (comp edit/insert-editing-before get-selected-form)
   ;; :insert-right-newline           (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :list "" {:form/linebreak true}))
   :edit/reject                    (fn [db] (insert/reject-edit-tx db (d/entid db [:form/editing true])))
   :edit/finish                    (fn [db text] (insert/finish-edit-tx db (d/entid db [:form/editing true]) text))
   :edit/finish-and-move-up        (fn [db text]
                                     (insert/finish-edit-and-move-up-tx
                                      db
                                      (d/entid
                                       db
                                       [:form/editing true])
                                      text))
   :edit/finish-and-edit-next-node (fn [db text] (->> text (insert/finish-edit-and-edit-next-tx (d/entity db [:form/editing true]))))
   :edit/wrap                      (fn [db ct value] (insert/wrap-edit-tx (d/entity db [:form/editing true]) ct value))

   :delete-left      (fn [db] (move-and-delete-tx db move/backward-up move/next-sibling))
   :delete-right     (fn [db] (move-and-delete-tx db move/forward-up move/prev-sibling))
   :raise            (comp edit/form-raise-tx get-selected-form)
   :raise-parent     (comp edit/form-raise-tx move/up get-selected-form )
   :clone            (comp edit/insert-duplicate-tx get-selected-form)
   :linebreak        linebreak-selected-form-tx
   :wrap             (fn [db] (edit/form-wrap-tx (get-selected-form db) :list))
   :indent           (fn [db] (indent-selected-form-tx db 1))
   :dedent           (fn [db] (indent-selected-form-tx db -1))
   :slurp-right      (fn [db] (edit/slurp-right-tx (get-selected-form db)))
   :barf-right       (fn [db] (edit/barf-right-tx (get-selected-form db)))
   :new-list         (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :list "" {}))
   :new-vec          (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :vec "" {}))
   :new-deref        (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :deref "" {}))
   :new-quote        (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :quote "" {}))
   :new-syntax-quote (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :syntax-quote "" {}))
   :new-unquote      (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :unquote "" {}))
   :new-meta         (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :meta "" {}))
   :new-comment      (comp new-comment-tx get-selected-form)
   :open-chain       (fn [db t props] (chain-from-text (get-selected-form db) t props))
   :new-bar          (fn [db] (new-bar-tx (get-selected-form db)))
   
   :hide                  (fn [db] (toggle-hide-show (peek (nav/parents-vec (get-selected-form db)))))
   :stringify             (fn [db] (replace-with-pr-str (get-selected-form db)))
   :plus                  (comp plus* get-selected-form)
   :minus                 (comp minus* get-selected-form)
   
   ;; bad
   :insert-txdata         (fn [db c] (insert-txdata (get-selected-form db) c))
   
   :hoist                 (comp hoist-tx get-selected-form)
   :gobble                (comp gobble-tx get-selected-form)
   :move-to-deleted-chain (comp move-to-deleted-chain get-selected-form)
   :tear                  (comp tear-tx get-selected-form)
   :alias                 (comp make-alias-tx get-selected-form)
   :drag-left             (comp drag-left-tx get-selected-form)
   :drag-right            (comp drag-right-tx get-selected-form)
   :split                 (comp edit/form-split-tx get-selected-form)
   :splice                (comp edit/form-splice-tx get-selected-form)
   :offer                 (comp edit/offer-tx get-selected-form)
   :multiline             recursive-multiline
   :oneline               recursive-oneline
   :clear-one-eval        clear-one-eval
   
   :eval-result (fn [db et data]
                  (let [target  (peek (nav/parents-vec et))
                        spine (edit/exactly-one (:seq/_first target))
                        coll (edit/exactly-one (:coll/_contains target))]
                    (when (and spine coll)
                      (into [[:db/add (:db/id spine) :seq/next "ncell"]
                             (when-let [old-next (:seq/next spine)]
                               [:db/add "ncell" :seq/next (:db/id old-next)])]
                            (go (list data) (:db/id coll) "ncell")))))

   :eval-inplace (fn [db target data]
                   (let [spine (edit/exactly-one (:seq/_first target))
                         coll (edit/exactly-one (:coll/_contains target))]
                     (when (and spine coll)
                       (let [ans (go (list data) (:db/id coll) (:db/id spine))
                             id-hack (some (fn [[_ e]]
                                             (when (neg? e) e))
                                           ans)]
                         (into (select-form-tx db id-hack)
                               ans)))))
   
   :eval-cont (fn [db target data]
                (println "Eval-cont"
                         (pr-str data))
                (let [spine (edit/exactly-one (:seq/_first target))
                      coll (edit/exactly-one (:coll/_contains target))]
                  (println "Sp" (:db/id spine)
                           "Col" (:db/id coll))
                  (when (and spine coll)
                    (let [sn (next data)]
                      (if (nil? sn)
                        (into (select-form-tx db (:db/id (move/backward-up target)))
                              (edit/form-delete-tx target))
                        (let [root (if-not (:seq/next spine)
                                     sn
                                     (list sn))
                              tx-data (go root (:db/id coll) (:db/id spine))
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
   
   :unraise (comp edit/unraise-tx get-selected-form)
   :compose (fn [db]
              (let [sel (get-selected-form db)]
                (edit/ffff sel)))
   :ingest-markdown (fn [db md-text]
                      (let [top-level (peek (nav/parents-vec (get-selected-form db)))
                            txe (emd/md->tx md-text)]
                        (into [txe]
                              (edit/insert-before-tx top-level txe))))})


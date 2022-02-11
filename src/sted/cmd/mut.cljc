(ns sted.cmd.mut
  (:require
   [clojure.string :as string]
   [datascript.core :as d]
   [sted.embed :as e]
   [sted.cmd.move :as move]
   [sted.cmd.edit :as edit]
   [sted.cmd.insert :as insert]
   [sted.cmd.nav :as nav]
   [sted.core :as core :refer [get-selected-form
                               move-selection-tx]]))

(defn select-form-tx
  [db eid]
  (when eid
   (move-selection-tx (:db/id (get-selected-form db))
                      eid)))

;; second movement type is plan B in case we are asked to delete first/last of chain


(defn move-and-delete-tx
  [db mta mtb]
  (let [src (get-selected-form db)]
    (when-let [dst (or (move/move mta src)
                       (and mtb (move/move mtb src)))]
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
        parent-indent (:form/indent (move/move :move/up sel))
        pre-lb        (:form/linebreak sel)]
    #_(println "PArentindent" parent-indent)
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
                  (println "Collection First?" (zero? idx)
                           (e/->string e))
                  (if-not es
                    (recur ch (+ 2 ind) 0 (into acc rec))
                    (recur es ind (inc idx) (into acc rec))))))]
    (go (e/seq->vec (get-selected-form db)) 2 0 [])))


(defn recursive-oneline
  [db]
  (println "Oneline" (get-selected-form db))
  (let [a
        (->> (get-selected-form db)
             (tree-seq :coll/contains :coll/contains)
             (next)
             (keep (fn [e]
                     (prn (keys e))
                     (when (:form/linebreak e)
                       (cond-> [[:db/retract (:db/id e) :form/linebreak (:form/linebreak e)]]
                         (:form/indent e) (conj [:db/retract (:db/id e) :form/indent ])))
                     #_(cond-> []
                         (:form/indent e)    (conj [:db/retract (:db/id e) :form/indent (:db/id e)])
                         (:form/linebreak e) (conj [:db/retract (:db/id e) :form/linebreak (:db/id e)])
                         #_(= [])              #_(do nil))))
             (reduce into))]
    (run! prn a)
    a
    ))

#_(defn select-1based-nth-reverse-parent
  [sel n]
  (let [rpa (reverse (nav/parents-vec sel))]
    (when (< 0 n (inc (count rpa)))
      (move-selection-tx (:db/id sel)
                         (:db/id (nth rpa (dec n)))))))

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

#_(defn lazy-bfs
  [top limit]
  ((fn iter [i [e :as front]]
     (when (and (some? e) (> limit i))
       (cond->> (lazy-seq (iter (cond-> i (:coll/type e) inc)
                                (cond-> (subvec front 1)
                                  (:seq/next e)  (conj (:seq/next e))
                                  (:seq/first e) (conj (:seq/first e)))))
         (:coll/type e) (cons e)))))
  0 top)

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

(comment
  (run! prn
        (map e/->form
             (no-double-colls-lazy (lazy-bfs (e/->entity
                                         '(defn bar [{:keys [a b]}]))))))
  (map :db/id (lazy-bfs (e/->entity '[:a :b :c [[[:dd [:bv [:ok]]]]]]))))

(defn get-numeric-movement-vec
  [sel]
  (let [children (next (take 8 (no-double-colls (lazy-bfs sel))))]
    (if (move/move :move/up sel)
      (into [(peek (nav/parents-vec sel))] children)
      (into [nil] children))))

(defn numeric-movement
  [n sel]
  (when-let [nmv (get-numeric-movement-vec sel)]
    #_(println "NMove" n sel)
    #_(run! prn (map vector (range) nmv))
    (when (< -1 n (count nmv))
      (nth nmv n))))

(defn eval-result-above-toplevel
  [db et resp-str]
  (let [top-level   (peek (nav/parents-vec et))
        prev        (move/move :move/prev-sibling top-level)
        result-node {:db/id "nn"
                     :token/type :verbatim
                     :token/value resp-str
                     :eval/of (:db/id et)}]
    (into [result-node]
          (edit/insert-before-tx top-level result-node))))

(defn delete-by-eid
  [db eid]
  (let [e (d/entity db eid)]
    (println "Dbeid" eid)
    (if-not (:form/highlight e)
      (edit/form-delete-tx e)
      (move-and-delete-tx db :move/backward-up :move/next-sibling))))

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

(defn ingest-nav-assoc
  [[[k v] & kvs]]
  (let [e (e/new-tempid)]
    (merge {:db/id e :coll/type :map}
           (e/seq-tx
            (->> kvs
                 (mapcat (fn [[k v]]
                           [(assoc (e/->tx k)
                                   :form/linebreak true
                                   :form/indent 1)
                            (e/->tx v)]))
                 (list* (e/->tx k) (e/->tx v))
                 (map #(assoc % :coll/_contains e)))))))

(defn ingest-nav-seq
  [vs]
  (let [e (e/new-tempid)]
    (merge {:db/id e :coll/type :vec}
           (e/seq-tx
            (for [v vs]
              (assoc (e/->tx v) :coll/_contains e))))))

(defn ingest-navigable
  [c]
  (-> (if (and (associative? c) (not (sequential? c)))
        (ingest-nav-assoc c)
        (ingest-nav-seq c)
        #_(e/->tx* c))
      (assoc :nav/pointer c :eval/action :nav)
      (update :db/id #(or % "new-node"))))

#_(defn ingest-eval-result
  [db et c]
  (let [top-level (peek (nav/parents-vec et))
        new-node (ingest-navigable c)
        outer {:db/id "outer"
               :coll/type :eval-result
               :seq/first (:db/id new-node)
               :eval/of (:db/id et)
               :coll/contains (:db/id new-node)}]
    (into [new-node outer]
          #_(edit/insert-before-tx top-level new-node)
          (edit/insert-after-tx top-level outer))))

(defn ingest-eval-result
  [db et c]
  (let [top-level (peek (nav/parents-vec et))
        new-node (-> (e/->tx c)
                     (update :db/id #(or % "new-node")))
        outer {:db/id "outer"
               :coll/type :eval-result
               :seq/first (:db/id new-node)
               :eval/of (:db/id et)
               :coll/contains (:db/id new-node)}]
    (into [new-node outer]
          (edit/insert-before-tx top-level outer)
          #_(edit/insert-after-tx top-level outer))))

#_(defn ingest-replacing
  [db et c]
  (let [top-level (peek (nav/parents-vec et))
        new-node  (ingest-navigable c)]
    (into [new-node]
          #_(edit/insert-before-tx top-level new-node)
          #_(edit/insert-after-tx top-level outer)
          (concat (edit/form-overwrite-tx et (:db/id new-node))
                  (move-selection-tx (:db/id et) (:db/id new-node))))))

(defn ingest-after
  [db et c]
  (let [top-level (peek (nav/parents-vec et))
        new-node  (-> (e/->tx c)
                      (update :db/id #(or % "new-node")))]
    (into [new-node]
          #_(edit/insert-before-tx top-level new-node)
          (edit/insert-after-tx top-level new-node))))

(defn insert-data
  [et c]
  (let [top-level (peek (nav/parents-vec et))
        prev (move/move :move/prev-sibling top-level)
        result-node (-> (e/->tx c)
                        (update :db/id #(or % "import-formdata-tx")))]
    (into [result-node]
          (edit/insert-before-tx top-level result-node))))

(defn insert-txdata
  [et c]
  (let [top-level (peek (nav/parents-vec et))
        prev (move/move :move/prev-sibling top-level)
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
                   (some->> (move/move :move/backward-up sel)
                            (:db/id)
                            (move-selection-tx (:db/id sel))))))))

(defn gobble-tx
  [sel]
  (when-let [gt (some->> sel
                         (nav/parents-vec)
                         (peek)
                         (move/move :move/prev-sibling))]
    (into (edit/form-unlink-tx gt)
          (concat (edit/insert-after-tx sel gt)
                  (move-selection-tx (:db/id sel) (:db/id gt))))))

(defn move-to-deleted-chain
  [sel]
  (when-let [dch (d/entity (d/entity-db sel) :sted.page/command-chain)]
    (when-let [nsel (move/move :move/backward-up sel)]
      (concat (edit/form-unlink-tx sel)
              (edit/form-cons-tx sel dch)
              (move-selection-tx (:db/id sel) (:db/id nsel))))))

(defn uneval-and-next
  [sel]
  (let [dst (move/move :move/next-sibling sel)]
    (concat (edit/form-wrap-tx sel :uneval)
            (when dst (move-selection-tx (:db/id sel) (:db/id dst))))))

;; This is junk because of retracting the highlight properly


(defn stitch*
  [sel e]
  (when-let [head (:seq/first e)]
    (let [text (->> (tree-seq :coll/contains :coll/contains e)
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
           (when more [:db/add "newnode" :seq/next "newtail"])
           (when more
             (-> (for [e more]
                   {:token/value e
                    :token/type tt
                    :coll/_contains "newnode"})
                 (e/seq-tx)
                 (assoc :db/id "newtail")))]
          (concat (edit/form-wrap-tx sel :tear)
                  (move-selection-tx (:db/id sel) "newnode")))))

(defn tear-tx
  [sel]
  (if-some [parts (some-> sel :token/value tear-preference-order)]
    (tear* sel parts)
    (restitch sel sel )))

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
                  :alias/of (:db/id sel)
                  
                  #_ #_:seq/first (:db/id sel)}]
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
                                          :form/editing true}}
                  #_{:db/id "inner"
                     :coll/_contains "newnode"
                     :form/edit-initial (or init  "")
                     :form/editing true}}]
    (into [new-node]
          (concat (edit/insert-after-tx sel new-node)
                  (move-selection-tx (:db/id sel) "newnode")))))

#_(defn drag*
  [chain-mover chain-inserter sel]
  (let [top-level   (peek (nav/parents-vec sel))
        chain       (some-> top-level :coll/_contains edit/exactly-one)
        other-chain (move/move chain-mover chain)
        target      (or (:chain/selection other-chain)
                        (:seq/first other-chain))
        target-tl   (peek (nav/parents-vec target))
        ;; new-sel (move/move :move/backward-up sel)
        ]
    (into (edit/form-unlink-tx sel)
          (cond
            target-tl
            (concat
             (edit/insert-before-tx target-tl sel)
             #_(when new-sel (move-selection-tx (:db/id sel) (:db/id new-sel))))
            (and chain (nil? other-chain))
            (let [new-chain {:db/id "newchain"
                             :coll/type :chain
                             :coll/contains (:db/id sel)
                             :seq/first (:db/id sel)}]
              (concat [new-chain]
                      (chain-inserter chain new-chain)
                      #_(when new-sel (move-selection-tx (:db/id sel) (:db/id new-sel)))))))))

(defn drag-to-chain-selection
  [chain-inserter sel target]
  (let [top (peek (nav/parents-vec target))]
    (into (edit/form-unlink-tx sel)
          (edit/insert-before-tx top sel))))

(defn drag-to-head-of-chain
  [chain-inserter sel target-chain]
  (into (edit/form-unlink-tx sel)
        (edit/form-cons-tx sel target-chain)))

(defn drag*
  [chain-mover chain-inserter sel]
  (let [top-level   (peek (nav/parents-vec sel))
        chain       (some-> top-level :coll/_contains edit/exactly-one)
        other-chain (move/move chain-mover chain)
        target      (:chain/selection other-chain)]
    (into (edit/form-unlink-tx sel)
          (if-let [top (peek (nav/parents-vec target))]
            (edit/insert-before-tx top sel)
            (if other-chain
              (edit/form-cons-tx sel other-chain)
              (let [nc {:db/id         "newchain"
                        :coll/type     :chain
                        :coll/contains (:db/id sel)
                        :seq/first     (:db/id sel)}]
                (into [nc] (chain-inserter chain nc))))))))

(def drag-left-tx  (partial drag* :move/prev-sibling edit/insert-before-tx))

(def drag-right-tx (partial drag* :move/next-sibling edit/insert-after-tx))

(defn chain-from-text
  [sel text props]
  (let [top-level   (peek (nav/parents-vec sel))
        chain       (some-> top-level :coll/_contains edit/exactly-one)
        new-node (-> (e/string->tx-all text)
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
                 (move/move :move/up e)
                 
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
   :flow-right      (partial move/move :move/flow)
   :flow-left       (partial move/move :move/back-flow)
   :flow-right-coll (fn [sel]
                      (->> sel
                           (iterate (partial move/move :move/flow))
                           (next)
                           (take-while some?)
                           (filter :coll/type)
                           (first)))
   :parent          (partial move/move :move/up)
   :toplevel        (partial move/move :move/most-upward)
   :next            (partial move/move :move/next-sibling)
   :prev            (partial move/move :move/prev-sibling)
   :tail            (partial move/move :move/most-nested)
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
   :edit/finish-and-move-up        (fn [db text] (insert/finish-edit-and-move-up-tx db (d/entid db [:form/editing true]) text))
   :edit/finish-and-edit-next-node (fn [db text] (->> text (insert/finish-edit-and-edit-next-tx (d/entity db [:form/editing true]))))
   :edit/wrap                      (fn [db ct value] (insert/wrap-edit-tx (d/entity db [:form/editing true]) ct value))
   :delete-left                    (fn [db] (move-and-delete-tx db :move/backward-up :move/next-sibling))
   :delete-right                   (fn [db] (move-and-delete-tx db :move/forward-up :move/prev-sibling))
   :raise                          (comp edit/form-raise-tx get-selected-form)
   :raise-parent                   (comp edit/form-raise-tx (partial move/move :move/up) get-selected-form )
   :clone                          (comp edit/insert-duplicate-tx get-selected-form)
   :linebreak                      linebreak-selected-form-tx
   :wrap                           (fn [db] (edit/form-wrap-tx (get-selected-form db) :list))
   :indent                         (fn [db] (indent-selected-form-tx db 1))
   :dedent                         (fn [db] (indent-selected-form-tx db -1))
   :slurp-right                    (fn [db] (edit/slurp-right-tx (get-selected-form db)))
   :barf-right                     (fn [db] (edit/barf-right-tx (get-selected-form db)))
   :new-list                       (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :list "" {}))
   :new-vec                        (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :vec "" {}))
   :new-deref                      (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :deref "" {}))
   :new-quote                      (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :quote "" {}))
   :new-syntax-quote               (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :syntax-quote "" {}))
   :new-unquote                    (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :unquote "" {}))
   :new-meta                       (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :meta "" {}))
   :new-comment                    (comp new-comment-tx get-selected-form)
   :open-chain                     (fn [db t props] (chain-from-text (get-selected-form db) t props))
   :new-bar                        (fn [db] (new-bar-tx (get-selected-form db)))
   :eval-result                    ingest-eval-result #_eval-result-above-toplevel

   :ingest-after          ingest-after
   :hide                  (fn [db] (toggle-hide-show (peek (nav/parents-vec (get-selected-form db)))))
   :stringify             (fn [db] (replace-with-pr-str (get-selected-form db)))
   :plus                  (comp plus* get-selected-form)
   :minus                 (comp minus* get-selected-form)
   :insert-data           (fn [db c] (insert-data (get-selected-form db) c))
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
   
   :ingest-result (fn [db et data]
                    (let [top-level  (peek (nav/parents-vec et))
                          [ftx mtam] (e/tx-and-meta data)]
                      (println "Ingest result" mtam)
                      (into ftx
                            (concat
                             (for [[tempid v] mtam]
                               [:db/add tempid :nav/pointer v])
                             (edit/form-overwrite-tx et -1)
                             (move-selection-tx (:db/id et) -1)))))
   
   :unraise (comp edit/unraise-tx get-selected-form)
   })

(def dispatch-table
  (merge movement-commands
         editing-commands))

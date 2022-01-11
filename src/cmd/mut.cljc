(ns cmd.mut
  (:require
   [clojure.string :as string]
   [datascript.core :as d]
   [embed :as e]
   [cmd.move :as move]
   [cmd.edit :as edit]
   [cmd.insert :as insert]
   [cmd.nav :as nav]
   [core :as core
    :refer [get-selected-form
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
        parent-indent (:form/indent (move/move :move/up sel))]
    #_(println "PArentindent" parent-indent)
    [[:db/add (:db/id sel) :form/linebreak (not (:form/linebreak sel))]
     (when parent-indent
       [:db/add (:db/id sel) :form/indent parent-indent])]))




(defn recursively-set-indent-tx
  [db indent]
  (->> (get-selected-form db)
       (tree-seq :coll/type e/seq->vec)
       (next)
       (keep (fn [e]
               (when (:coll/type e)
                 [:db/add (:db/id e) :form/linebreak indent])))))


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
         (:coll/type e) (cons e))))
   [top]))

#_(defn no-double-colls
  [nmv]
  (loop [[x y :as xs] nmv
         stacked? false
         out []]
    (if (nil? x)
      out
      (recur (next xs)
             (= (:db/id y) (:db/id (:seq/first x)))
             (cond-> out (not stacked?) (conj x))))))

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

  (map :db/id (lazy-bfs (e/->entity '[:a :b :c [[[:dd [:bv [:ok]]]]]])))
  
  )



(defn get-numeric-movement-vec
  [sel]
  (let [children (next (take 8 (no-double-colls (lazy-bfs sel))))]
    (if (move/move :move/up sel)
      (into [(peek (nav/parents-vec sel))] children)
      (into [nil] children))))

(defn numeric-movement
  [sel n]
  (when-let [nmv (get-numeric-movement-vec sel)]
    #_(println "NMove" n sel )
    #_(run! prn (map vector (range) nmv))
    (when (< -1 n (count nmv))
      (move-selection-tx (:db/id sel)
                         (:db/id (nth nmv n))))))

(defn ingest-result
  [db et c]
  (let [ee (d/entity db :page/evalchain)
        new-node (-> (e/->tx* c)
                     (assoc :form/linebreak true)
                     (update :db/id #(or % "new-node")))]
    (prn 'ingest et c)
    (into [new-node]
          (edit/insert-before-tx
           (:seq/first ee)
           {:db/id (:db/id new-node)}))))

(defn eval-result
  [db et-eid c]
  (let [ee (d/entity db :page/evalchain)
        new-node (-> (e/->tx* #_(with-out-str (cljs.pprint/pprint c))
                              (pr-str c))
                     (assoc :form/linebreak true)
                     (update :db/id #(or % "import-formdata-tx")))]
    (prn et-eid c)
    (into [new-node]
          (edit/insert-before-tx (:seq/first ee) new-node))))

#_(defn eval-result
  [db et c]
  (let [top-level   (peek (nav/parents-vec et))
        prev        (move/move :move/prev-sibling top-level)
        result-node (-> (e/->tx* #_(with-out-str (cljs.pprint/pprint c))
                                 (pr-str c))
                        (update :db/id #(or % "import-formdata-tx")))
        prev-eval (d/datoms db :avet :eval/of )
        new-node    {:db/id       "eval-result"
                     :eval/of     (:db/id et)
                     :coll/type   :eval-result
                     :eval/result result-node}]
    (println "PRevevel")
    (run! prn prev-eval)
    (doto
        (into [new-node]
              (apply
               concat
               (edit/insert-before-tx top-level new-node)
               (for [[e a v t a?] (d/datoms db :avet :eval/of)
                     :when a?]
                 (edit/form-delete-tx (d/entity db e)))))
        prn
        )
    
    #_(into [new-node]
            (if (= (:db/id et) (:db/id (:eval/of prev)))
              (edit/form-overwrite-tx prev (:db/id new-node))
              (edit/insert-before-tx top-level new-node)))
    #_(into [new-node]
            (edit/insert-before-tx (:seq/first ee) new-node))))

(defn insert-data
  [et c]
  (let [top-level (peek (nav/parents-vec et))
        prev (move/move :move/prev-sibling top-level)
        result-node (-> (e/->tx* c)
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
  (let [new-node {:db/id        "eval-result"
                  :string/value (pr-str (e/->form sel))}]
    (prn "RPPRS" new-node )
    (doto
        (into [new-node]
              (concat
               (edit/form-overwrite-tx sel "eval-result")
               (move-selection-tx (:db/id sel) "eval-result")))
        prn
        )))

(defn plus*
  [e]
  (when-let [n (:number/value e)]
    [[:db/add (:db/id e) :number/value (inc n)]]))

(defn minus*
  [e]
  (when-let [n (:number/value e)]
    [[:db/add (:db/id e) :number/value (dec n)]]))

(defn hoist-tx
  [sel]
  (let [top-level (peek (nav/parents-vec sel))]
      (into (edit/form-delete-tx sel false)
            (edit/insert-before-tx top-level sel))))

(defn move-to-deleted-chain
  [sel]
  (when-let [dch (d/entity (d/entity-db sel) :page/command-chain)]
    (when-let [nsel (move/move :move/backward-up sel)]
      (concat
       (edit/form-delete-tx sel false)
       (edit/form-cons-tx sel dch)
       (move-selection-tx (:db/id sel) (:db/id nsel))))))

(defn uneval-and-next
  [sel]
  (let [dst (move/move :move/next-sibling sel)]
    (concat (edit/form-wrap-tx sel :uneval)
            (when dst (move-selection-tx (:db/id sel) (:db/id dst))))))



;; This is junk because of retracting the highlight properly
(defn stitch*
  [sel vt e]
  (when-let [head (:seq/first e)]
    (let [xs (keep vt (tree-seq :coll/type e/seq->vec e)
                   #_(e/seq->vec e))]
      (into [[:db/add (:db/id head) vt (string/join "-" xs)]]
            (concat (edit/form-raise-tx head)
                    (move-selection-tx (:db/id sel) (:db/id head)))))))

(defn restitch
  [sel c]
  (some->> (nav/parents-seq c)
           (filter (comp #{:tear} :coll/type))
           (first)
           (stitch* sel :symbol/value)))

(defn tear-str
  [s]
  (let [[head & more :as sp] (string/split s #"[./\-]")]
    (when more sp)))

(defn tear*
  ([sel]
   (or (tear* sel :symbol/value)
       (tear* sel :string/value)
       (tear* sel :keyword/value)))
  ([sel vt]
   (when-let [vs (vt sel)]
     (let [[head & more] (string/split vs #"[./\-]")]
       (into [[:db/add (:db/id sel) vt head]
              (when more
                [:db/add "newnode" :seq/next "newtail"])
              (when more
                (-> (for [e more]
                      {vt e :coll/_contains "newnode"})
                    (e/seq-tx)
                    (assoc :db/id "newtail")))]
            (concat
             (edit/form-wrap-tx sel :tear)
             (move-selection-tx (:db/id sel) "newnode")))))))
(defn tear-tx
  [sel]
  (or (restitch sel sel)
      (tear* sel)))



(defn find-next*
  [me ir]
  (loop [[[e a v t] & more] ir]
    (cond
      (nil? e)            nil
      (and more (= e me)) (first (first more))
      :else (recur more))))

(defn find-next-tx
  ([sel]
   (or (find-next-tx sel :symbol/value)
       (find-next-tx sel :keyword/value)
       (find-next-tx sel :string/value)))
  ([sel vt]
   (when-let [text (vt sel)]
     (let [db  (d/entity-db sel)
           ln  (str text "\udbff\udfff")
           ir  (filter (fn [[e a v t]] (= v text)) (d/index-range db vt text ln))
           fnf (find-next* (:db/id sel) ir)]

       (select-form-tx db
                       (or fnf
                           (when (next ir)
                             (first (first ir)))))))))

(defn find-first-tx
  ([sel]
   (or (find-first-tx sel :symbol/value)
       (find-first-tx sel :keyword/value)
       (find-first-tx sel :string/value)))
  ([sel vt]
   (when-let [text (vt sel)]
     (let [ln  (str text "\udbff\udfff")
           ir  (d/index-range (d/entity-db sel) vt text ln)
           dst (apply min (map first ir))]
       (when (and dst (not= dst (:db/id sel)))
         (move-selection-tx (:db/id sel) dst))))))

(def dispatch-table
  {:select     nav/select-form-tx
   :find-next  (comp find-next-tx get-selected-form)
   :find-first (comp find-first-tx get-selected-form)
   
   :flow-right      (fn [db] (move/movement-tx db :move/flow))
   :flow-left       (fn [db] (move/movement-tx db :move/back-flow))
   :flow-right-coll (fn [db]
                      (println "FRCnext" )
                      (let [sel (get-selected-form db)]
                        (when-let [dst (->> sel
                                            (iterate (partial move/move :move/flow))
                                            (next)
                                            (take-while some?)
                                            (filter :coll/type)
                                            (first))]
                          (move-selection-tx (:db/id sel) (:db/id dst)))))
   :float           (comp edit/exchange-with-previous-tx get-selected-form)
   :sink            (comp edit/exchange-with-next-tx get-selected-form)
   :parent          (fn [db] (move/movement-tx db :move/up))
   :toplevel        (fn [db] (move/movement-tx db :move/most-upward))
   :next            (fn [db] (move/movement-tx db :move/next-sibling))
   :prev            (fn [db] (move/movement-tx db :move/prev-sibling))
   :tail            (fn [db] (move/movement-tx db :move/most-nested))
   
   :uneval (comp uneval-and-next get-selected-form)
   
   :insert-right (comp edit/insert-editing-after get-selected-form)
   :insert-left  (comp edit/insert-editing-before get-selected-form)
   
   ;; :insert-right                   (fn [db] (edit/insert-editing-tx db :after))
   ;; :insert-left                    (fn [db] (edit/insert-editing-tx db :before))
   
   :insert-right-newline
   (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :list "" {:form/linebreak true}))
   
   :edit/reject
   (fn [db]
     (insert/reject-edit-tx db (d/entid db [:form/editing true])))
   
   :edit/finish
   (fn [db text]
     (insert/finish-edit-tx db (d/entid db [:form/editing true]) text)) 
   
   :edit/finish-and-move-up
   (fn [db text]
     (insert/finish-edit-and-move-up-tx db (d/entid db [:form/editing true]) text))
   
   :edit/finish-and-edit-next-node
   (fn [db text]
     (->> text
          (insert/finish-edit-and-edit-next-tx
           (d/entity db [:form/editing true])
           )))
   
   :edit/wrap        (fn [db ct value] (insert/wrap-edit-tx (d/entity db [:form/editing true]) ct value))
   :delete-left      (fn [db] (move-and-delete-tx db :move/backward-up :move/next-sibling))
   :delete-right     (fn [db] (move-and-delete-tx db :move/forward-up :move/prev-sibling))
   :raise            (comp edit/form-raise-tx get-selected-form)
   :clone            (comp edit/insert-duplicate-tx get-selected-form)
   :linebreak        linebreak-selected-form-tx
   :hop-left         nav/hop-left
   :hop-right        nav/hop-right
   :compose          (fn [db] (edit/wrap-and-edit-first-tx (get-selected-form db) :list))
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

   :new-bar (fn [db] (edit/edit-new-wrapped-tx (get-selected-form db) :bar "" {}))

   :m1                    (fn [db] (numeric-movement (get-selected-form db) 0))
   :m2                    (fn [db] (numeric-movement (get-selected-form db) 1))
   :m3                    (fn [db] (numeric-movement (get-selected-form db) 2))
   :m4                    (fn [db] (numeric-movement (get-selected-form db) 3))
   :m5                    (fn [db] (numeric-movement (get-selected-form db) 4))
   :m6                    (fn [db] (numeric-movement (get-selected-form db) 5))
   :m7                    (fn [db] (numeric-movement (get-selected-form db) 6))
   :m8                    (fn [db] (numeric-movement (get-selected-form db) 7)) 
   :eval-result           eval-result
   :ingest-result         ingest-result
   :hide                  (fn [db]
                            (toggle-hide-show
                             (peek (nav/parents-vec (get-selected-form db)))))
   :select-chain          (fn [db] (nav/select-chain-tx (get-selected-form db)))
   :stringify             (fn [db] (replace-with-pr-str (get-selected-form db)))
   :plus                  (comp plus* get-selected-form)
   :minus                 (comp minus* get-selected-form)
   :insert-data           (fn [db c] (insert-data (get-selected-form db) c))
   :insert-txdata         (fn [db c] (insert-txdata (get-selected-form db) c))
   :hoist                 (comp hoist-tx get-selected-form)
   :move-to-deleted-chain (comp move-to-deleted-chain get-selected-form)
   :tear                  (comp tear-tx get-selected-form)

   
   :e1 (fn [db]
         (println "E one!" ))})



(ns cmd.mut
  (:require
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
  (move-selection-tx (:db/id (get-selected-form db))
                     eid))

#_(defn move-and-delete-tx
  [db movement-type]
  (let [src (get-selected-form db)]
    (when-let [dst (move/move movement-type src)]
      (concat (edit/form-delete-tx src)
              [[:db/add (:db/id dst) :form/highlight true]]))))
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
                              (when (:coll/type e)
                                (println "Bfs" limit e (next front)))
                              (recur (cond-> out
                                       (:coll/type e) (conj e))
                                     (cond-> (subvec front 1)
                                       (:seq/next e)  (conj (:seq/next e))
                                       (:seq/first e) (conj (:seq/first e))))))))

#_(defn select-1based-nth-breadthfirst-descendant
  [sel n]
  (let [rpa (el-bfs sel 8)]
    (when (< 0 n (inc (count rpa)))
      (move-selection-tx (:db/id sel)
                         (:db/id (nth rpa (dec n)))))))

(defn get-numeric-movement-vec
  [sel]
  (if (move/move :move/up sel)
    (do
      (println "Reverse parents")
      (reverse (nav/parents-vec sel)))
    (do
      (println "El-bfs")
      (el-bfs sel 8))))

(defn numeric-movement
  [sel n]
  (when-let [nmv (get-numeric-movement-vec sel)]
    (println "NMove" n sel )
    (run! prn (map vector (range) nmv))
    (when (< -1 n (count nmv))
      (move-selection-tx (:db/id sel)
                         (:db/id (nth nmv n))))))

#_(defn eval-result
  [db et c]
  (let [ee (d/entity db :page/evalchain)
        new-node (-> (e/->tx* #_(with-out-str (cljs.pprint/pprint c))
                              (pr-str c))
                     (assoc :form/linebreak true)
                     (update :db/id #(or % "import-formdata-tx")))]
    (prn c)
    (into [{:db/id "evalresult"
            :coll/type :vec
            :form/linebreak true
            :seq/first {:coll/type :alias
                        :alias/of (:db/id et)}
            :seq/next {:seq/first new-node}}]
          (edit/insert-before-tx
           (:seq/first ee)
           {:db/id "evalresult"}))
    #_(into [new-node]
            (edit/insert-before-tx (:seq/first ee) new-node))))

(defn eval-result
  [db et c]
  (let [ee (d/entity db :page/evalchain)
        new-node (-> (e/->tx* #_(with-out-str (cljs.pprint/pprint c))
                              (pr-str c))
                     (assoc :form/linebreak true)
                     (update :db/id #(or % "import-formdata-tx")))]
    (prn c)
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

(def dispatch-table
  {:select                         nav/select-form-tx
   :flow-right                     (fn [db] (move/movement-tx db :move/flow))
   :flow-left                      (fn [db] (move/movement-tx db :move/back-flow))
   :float                          (comp edit/exchange-with-previous-tx get-selected-form)
   :sink                           (comp edit/exchange-with-next-tx get-selected-form)
   :parent                         (fn [db] (move/movement-tx db :move/up))
   :toplevel                       (fn [db] (move/movement-tx db :move/most-upward))
   :next                           (fn [db] (move/movement-tx db :move/next-sibling))
   :prev                           (fn [db] (move/movement-tx db :move/prev-sibling))
   :tail                           (fn [db] (move/movement-tx db :move/most-nested))
   :insert-right                   (fn [db] (edit/insert-editing-tx db :after))
   :insert-left                    (fn [db] (edit/insert-editing-tx db :before))
   :insert-right-newline           (fn [db] (edit/edit-new-wrapped-tx db :list "" {:form/linebreak true}))
   :edit/reject                    (fn [db] (insert/reject-edit-tx db (d/entid db [:form/editing true])))
   :edit/finish                    (fn [db text] (insert/finish-edit-tx db (d/entid db [:form/editing true]) text)) 
   :edit/finish-and-move-up        (fn [db text] (insert/finish-edit-and-move-up-tx db (d/entid db [:form/editing true]) text))
   :edit/finish-and-edit-next-node (fn [db text] (insert/finish-edit-and-edit-next-tx db (d/entid db [:form/editing true]) text))
   :edit/wrap
   (fn [db ct value]
     (insert/wrap-edit-tx
      (d/entity db [:form/editing true])
      ct value))
   ;; second movement type is plan B in case we are asked to delete first/last of chain 
   :delete-left                    (fn [db] (move-and-delete-tx db :move/backward-up :move/next-sibling))
   :delete-right                   (fn [db] (move-and-delete-tx db :move/forward-up :move/prev-sibling))
   :raise                          (comp edit/form-raise-tx get-selected-form)
   :clone                          edit/insert-duplicate-tx
   :linebreak                      linebreak-selected-form-tx
   :hop-left                       nav/hop-left
   :hop-right                      nav/hop-right
   :compose                        (fn [db] (edit/wrap-and-edit-first-tx (get-selected-form db) :list))
   :wrap                           (fn [db] (edit/form-wrap-tx (get-selected-form db) :list))
   :indent                         (fn [db] (indent-selected-form-tx db 1))
   :dedent                         (fn [db] (indent-selected-form-tx db -1))
   :slurp-right                    (fn [db] (edit/slurp-right-tx (get-selected-form db)))
   :barf-right                     (fn [db] (edit/barf-right-tx (get-selected-form db)))
   :new-list                       (fn [db] (edit/edit-new-wrapped-tx db :list "" {}))
   :new-vec                        (fn [db] (edit/edit-new-wrapped-tx db :vec "" {}))
   
   :m1 (fn [db] (numeric-movement (get-selected-form db) 0))
   :m2 (fn [db] (numeric-movement (get-selected-form db) 1))
   :m3 (fn [db] (numeric-movement (get-selected-form db) 2))
   :m4 (fn [db] (numeric-movement (get-selected-form db) 3))
   :m5 (fn [db] (numeric-movement (get-selected-form db) 4))
   :m6 (fn [db] (numeric-movement (get-selected-form db) 5))
   :m7 (fn [db] (numeric-movement (get-selected-form db) 6))
   :m8 (fn [db] (numeric-movement (get-selected-form db) 7))
   
   :eval-result  eval-result
   :hide         (fn [db] (toggle-hide-show (get-selected-form db)))
   :select-chain (fn [db] (nav/select-chain-tx (get-selected-form db)))
   :stringify (fn [db] (replace-with-pr-str (get-selected-form db)))})

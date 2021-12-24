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


(defn move-and-delete-tx
  [db movement-type]
  (let [src (get-selected-form db)]
    (when-let [dst (move/move movement-type src)]
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


(defn select-1based-nth-reverse-parent
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
      (empty? front) out
      (= limit (count out)) out
      :else (let [e (first front)]
              (recur (cond-> out
                       (:coll/type e) (conj e))
                     (cond-> (subvec front 1)
                       (:seq/next e) (conj (:seq/next e))
                       (:seq/first e) (conj (:seq/first e))))))))

(defn select-1based-nth-breadthfirst-descendant
  [sel n]
  (let [rpa (el-bfs sel 8)]
    (when (< 0 n (inc (count rpa)))
      (move-selection-tx (:db/id sel)
                         (:db/id (nth rpa (dec n)))))))

(defn numeric-movement
  [sel n]
  (if (move/move :move/up sel)
    (select-1based-nth-reverse-parent sel n)
    (select-1based-nth-breadthfirst-descendant sel n)))

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
   :insert-right                   (fn [db] (edit/insert-editing-tx db :after ""))
   :insert-left                    (fn [db] (edit/insert-editing-tx db :before ""))
   :edit/reject                    (fn [db] (insert/reject-edit-tx db (d/entid db [:form/editing true])))
   :edit/finish                    (fn [db text] (insert/finish-edit-tx db (d/entid db [:form/editing true]) text)) 
   :edit/finish-and-move-up        (fn [db text] (insert/finish-edit-and-move-up-tx db (d/entid db [:form/editing true]) text))
   :edit/finish-and-edit-next-node (fn [db text] (insert/finish-edit-and-edit-next-tx db (d/entid db [:form/editing true]) text))
   :edit/wrap                      (fn [db ct value] (insert/wrap-edit-tx db (d/entid db [:form/editing true]) ct value))
   :delete-right                   (fn [db] (move-and-delete-tx db :move/forward-up))
   :delete-left                    (fn [db] (move-and-delete-tx db :move/backward-up))
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
   :new-list                       (fn [db] (edit/edit-new-wrapped-tx db :list ""))
   :new-vec                        (fn [db] (edit/edit-new-wrapped-tx db :vec ""))
   
   :m1 (fn [db] (numeric-movement (get-selected-form db) 1))
   :m2 (fn [db] (numeric-movement (get-selected-form db) 2))
   :m3 (fn [db] (numeric-movement (get-selected-form db) 3))
   :m4 (fn [db] (numeric-movement (get-selected-form db) 4))
   :m5 (fn [db] (numeric-movement (get-selected-form db) 5))
   :m6 (fn [db] (numeric-movement (get-selected-form db) 6))
   :m7 (fn [db] (numeric-movement (get-selected-form db) 7))
   :m8 (fn [db] (numeric-movement (get-selected-form db) 8))

   })

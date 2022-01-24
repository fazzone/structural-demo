(ns sted.schema)

(def form-schema
  {:token/type {}
   :token/value {:db/index true}
   :coll/type {}
   :coll/contains {:db/valueType :db.type/ref
                   :db/cardinality :db.cardinality/many}
   :seq/first {:db/valueType :db.type/ref}
   :seq/next {:db/valueType :db.type/ref}})

(def schema
  (merge
   form-schema
   {:state/bar {:db/valueType :db.type/ref}
    :history/item          {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
    :history-item/mutation {:db/valueType :db.type/ref}
    :history-item/tx       {:db/valueType :db.type/ref}
    ;; :state/selected-form {:db/valueType :db.type/ref}
    :form/editing      {:db/unique :db.unique/identity}
    :form/edit-initial {}
    :form/highlight    {:db/unique :db.unique/identity}
    :form/indent       {}
    :form/linebreak    {}
    :form/edited-tx    {:db/valueType :db.type/ref}
    :edit/of {:db/valueType :db.type/ref}
    :chain/selection {:db/valueType :db.type/ref}
    :chain/filename {}
    :key/kbd         {:db/unique :db.unique/identity}
    :key/mutation    {}
    :keymap/bindings {:db/valueType :db.type/ref
                      :db/cardinality :db.cardinality/many}
    :alias/of {:db/valueType :db.type/ref}
    :eval/of     {:db/valueType :db.type/ref}
    :eval/result {:db/valueType :db.type/ref}
    :eval/action {}
    :hidden/coll-type {}}))

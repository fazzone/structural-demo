(ns schema)

(def form-schema
  {:symbol/value {:db/index true}
   :number/value {}
   :string/value {}
   :keyword/value {:db/index true}
   :whitespace/value {}
   :coll/type {}
   :coll/contains {:db/valueType :db.type/ref
                   :db/cardinality :db.cardinality/many}
   :seq/first {:db/valueType :db.type/ref}
   :seq/next {:db/valueType :db.type/ref}})

(def schema
  (merge
   form-schema
   {:state/bar {:db/valueType :db.type/ref}
    
    :history/item {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
    :history-item/mutation {:db/valueType :db.type/ref}
    :history-item/tx {:db/valueType :db.type/ref}
    
    
    ;; :state/selected-form {:db/valueType :db.type/ref}
    :form/editing {:db/unique :db.unique/identity} 
    :form/edit-initial {}
    :form/highlight {:db/unique :db.unique/identity}
    :form/indent {}
    :form/linebreak {}
    :form/edited-tx {:db/valueType :db.type/ref}

    :edit/of {:db/valueType :db.type/ref}
    
    :chain/selection {:db/valueType :db.type/ref}

    :key/kbd {:db/unique :db.unique/identity}
    :key/mutation {}
    
    :alias/of {:db/valueType :db.type/ref}}))

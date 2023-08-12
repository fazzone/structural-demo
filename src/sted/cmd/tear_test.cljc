(ns sted.cmd.tear-test
  (:require
   #?(:clj [clojure.test :as t]
      :cljs [cljs.test :as t :include-macros true])
   [sted.cmd.mut :as mut]
   [sted.embed :as e]
   [datascript.core :as d]))




(let [conn (-> [:a.b.c.d/f]
               e/->entity
               d/entity-db
               d/conn-from-db)
      _ (d/transact! conn (mut/tear-tx (:seq/first (d/entity @conn 1))))
      {:keys [db-after]} (d/transact! conn (mut/tear-tx (:seq/first (d/entity @conn 1))))]
  (e/->form (d/entity db-after 1)))

#_(do
  (defn go []
    (let [ent (e/->entity "a/b/c/d") 
          db (d/entity-db ent)
          {:keys [db-after]} (d/transact! db (mut/tear-tx ent))]
      (e/->form (d/entity db-after (:db/id ent)))
      )
    
    
    )
  (go))

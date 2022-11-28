(ns sted.sys.handle
  (:require [datascript.core :as d]))

(def storage (atom {}))

(defn drop-all [] (reset! storage {}))

(defn store [k v]
  (swap! storage assoc k v)
  nil)

(defn load [k]
  (get @storage k))

(defn setup!
  [{:keys [conn] :as app}]
  #_(d/transact! conn
                 [{:db/ident :tokenize
                   :db/fn (fn [_db k ptr]
                            (swap! storage assoc k ptr)
                            nil) }])
  app)

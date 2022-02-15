(ns sted.sys.handle
  (:require [datascript.core :as d]))

(def storage (atom {}))

(defn drop-all [] (reset! storage {}))

(defn setup!
  [{:keys [conn] :as app}]
  (d/transact! conn
               [{:db/ident :tokenize
                 :db/fn (fn [_db k ptr]
                          (swap! storage assoc k ptr)
                          nil) }])
  app)

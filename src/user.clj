(ns user
  (:require
   [shadow.cljs.devtools.api :as shadow]
   [shadow.cljs.devtools.server :as server]
   [nrepl.server :as nrepl-server]   
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [clojure.walk :as walk]
   [clojure.string :as string])
  )

(def shadow-server (future (server/start!)))
(def shadow-watch (future
                    @shadow-server
                    (shadow/watch :br)))
(def nrepl-server
  (let [port-file (io/file ".nrepl-port")
        {:keys [port]} (nrepl-server/start-server #_ #_:handler cnr/cider-nrepl-handler)]
    (spit ".nrepl-port" port)))

(comment
  (shadow/watch :br)
  (shadow/release :br))

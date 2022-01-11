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

(defonce shadow-server (future (server/start!)))
(defonce shadow-watch (future
                    @shadow-server
                    (Thread/sleep 200)
                    (println "Starting watch")
                    (shadow/watch :br)))
(defonce nrepl-server
  (let [port-file (io/file ".nrepl-port")
        {:keys [port]} (nrepl-server/start-server #_ #_:handler cnr/cider-nrepl-handler)]
    (spit ".nrepl-port" port)))

(defn release-cljs-and-exit
  []
  (future-cancel shadow-watch)
  @shadow-server
  (shadow/release :br)
  (System/exit 0))

(comment
  (shadow/watch :elec))
(comment
  (shadow/release :elec))
(comment
  (do
    (server/stop!)
    (Thread/sleep 223)
    (server/start!))
  (shadow/watch :br)
  (shadow/watch :ptr)
  (do
    (shadow/release :elec)
    (shadow/release :br)))





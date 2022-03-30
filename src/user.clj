(ns user
  (:require
   [shadow.cljs.devtools.api :as shadow]
   [shadow.cljs.devtools.server :as server]
   [nrepl.server :as nrepl-server]
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [clojure.walk :as walk]
   [clojure.string :as string]))

(defonce shadow-server (future (server/start!)))

(defonce shadow-watch
  (future
    @shadow-server
    (Thread/sleep 200)
    #_(println "Starting watch")
    #_(shadow/watch :br)))

(defonce nrepl-server
  (let [port-file (io/file ".nrepl-port")
        {:keys [port]} (nrepl-server/start-server #_#_:handler cnr/cider-nrepl-handler)]
    (spit ".nrepl-port" port)))

(comment (shadow/watch :br))

(comment
  (shadow/release :elec)
  (shadow/release :br))

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

(defn release-cljs-and-exit
  [& what]
  (future-cancel shadow-watch)
  @shadow-server
  (shadow/release :br)
  (shadow/release :elec)
  (shadow/release :ptr)
  (System/exit 0))

(defn release-cljs!
  []
  (future-cancel shadow-watch)
  @shadow-server
  (shadow/release :br)
  (shadow/release :elec)
  (shadow/release :ptr)
  :ok)

(comment
  (shadow/watch :ptr))

#_(count
 (slurp
  (str
   "C:/Program Files/KiCad/6.0/share/kicad/symbols/"
   "4xxx.kicad_sym")))

#_(spit
 "testpattern.bin"
 (let [ns [0 0xff 0xf0 0xff00 0x0f0 0x0fff]
       b (java.nio.ByteBuffer/allocate (* 4 (count ns)))]
   (doseq [n ns]
     (.putInt b n))
   (.array b)))

#_(->>   "4xxx.kicad_sym"
       (str "C:/Program Files/KiCad/6.0/share/kicad/symbols/")
       slurp
       clojure.edn/read-string
       (tree-seq coll? seq)
       next
       (filter (fn [e]
                 (and (list? e) (= 'symbol (first e)))))
       (take 3)
       (run! (fn [[_symbol s & body]]
               (clojure.pprint/pprint
                (into {:symbol s}
                      (for [[p v & vs] body]
                        [p (if-not vs v (into [v] vs))])))
               #_(run! prn e))))











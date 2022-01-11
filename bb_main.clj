(ns bb-main
  (:require [babashka.deps :as deps]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def shadow-config (edn/read-string (slurp "shadow-cljs.edn")))

(def ptr-script (-> shadow-config :builds :ptr :output-to))

(defn io-prepl []
  (deps/clojure 
   ["-M"
    "-e" "(require '[clojure.core.server :as s])"
    "-e" "@user/shadow-server"
    "-e" "(s/io-prepl)"]
   {:in nil
    :out nil
    :err nil}))

(def the-prepl (io-prepl))

(def input-writer (io/writer (:in the-prepl)))        
(def output-reader (-> the-prepl :out io/reader))

(binding [*out* input-writer]
  (prn '(user/release-cljs!)))

;; wait for shadow to finish
(binding [*in* output-reader]
  (loop []
    (when-let [line (read-line)]
      (if (not= "{" (subs line 0 1))
        (do
          (println "[clj] " line)
          (recur))
        (let [v (edn/read-string line)]
          (case (:tag v)
            :ret (:val v)
            :out (do (print "[clj] " (:val v))
                     (recur))
            :else (recur)))))))

(def puppeteer (p/process ["node" ptr-script]))

(binding [*in* (-> puppeteer :out io/reader)]
  (loop []
    (when-let [line (read-line)]
      (println "[pptr] " line)
      (recur))))

(let [exit (:exit @puppeteer)]
  (println "Got an exit" exit)
  (System/exit exit))

(ns bb-main
  (:require [babashka.deps :as deps]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def shadow-config (edn/read-string (slurp "shadow-cljs.edn")))
(def ptr-script (-> shadow-config :builds :ptr :output-to))


(def npm-ci (p/process ["npm" "ci"]))

(binding [*in* (-> npm-ci :out io/reader)]
  (loop []
    (when-let [line (read-line)]
      (println "[npm]  " line)
      (recur))))

(when-not (zero? (:exit @npm-ci))
  (println "!!! Npm failed")
  (println (slurp (:err npm-ci)))
  (println "!!! Npm failed")
  (System/exit 1))


(defn io-prepl []
  (deps/clojure 
   ["-M"
    "-e" "(require '[clojure.core.server :as s])"
    "-e" "@user/shadow-server"
    "-e" "(s/io-prepl)"]
   {:in nil
    :out nil}))

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
          (println "[clj]  " line)
          (recur))
        (let [v (edn/read-string line)]
          (case (:tag v)
            :ret (:val v)
            :out (do (print "[clj]  " (:val v))
                     (recur))
            (do (prn 'clj-error v)
                (recur))))))))

(doseq [od ["artifact/screenshot"]]
 (io/make-parents (io/file od)))

(def puppeteer (p/process ["node" ptr-script]))

(binding [*in* (-> puppeteer :out io/reader)]
  (loop []
    (when-let [line (read-line)]
      (println "[pptr] " line)
      (recur))))

(let [exit (:exit @puppeteer)]
  (println "Got an exit" exit)
  (System/exit exit))

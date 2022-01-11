(ns bb-main
  (:require [babashka.deps :as deps]
            [babashka.process :as p]
            [clojure.string :as string]
            [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def shadow-config (edn/read-string (slurp "shadow-cljs.edn")))
(def ptr-script (-> shadow-config :builds :ptr :output-to))
(def electron-main (-> shadow-config :builds :elec :output-to))

(def chrome-paths [(str (System/getenv "LOCALAPPDATA") "\\Google\\Chrome SxS\\Application\\chrome.exe")])
(def puppeteer-env
  (when-let [found-chrome (first (filter #(-> % io/file (.exists)) chrome-paths))]
    {"PUPPETEER_EXECUTABLE_PATH" found-chrome
     "PUPPETEER_SKIP_DOWNLOAD" "1"}))

(defn npm-install []
  (when-not (fs/exists? "node_modules")
   (let [npm-ci (p/process ["npm" "ci"] {:extra-env puppeteer-env})]
     (binding [*in* (-> npm-ci :out io/reader)]
       (loop []
         (when-let [line (read-line)]
           (println "[npm]  " line)
           (recur))))
     (when-not (zero? (:exit @npm-ci))
       (println "!!! Npm failed")
       (println (slurp (:err npm-ci)))
       (println "!!! Npm failed")
       (System/exit 1)))))

(defn start-interactive
  []
  (deps/clojure []))

(defn compile-and-host-cljs
  []
  (let [the-prepl     (deps/clojure ["-M"
                                     "-e" "(require '[clojure.core.server :as s])"
                                     "-e" "@user/shadow-server"
                                     "-e" "(s/io-prepl)"]
                                    {:in  nil
                                     :out nil
                                     :err :inherit})
        input-writer  (io/writer (:in the-prepl))
        output-reader (-> the-prepl :out io/reader)]
    (binding [*out* input-writer]
      #_(println
         '(+ 91 94))
      (prn '(user/release-cljs!)))

    ;; wait for shadow to finish
    (binding [*in* output-reader]
      (loop []
        (when-let [line (read-line)]
          (prn 'L line)
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
                    (recur))))))))))


(defn screenshots []
  (doseq [od ["artifact/screenshot/whatever"]]
    (io/make-parents (io/file od)))
  (let [puppeteer (p/process ["node" ptr-script]
                             {:err :inherit
                              :extra-env puppeteer-env})]

    (binding [*in* (-> puppeteer :out io/reader)]
      (loop []
        (when-let [line (read-line)]
          (println "[pptr] " line)
          (recur))))

    (let [exit (:exit @puppeteer)]
      (println "OK"))))

(defn install-electron
  []
  (when-not (fs/exists? "node_modules/electron")
    @(p/process ["npm" "i" "electron"] {:out :inherit})))

(defn run-electron
  []
  (install-electron)
  (compile-and-host-cljs)
  (p/process
   [(string/trim (slurp (:out (p/process ["node" "-e" "console.log(require('electron'))"]))))
    electron-main]))

#_(run-electron)

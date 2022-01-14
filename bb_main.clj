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

(def value-expected (atom nil))

(defonce the-prepl
  (delay
    (println "Staring big clojure...")
    (let [{:keys [in out err]} (deps/clojure ["-M"
                                              "-e" "(require '[clojure.core.server :as s])"
                                              "-e" "@user/shadow-server"
                                              "-e" "(s/io-prepl)"]
                                             {:in nil
                                              :out nil
                                              :err nil
                                              :shutdown p/destroy-tree})]
      (future
        (binding [*in* (io/reader err)]
          (loop []
            (when-let [line (read-line)]
              (println "[clj err] " line)
              (recur)))
          (println "Error slurper done?")))
      
      (future
        (binding [*in* (io/reader out)]
          (loop []
            (when-let [line (read-line)]
              (if (not= "{" (subs line 0 1))
                (do
                  (println "[clj]  " line)
                  (recur))
                (let [v (edn/read-string line)]
                  (case (:tag v)
                    :ret (do
                           (when-let [ve @value-expected]
                             (deliver ve (:val v)))
                           (recur))
                    :out (do (print "[clj]  " (:val v))
                             (recur))
                    (do (prn 'clj-error v)
                        (recur)))))))))      
      
      {:out (io/writer in)})
    #_{:proc proc
       :writer (-> proc :in io/writer) 
       :reader (-> proc :out io/reader)}))

(defn sync-prepl-exec
  [body-expr]
  (let [{:keys [out]} @the-prepl
        ans (promise)]
    (reset! value-expected ans)
    (binding [*out* out] (prn body-expr))
    (println "Answer" @ans)
    @ans))


(def puppeteers (atom []))

(defn screenshots []
  (doseq [od ["artifact/screenshot/whatever"]]
    (io/make-parents (io/file od)))
  (let [puppeteer (p/process ["node" ptr-script]
                             {:extra-env puppeteer-env})]

    (swap! puppeteers conj puppeteer)
    (future
      (binding [*in* (-> puppeteer :err io/reader)]
        (loop []
          (when-let [line (read-line)]
            (println "[pptr err] " line)
            (recur)))
        (println "Error slurper done?")))
    
    (binding [*in* (-> puppeteer :out io/reader)]
      (loop []
        (when-let [line (read-line)]
          (println "[pptr] " line)
          (recur)))
      (println "In slurpy done?"))

    (let [exit (:exit @puppeteer)]
      (println "OK"))))

(defn maybe-install-electron
  []
  (when-not (fs/exists? "node_modules/electron")
    @(p/process ["npm" "i" "electron"] {:out :inherit})))

(def electron-exe
  (delay
    (maybe-install-electron)
    (-> ["node" "-e" "console.log(require('electron'))"]
        (p/process)
        :out
        (slurp)
        (string/trim))))

(def electron-process (atom nil))


(defn run-electron
  []
  #_(compile-and-host-cljs
     (quote (+ 1 1))
     #_(quote (user/release-cljs!)))
  
  (let [result (sync-prepl-exec (quote (do
                                         (shadow.cljs.devtools.api/release :elec)
                                         (shadow.cljs.devtools.api/release :br))))]
    (if-not (= ":done" result)
      (println "Failed to build")
      (reset! electron-process
              (p/process [@electron-exe electron-main "These" "Are" "Yourargs"])))))

(defn start-repl
  []
  (sync-prepl-exec
   (quote
    (shadow/watch :br))))

(comment
  (sync-prepl-exec
   (quote
    (shadow/release :br)
    #_(shadow/compile :ptr)
    #_(release-cljs!)
    #_(do
        (System/currentTimeMillis))))
  
  (future-cancel screenshot-future)
  (count @puppeteers)
  (for [c @puppeteers] (.isAlive (:proc c)))
  (future-done? screenshot-future)
  (def screenshot-future
    (future
      (let [result (sync-prepl-exec (quote (do
                                             (use 'df.async :reload)
                                             (shadow.cljs.devtools.api/compile
                                              :ptr))))]
        (if-not (= ":done" result)
          (println "Failed: " (pr-str result))
          (do (screenshots)
              (println "Finished in bb"))))))
  )
#_(run-electron)



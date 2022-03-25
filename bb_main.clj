(ns bb-main
  (:require [babashka.deps :as deps]
            [babashka.fs :as fs]
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

(defn maybe-npm-install []
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



(defn maybe-install-electron
  []
  (when-not (fs/exists? "node_modules/electron")
    @(p/process ["npm" "i" "electron"] {:out :inherit})))

(defn maybe-install-puppeteer
  []
  (when-not (fs/exists? "node_modules/puppeteer")
    @(p/process ["npm" "i" "puppeteer"] {:out :inherit})))

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
  
  (let [result (sync-prepl-exec (quote
                                 :done
                                 #_(do
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
      (println "Exit:" exit)
      exit)))

(comment
  (sync-prepl-exec
   (quote
    (do
      (shadow/release :br)
      (shadow/release :elec)
      (System/exit 0))
    
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

(defn compile-cljs!
  [b]
  (when (not= ":done" (sync-prepl-exec `(shadow.cljs.devtools.api/compile ~b)))
    (throw (ex-info "Shadow failed" {:b b}))
    :ok))

(defn release-cljs!
  [b]
  (when (not= ":done" (sync-prepl-exec `(shadow.cljs.devtools.api/release ~b)))
    (throw (ex-info "Shadow failed" {:b b}))
    :ok))



(def electron-files
  [electron-main
   "srv/electron_preload.js"
   "srv/electron_preload_no_isolation.js"
   "srv/font/iosevka-aile-light.woff2"
   "srv/font/iosevka-term-ss03-light.woff2"
   "srv/index.html"
   "srv/index.css"
   "srv/js/main.js"
   "srv/pdf.min.js"
   "srv/pdf.worker.min.js"
   #_"package.json"
   ])

(defn package-electron
  [out]
  (doseq [e electron-files]
    (let [of (fs/path out e)]
      (fs/create-dirs (fs/parent of))
      ((if (fs/directory? e) fs/copy-tree fs/copy)
         e
         (fs/path out e)
         {:replace-existing true}))))

(comment
  (package-electron "/tmp/temperloy"))


(defn He
  ([] (He (str "subtree/" (System/currentTimeMillis) )))
  ([out]
   (release-cljs! :elec)
   (release-cljs! :br)
   (package-electron out)
   (reset! electron-process (p/process [@electron-exe (fs/path out electron-main) "--localhost"]))
   (sync-prepl-exec '(do (shadow.cljs.devtools.api/watch :br)
                         #_(shadow.cljs.devtools.api/compile :br)))
   (sync-prepl-exec '(do (shadow.cljs.devtools.api/watch :elec)
                         #_(shadow.cljs.devtools.api/compile :elec)))
   :started))

(ns sted.electron-main
  (:require ["electron" :as electron :refer [app remote Menu BrowserWindow crashReporter]]
            ["path" :as path]))

(def ^:export browser-window-opts
  (clj->js
   {:width 1920
    :height 1200
    :webPreferences {:preload #_(str js/__dirname "/electron_preload.js")
                     (str js/__dirname "/electron_preload_no_isolation.js")
                     :contextIsolation false
                     :nodeIntegration true}}))

(defn init-browser
  []
  #_(.setApplicationMenu Menu nil)
  (let [[electron me u :as argv]        (some-> js/process (.-argv) js->clj)
        dev-version?        (some #{"--localhost"} argv)
        _                   (println "Init browser, argv=" argv)
        file-src            (str "file://" js/__dirname "/index.html")
        ;; localhost-url        #_ "http://localhost:8087/?title=Shadow host"
        
        window-open-handler (fn [_url]
                              #js {:action                       "allow"
                                   :overrideBrowserWindowOptions browser-window-opts})
        
        ^js/electron.BrowserWindow main-window (BrowserWindow. browser-window-opts)]
    
    (js/console.log "loadURL u=" (pr-str u))
    (js/console.log "file-src=" (pr-str file-src))
    
    (.loadURL main-window #_u  file-src)
    (doto (.-webContents main-window)
      (.setWindowOpenHandler window-open-handler))
    
    #_(when dev-version?
        (.loadURL second-window localhost-url)
        (doto (.-webContents second-window)
          (.setWindowOpenHandler window-open-handler)
          (.openDevTools)))))

(defn main []
  #_(.on app "window-all-closed" #(when-not (= js/process.platform "darwin")
                                    (.quit app)))
  (js/console.log "Electron main")
  (.on app "window-all-closed" #(.quit app))
  (.on app "ready" init-browser))



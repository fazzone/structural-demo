(ns sted.electron-main
  (:require ["electron" :as electron :refer [app remote Menu BrowserWindow crashReporter]]))

(def ^:export browser-window-opts
  (clj->js
   {:width 1920
    :height 1200
    :webPreferences {:preload (str js/__dirname "/electron_preload.js")}}))



(defn init-browser
  []
  #_(.setApplicationMenu Menu nil)
  (let [dev-version? (some #{"--localhost"}
                           (some-> js/process
                                   (.-argv)
                                   js->clj))
        
        file-src      (str "file://" js/__dirname "/index.html")
        localhost-url "http://localhost:8087/?title=Shadow host"
        
        window-open-handler (fn [_url]
                              #js {:action "allow"
                                   :overrideBrowserWindowOptions browser-window-opts})
        
        ^js/electron.BrowserWindow main-window   (BrowserWindow. browser-window-opts)]
    
    (.loadURL main-window file-src)
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
  (.on app "window-all-closed" #(.quit app))
  (.on app "ready" init-browser))

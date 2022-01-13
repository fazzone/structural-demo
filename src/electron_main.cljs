(ns electron-main
  (:require ["electron" :as electron :refer [app Menu BrowserWindow crashReporter]])
  
  )

(def main-window (atom nil))

(defn init-browser []
  #_(.setApplicationMenu Menu nil)
  (reset! main-window (BrowserWindow.
                       (clj->js {:width 1920
                                 :height 1200
                                 :webPreferences {:preload (str js/__dirname "/electron_preload.js")
                                                  #_ #_:nodeIntegration true}})))
  
  ;; Path is relative to the compiled js file (main.js in our case)
  (.loadURL ^js/electron.BrowserWindow @main-window
            (str "file://" js/__dirname "/index.html"))
  (.openDevTools (.-webContents  ^js/electron.BrowserWindow @main-window))
  (.on ^js/electron.BrowserWindow @main-window "closed" #(reset! main-window nil)))

(defn main []
  #_(.start crashReporter
            (clj->js
             {:companyName "MyAwesomeCompany"
              :productName "MyAwesomeApp"
              :submitURL "https://example.com/submit-url"
              :autoSubmit false}))

  #_(.on app "window-all-closed" #(when-not (= js/process.platform "darwin")
                                  (.quit app)))
  (.on app "window-all-closed" #(.quit app))
  
  (.on app "ready" init-browser))

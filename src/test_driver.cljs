(ns test-driver
  (:require
   [df.async :as a]
   ["puppeteer" :as pt]
   ["process" :as process]
   ["http" :as http]
   ["path" :as path]))

;; const puppeteer = require('puppeteer');


;; (async () => {


;;   const browser = await puppeteer.launch();


;;   const page = await browser.newPage();


;;   await page.goto('https://example.com');


;;   await page.screenshot({ path: 'example.png' });


;;   await browser.close();


;; })();


(defonce refs (atom {}))

(def test-params
  {:example {:form '[a ^:form/highlight nice [This is my awesome form where I am typing stuff in the test driver] b c]
             :mutations [[:delete-right]
                         [:barf-right]
                         [:barf-right]
                         [:delete-right]
                         [:flow-left]
                         [:barf-right]]}})

(defn setup-server
  []
  (doto (.createServer http
                       (fn [req ^js res]
                         (js/console.log req)
                         (.writeHead res 200)
                         (.end res "Dingus")))
    (.listen 9997)))

(comment
  (str "file://" (js/process.cwd) "/srv/index.html"
       (str "?"
            (binding [*print-meta* true]
              (-> test-params
                  (pr-str)
                  (js/Buffer.from "utf-8")
                  (.toString "base64"))))))

#_(defn main []
  (println "Start main")
  #_(go)
  (.then (.launch pt #js {:headless true
                          :defaultViewport #js {:width 1280 :height 1024}})
         (fn [^js browser]
           (swap! refs assoc :browser browser)
           (.then (.newPage browser)
                  (fn [^js page]
                    (swap! refs assoc :page page)
                    (.then (.evaluate page (fn [ps] (set! js/window.mytestparams ps))
                                      (clj->js
                                       {:Does :This
                                        :Work [1 2 3 4]}))
                           (fn [qqq]
                             (.on page "console" (fn [^js e] (js/console.log "[page]" (.text e))))
                             (-> (.start (.-tracing page)
                                         #js {:path "artifact/screenshot/profile.json"})
                                 (.then (fn [_]
                                          (-> (.goto page "http://localhost:8087")
                                              (.then (fn [e]
                                                       (-> (.stop (.-tracing page))
                                                           (.then (fn [_]
                                                                    (swap! refs :thisreturn e)
                                                                    (js/console.log "Finisherer")
                                                                    (let [op  "artifact/screenshot/example.png"]
                                                                      (-> (.screenshot page #js {:path op :fullPage true})
                                                                          (.then (fn [s]
                                                                                   (println "Wrote " op)
                                                                                   (-> (.close browser)
                                                                                       (.then (fn [c]
                                                                                                (println "Closed browser ")
                                                                                                (js/process.exit 0))))))))))))))))))))))))

(def ^:const ptr-opts
  #js {:headless true
       :defaultViewport #js {:width 1000 :height 800}})

(defn tseq
  [^js page i ts]
  (println "TSeq")
  (a/let [outf (str "artifact/screenshot/state" i ".png")
          _ (.type (.-keyboard page) ts #js {:delay 20})
          _ (.waitForTimeout page 50)
          _ (.screenshot page #js {#_#_:fullPage true :path outf})
          ;; _ (println "Wrote" outf)]
    outf))

(defn go []
  (a/let [^js browser (.launch pt ptr-opts)
          _ (.on browser "disconnected"
                 (fn []
                   (js/console.log "Chromeclosed")
                   (js/process.exit 0)))
          ^js page (.newPage browser)
          ;; _ (.start (.-tracing page))
          cdp-client (.createCDPSession (.target page))
          _ (.send cdp-client "Overlay.setShowFPSCounter" #js {:show true})
          ;; _ (.goto page "http://localhost:8087")
          _ (println "Js dirname" js/__dirname)
          uu (str "file:///" (-> js/__dirname
                                 (path/dirname)
                                 (path/join "index.html")))
          _ (println "File url?" uu)
          _ (.goto page uu)
          ;; _ (.stop (.-tracing page))
          _ (tseq page 0 "z")
          _ (reduce
             (fn [p [i t]]
               (.then p (fn [] (tseq page i t))))
             (js/Promise.resolve nil)
             (map vector (range) (map str (str "zfjjjj" "jl first-to" "ken\n1j" "jonext-token\n1jj" "dfothird-token\n1jff last-"
                                               "token\nfafaafffffffaafff"
                                               "1jjjjlhc_cH1kf"
                                               "f;done\n")))
             #_[])
          _ (when (aget ptr-opts "headless")
              (println "closing headless")
              (.close browser))]
    (println "Nice.")
    #_(js/process.exit 0)))

(defn main []
  (println "Start main")
  (go))

(defn ^:dev/after-load loady [])
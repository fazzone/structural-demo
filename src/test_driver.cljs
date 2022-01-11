(ns test-driver
  (:require
   ["puppeteer" :as pt]
   ["process" :as process]
   ["http" :as http]))


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

(defn ^:dev/after-load go []
  (println "After load")
  #_(setup-server)
  (js/console.log "asdf"))

(defn main []
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
                               (.on page "console" (fn [^ js e] (js/console.log "[page]" (.text e))))
                               (.then (.goto page
                                             #_(str "file://" (js/process.cwd) "/srv/index.html"
                                                    (str "?"
                                                         (binding [*print-meta* true]
                                                           (-> test-params
                                                               (pr-str)
                                                               (js/Buffer.from "utf-8")
                                                               (.toString "base64")))))
                                             "http://localhost:8087")
                                      (fn [e]
                                        (swap! refs :thisreturn e)
                                        (js/console.log "Finisherer")
                                        (.then (.screenshot page #js {:path "ya DINGUS.png"
                                                                      :fullPage true})
                                               (fn [s]
                                                 (.then (.close browser)
                                                        (fn [c]
                                                          
                                                          (println "Closed")
                                                          (js/process.exit 0))))))))))))))

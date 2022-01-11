(ns test-driver
  (:require
   ["puppeteer" :as pt]
   ["process" :as process]))


;; const puppeteer = require('puppeteer');

;; (async () => {
;;   const browser = await puppeteer.launch();
;;   const page = await browser.newPage();
;;   await page.goto('https://example.com');
;;   await page.screenshot({ path: 'example.png' });

;;   await browser.close();
;; })();


(defonce refs (atom {}))

(defn main []
  (.then (.launch pt #js {:headless true
                          :defaultViewport #js {:width 1280 :height 1024}})
         (fn [^js browser]
           (swap! refs assoc :browser browser)
           (.then (.newPage browser)
                  (fn [^js page]
                    (swap! refs assoc :page page)
                    (.then (.goto page
                                  (str "file://" (js/process.cwd) "/srv/index.html")
                                  #_"http://localhost:8087")
                           (fn [e]
                             (swap! refs :thisreturn e)
                             (js/console.log "Finisherer")
                             (.then (.screenshot page #js {:path "example.png"
                                                           :fullPage true})
                                    (fn [s]
                                      (.then (.close browser)
                                             (fn [c] (println "Closed"))))))))))))

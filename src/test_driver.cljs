(ns test-driver
  (:require
   ["puppeteer" :as pt]
   
   )
  
  )


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
                          :defaultViewport #js {:width 3840 :height 2160}})
         (fn [^js browser]
           (swap! refs assoc :browser browser)
           (.then (.newPage browser)
                  (fn [^js page]
                    (swap! refs assoc :page page)
                    (.then (.goto page "http://localhost:8087")
                           (fn [e]
                             (swap! refs :thisreturn e)
                             (js/console.log "Finisherer")
                             (.then (.screenshot page #js {:path "example.png"})
                                    (fn [s]
                                      (.then (.close browser)
                                             (fn [c] (println "Closed"))))))))))))

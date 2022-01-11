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
  (js/console.log "Ok(")
  (.then (.launch pt #js {:headless false
                          :defaultViewport #js {:width 1280 :height 1024}})
         (fn [^js browser]
           (swap! refs assoc :browser browser)
           (.then (.newPage browser)
                  (fn [^js page]
                    (swap! refs assoc :page page)
                    (.then (.goto page "http://localhost:8087")
                           (fn [e]
                             (swap! refs :thisreturn e)
                             (js/console.log "Finisherer"))))))))

(ns test-driver
  (:require
   [df.async :as a]
   [rum.core :as rum]
   ["puppeteer" :as pt]
   ["process" :as process]
   ["http" :as http]
   ["path" :as path]
   ["fs" :as fs]))

(defonce refs (atom {}))

(def test-params
  {:example {:form '[a ^:form/highlight nice
                     [This is my awesome form where I am typing stuff in the
                      test driver] b c]
             :mutations [[:delete-right] [:barf-right] [:barf-right]
                         [:delete-right] [:flow-left] [:barf-right]]}})

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

#_(def viewport-width 1000)

(def viewport-width 320)

#_(def viewport-height 800)

(def viewport-height 50)

(def ^:const ptr-opts
  #js {:headless false
       :defaultViewport #js {:width viewport-width :height viewport-height}})

(rum/defc test-report-root
  [fs]
  (prn "Fs:" fs)
  [:div
   {:style {:font-family "monospace"
            :background-color "#000"
            :color "#fff"
            :display :grid
            :grid-template-columns "minmax(3em, auto) 2fr"}}
   (for [{:keys [index typed screenshot]} fs]
     (rum/fragment [:span {:key (inc (* 2 index))} (pr-str typed)]
                   [:img
                    {:key (dec (* 2 index))
                     :src screenshot
                     :width viewport-width
                     :height viewport-height
                     :style {:border "1px dashed aliceblue"}}]))])

(def report-filename "test-report.html")

(defonce last-result (atom nil))

(defn report-url [f]
  (str "file:///"
       (-> js/__dirname
           (path/dirname)
           (path/dirname)
           (path/join f))))

(defonce the-browser (atom nil))

(defn get-browser!
  []
  (or (some-> the-browser
              deref
              (js/Promise.resolve))
      (a/let [^js browser (.launch pt ptr-opts)]
        (reset! the-browser browser)
        (doto browser
          (.on "disconnected"
               (fn [] (js/console.log "Chromeclosed") (js/process.exit 0)))))))

(defn tseq
  [^js page i ts]
  (println "TSeq" (pr-str ts))
  (a/let [outf (str "artifact/screenshot/state" i ".png")
          _ (.type (.-keyboard page) ts #js {:delay 20})
          #__ #_(.waitForTimeout page 50)
          _ (.screenshot page #js {#_#_:fullPage true :path outf})]
    outf))

(defn run-steps
  [^js browser url steps]
  (a/let [^js page (.newPage browser)
          _ (.goto page url)
          result (reduce (fn [p [i t]]
                           (a/let [acc p
                                   out (tseq page i t)]
                             (conj acc {:index i  :typed t  :screenshot out})))
                   (js/Promise.resolve [])
                   (map vector (range) steps))
          _ (.close page)]
    result))

(defn go
  []
(a/let [^js browser (get-browser!)
          uu (str "file:///"
                  (-> js/__dirname
                      (path/dirname)
                      (path/join "index.html")))
          result (run-steps browser uu (map str (apply str ["oa b c d e f g\n" "aa" "aaff" "9999" "hhhh" "pppp" "wwPP"])))
          _ (prn "Result " result)
          _ (.writeFile fs/promises
                        report-filename
                        (rum/render-html (test-report-root result)))
          _ (if (aget ptr-opts "headless")
              (do (println "closing headless") (.close browser)))]
    (reset! last-result result)
    (println "Nice.")
    #_(js/process.exit 0))
  #_(a/let [^js browser (get-browser!)
          ^js page (.newPage browser)
          ;; _ (.start (.-tracing page))
          #_cdp-client #_(.createCDPSession (.target page))
          #__ #_(.send cdp-client "Overlay.setShowFPSCounter" #js {:show true})
          ;; _ (.goto page "http://localhost:8087")
          _ (println "Js dirname" js/__dirname)
          uu (str "file:///"
                  (-> js/__dirname
                      (path/dirname)
                      (path/join "index.html")))
          _ (println "File url?" uu)
          _ (.goto page uu)
          ;; _ (.stop (.-tracing page))
          result
            (reduce
              (fn [p [i t]]
                (.then
                  p
                  (fn [a]
                    (.then (tseq page i t)
                           (fn [out]
                             (conj a {:index i  :typed t  :screenshot out}))))))
              (js/Promise.resolve [])
              (map vector (range) (map str (apply str ["of a b c d e f g " "h\n" "aaa" "9hwwwpp"])))
              #_[])
          _ (prn "Result " result)
          _ (.writeFile fs/promises
                        report-filename
                        (rum/render-html (test-report-root result)))
          _ (if (aget ptr-opts "headless")
              (do (println "closing headless") (.close browser))
              (do (println "Report url" (report-url report-filename))
                  (.goto page (report-url report-filename))))]
    (reset! last-result result)
    (println "Nice.")
    #_(js/process.exit 0)))

(defn main []
  (println "Start main")
  (go))

(comment)

(defn ^:dev/after-load loady
  []
  (println "Loady!" (deref last-result) (deref the-browser))
  (go)
  #_(when-some [r (deref last-result)]
      (.writeFile fs/promises
                  report-filename
                  (rum/render-html (test-report-root r)))))
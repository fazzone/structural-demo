(ns sted.test-driver
  (:require
   [sted.df.async :as a]
   [rum.core :as rum]
   [sted.sys.kbd.map :as skm]
   [clojure.set :as set]
   ["puppeteer" :as pt]
   ["process" :as process]
   ["http" :as http]
   ["path" :as path]
   ["fs" :as fs]))

(def inverse-keymap (set/map-invert skm/default-keymap))

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

(def viewport-width 1320)
(def viewport-height 1080)

#_(def viewport-width 320)
#_(def viewport-height 50)

(def ^:const ptr-opts
  #js {:headless true
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
          _ (.evaluate page (fn [] (doseq [j (range 9)] (println "j" j))))
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
          #__ #_(.close page)]
    
    result))

(defn type-mutation
  [^js page mut args]
  (when args (throw (ex-info "no args yet" {:args args})))
  (let [keys (sk/kbd->keydowns (get inverse-keymap mut ))]
    (reduce (fn [p [du k]]
              (a/let [acc p
                      _ (case du
                          :up (.up (.-keyboard page) k)
                          :down (.down (.-keyboard page) k))]
                (conj acc [[du k]])))
            []
            (doto
                (concat (map (partial vector :down) keys)
                        (map (partial vector :up) (reverse keys)))
              prn))))

(def ^:dynamic *delay-ms* 20)

(defn type-chars
  [^js page s]
  (.type (.-keyboard page) s
         #js {:delay
              20
              
              #_*delay-ms*}))

(defn single-step*
  [^js page [d a & args]]
  (case d
    :mutate (type-mutation page a args)
    :type (type-chars page a)))

(defn single-step
  [^js page step]
  (a/let [_ (println "Start" (pr-str step))
          sr (single-step* page step)
          _ (.waitForTimeout page *delay-ms*)
          _ (println "End" (pr-str step))]
    sr))

(defn run-steps-next
  [^js browser url muts]
  (a/let [^js page (.newPage browser)
          _ (.setViewport page #js {:width viewport-width :height viewport-height})
          _ (.goto page url)
          result (reduce (fn [p [i t]]
                           (a/let [acc p
                                   out (single-step page t)]
                             (conj acc {:index i  :mut t
                                        ;; :screenshot out
                                        })))
                         (js/Promise.resolve [])
                         (map vector (map inc (range)) muts))
          #__ #_(.close page)]
    (run! prn result)
    result))

(comment
              ;; _ (.start (.-tracing page))
              ;; _ (.stop (.-tracing page))
  cdp-client (.createCDPSession (.target page))
  _ (.send cdp-client "Overlay.setShowFPSCounter" #js {:show true}))


(defn go
  []
  (a/let [^js browser (get-browser!)
          uu (str "file:///"
                  (-> js/__dirname
                      (path/dirname)
                      (path/join "index.html")))
          result (run-steps-next browser uu
                                 #_[[:mutate :offer]
                                    [:type "ya\n"]
                                    [:mutate :parent]
                                    [:mutate :clone]
                                    [:mutate :clone]
                                    [:mutate :clone]
                                    [:mutate :next]
                                    [:mutate :slurp-right]
                                    [:mutate :slurp-right]
                                    [:mutate :float]
                                    [:mutate :offer]
                                    [:type "dingus\n"]
                                    [:mutate :wrap]
                                    [:mutate :parent]
                                    [:mutate :hoist]
                                    [:mutate :m1]
                                    [:mutate :flow-right]
                                    [:mutate :gobble]
                                    ]
                                 #_(concat [[:mutate :offer]
                                            [:type "1\n"]]
                                           (apply concat
                                                  (repeat 5
                                                          [[:mutate :plus]
                                                           [:mutate :parent]
                                                           [:mutate :clone]
                                                           [:mutate :zp]
                                                           [:mutate :slurp-right]
                                                           [:mutate :flow-right]])))

                                 (concat [[:mutate :offer]
                                          [:type "ingest\n"]
                                          [:mutate :new-vec]
                                          [:type "+ 0 1\n"]
                                          [:mutate :prev]
                                          [:mutate :prev]
                                          [:mutate :wrap]
                                          [:mutate :slurp-right]
                                          [:mutate :slurp-right]]
                                         #_(repeat 5 [:type "1eslk_gfr"])
                                         (apply concat
                                                (repeat 20
                                                        [[:mutate :m1]
                                                         [:mutate :eval-sci]
                                                         [:mutate :tail]
                                                         [:mutate :prev]
                                                         [:mutate :hoist]
                                                         #_[:mutate :linebreak]
                                                         [:mutate :m1]
                                                         [:mutate :sink]
                                                         [:mutate :tail]
                                                         [:mutate :gobble]
                                                         [:mutate :flow-right]
                                                         [:mutate :raise]])))
                                 #_(map str (apply str ["oa b c d e f g\n" "aa" "aaff" "9999" "hhhh" "pppp" "wwPP"])))
          _ (.writeFile fs/promises
                        report-filename
                        (rum/render-html (test-report-root result)))
          _ (if (aget ptr-opts "headless")
              (do (println "closing headless") (.close browser)))]
    (reset! last-result result)
    (println "Nice.")
    #_(js/process.exit 0)))

(defn main []
  (println "Start main")
  (go))

(comment)

(defn ^:dev/after-load loady
  []
  #_(println "Loady!" (deref last-result) (deref the-browser))
  (go)
  #_(when-some [r (deref last-result)]
      (.writeFile fs/promises
                  report-filename
                  (rum/render-html (test-report-root r)))))

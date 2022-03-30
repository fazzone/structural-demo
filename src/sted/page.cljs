(ns sted.page
  (:require
   [clojure.edn :as edn]
   [sted.embed :as e]
   [sted.embed.common :as ec]
   [sted.schema :as s]
   [goog.string :as gstring]
   [goog.functions :as gfunc]
   [datascript.core :as d]
   [clojure.datafy :as datafy]
   [clojure.string :as string]
   [rum.core :as rum]
   [cljs.core.async :as async]
   [sci.core :as sci]
   [sted.db-reactive :as dbrx]
   [sted.comp.cons :as cc]
   [sted.comp.edit-box :as eb]
   [sted.comp.code :as code]
   [sted.comp.scroll :as csc]
   [sted.comp.common :as ccommon]
   [sted.comp.modeline :as ml]
   [sted.comp.root :as cr]
   [sted.cmd.move :as move]
   [sted.cmd.nav :as nav]
   [sted.cmd.insert :as insert]
   [sted.cmd.edit :as edit]
   [sted.cmd.mut :as mut]
   [sted.df.github :as dfg]
   [sted.df.async :as a]
   [sted.sys.eval.sci :as eval-sci]
   [sted.sys.fmt :as sf]
   [sted.sys.kbd.map :as skm]
   [sted.sys.kbd.evt :as ske]
   [sted.sys.search.setup :as search]
   [sted.sys.mouse :as sm]
   [sted.sys.handle :as sh]
   [sted.sys.keyboard :as sk]
   [goog.object :as gobj]
   [zprint.core :as zp-hacks]
   [sted.core :as core :refer [get-selected-form]]
   
   ["svgo/dist/svgo.browser" :as svgo]
   ["svgo/plugins/_applyTransforms" :as svgo-applytransforms]
   ["svgo/plugins/_transforms" :as svgo-transforms]
   ["svgo/plugins/_path" :as svgo-path]
   [sted.eda.dsn :as dsn])
  (:require-macros
   [cljs.core.async.macros :refer [go
                                   go-loop]]
   [sted.macros :as m]))


(def ^js pdfjs (gobj/get js/window "pdfjs-dist/build/pdf"))

(def test-form-data-bar (assoc (e/string->tx-all (m/macro-slurp "src/sted/user.clj"))
                               :chain/filename  "src/sted/user.clj"))

(def init-tx-data
  (let [chains (concat
                #_[(e/string->tx-all (m/macro-slurp "src/core.cljc"))]
                #_[(e/string->tx-all (m/macro-slurp "src/cmd/edit.cljc"))]
                [test-form-data-bar]
                #_[(e/->tx ["hello"
                            "hello"
                            "hello"
                            "hello"
                            "hello"
                            "hello"
                          
                            ])]
                #_[(e/->tx [^:form/highlight ()])]
                #_[(e/string->tx-all (m/macro-slurp "subtree/input.clj"))])]
    [{:db/ident ::state
      :state/bar "bar"
      :state/limit 512}
     {:db/ident ::command-chain
      :db/id "command-chain"
      :coll/type :vec
      :form/highlight true}
     {:db/ident ::inspect  :db/id "inspect"
      :coll/type :demo
      
      }
     {:db/ident ::default-keymap
      :db/id "defaultkeymap"
      :coll/type :keyboard
      :keymap/bindings (for [[k m] skm/default]
                         {:key/kbd k  :key/mutation m})}
     (assoc (ec/seq-tx
             (concat
              (for [ch chains]
                (assoc ch
                       :coll/type :chain
                       :coll/_contains "bar"))
              [{:db/ident ::meta-chain
                :coll/type :chain
                :coll/_contains "bar"
                :coll/contains #{"label" "defaultkeymap" "inspect"
                                 "command-chain"}
                :seq/first {:db/id "label"
                            :token/type :string
                            :token/value "Keyboard"}
                :seq/next {:seq/first "defaultkeymap"
                           :seq/next {:seq/first "command-chain"
                                      :seq/next {:seq/first "inspect"}}}}]))
            :db/id "bar"
            :coll/type :bar)]))

#_(declare snapshot)

#_(rum/defc display-undo-preview
  [c b s top?]
  [:ul.undo-preview
   #_(when s {:class s :ref "selected"})
   {}
   (when top?
     [:span.form-title "History"])
   (for [e (cond-> (e/seq->vec c) (not top?) reverse)]
     (rum/with-key
       (snapshot e b)
       (:db/id e)))])

#_(rum/defc snapshot < dbrx/ereactive
  [e bus]
  (let [tx (int (:token/value e))
        r  (core/get-history bus tx)]
    (if-not tx
      (display-undo-preview e bus (when (:form/highlight e) "selected") nil)
      [:li.undo-preview-entry
       {:class [(when (:form/highlight e)
                  "selected")]}
       (str tx " " (some-> r :mut pr-str))
       #_(apply str (interpose " " (map pr-str (:mut r))))
       (if-not (some (fn [[e a v t]] (not= a :form/highlight))
                     (:tx-data r))
         " movement only"
         (when r
           [:div.alternate-reality
            {}
            ^:inline (-> (peek (nav/parents-vec (get-selected-form (:db-after r))))
                         (fcc core/blackhole 0 nil))]))])))

#_(defmethod display-coll :undo-preview  [c b i s p]
  (display-undo-preview c b s true))

(defn ask-download-file
  [path contents]
  (let [b (js/Blob. #js [contents])
        u (js/URL.createObjectURL b)
        a (js/document.createElement "a")]
    (set! (.-href a) u)
    (set! (.-download a) path)
    (.click a)))

(defn save*
  [path contents]
  #_(.writeFile (js/require "fs/promises") file contents)
  (or (when-let [require (aget js/window "secret_electron_require")]
        (-> (.writeFile ^js (require "fs/promises") path contents )
            (.then  (fn [] (swap! ml/save-status assoc :status :ok :file path)))
            (.catch (fn [] (swap! ml/save-status assoc :status :error)))))
      (do (ask-download-file path contents)
          (swap! ml/save-status assoc :status :ok :file path)))
  
  #_(if-let [spit (some-> (aget js/window "my_electron_bridge")
                          (aget "spit"))]
      (-> (spit file contents)
          (.then  (fn [] (swap! ml/save-status assoc :status :ok :file file)))
          (.catch (fn [] (swap! ml/save-status assoc :status :error))))
      #_(do
          (js/console.time "Thing")
          (prn  (thinger "c"))
          (js/console.timeEnd "Thing"))
      (let [w (js/window.open "")]
        (js/setTimeout
         (fn []
           (let [el (js/document.createElement "pre")]
             (set! (.-innerText el) contents)
             (.appendChild (.-body (.-document w)) el)))
         0))))

(defonce ^:export the-app (atom nil))

(defn setup-app
  ([] (setup-app (doto (d/create-conn s/schema) (d/transact! init-tx-data))))
  ([conn]
   (let [zb (some-> the-app deref :bus (core/reset))
         _ (js/console.log "Zbus" zb)
         a (core/app conn zb)]
     (doseq [[m f] mut/movement-commands]
       (core/register-simple! a m (core/movement->mutation f)))
     (doseq [[m f] mut/editing-commands]
       (core/register-simple! a m f))
     (-> a
         
         (sh/setup!)
         
         (core/register-mutation! :eval-sci (eval-sci/mutatef a the-app))
         (core/register-simple! :zp (sf/mutatef a))
         (core/register-mutation! :scroll (fn [_ _ _] (csc/scroll-to-selected!)))
         (sk/setup!)
         (sm/setup!)
         (search/setup!)
         (core/register-mutation!
          :save
          (fn [_ db bus]
            (let [sel (get-selected-form db)
                  chain
                  (cond-> sel
                    (not= :chain (:coll/type sel))
                    (-> (nav/parents-vec)
                        (peek)
                        :coll/_contains
                        (first)))
                  file (or (:chain/filename chain) "noname.clj")]
              (when chain
                (reset! ml/save-status {:at (:max-tx db)
                                        :on (:db/id sel)
                                        :status :saving})
                (save* file (e/->string chain)))
              nil)))))))

(defonce the-singleton-db
  (doto (d/create-conn s/schema)
    (d/transact! init-tx-data)))

(defn fetch-json
  [u]
  (-> (js/fetch u)
      (.then #(.json %))
      (.then #(js->clj % :keywordize-keys true))))

(defn stupid-github-crap
  []
  (a/let [ref    (fetch-json "https://api.github.com/repos/babashka/sci/git/ref/heads/master")
          commit (fetch-json (-> ref :object :url))
          tree   (fetch-json (-> commit :tree :url))]
    #_(cljs.pprint/pprint tree)))



(defonce set-scroll-user
  (fn [ev]
    (vswap! core/scroll-sequence-number inc)))

(defn ^:dev/before-load stop []
  (js/console.log "stop")
  (let [ls (:listeners (meta the-singleton-db))]
    (doseq [l (vals @ls)]
      (println "Cleaning listener" (meta l)))
    (reset! ls {}))
  (println "Cleared DB listeners")
  (swap! the-app
         #(-> %
              (sk/cleanup!)
              (sm/cleanup!))))

(rum/defc zsvg
  []
  (let [size 1000]
    [:svg {:viewBox (str "0 0 " size " " size)
           :style   {:width "800px"
                     :border "1px solid aliceblue"}}
     (let [rmax 20
           cmax 10
           ps (* 0.002 size)
           hpitch (* 0.02 size)
           vpitch (* 0.02 size)
           tlx (* 0.4 size)
           tly (* 0.4 size)]
      (for [r (range rmax)
            c (range cmax)]
        [:circle {:r ps
                  :fill   "#ae81ff"
                  :stroke "none"
                  :cx (+ tlx (* hpitch c))
                  :cy (+ tly (* vpitch r))}]))]))

(def kicadstr-kailhsocket
  "(module Kailh_socket_MX (layer F.Cu) (tedit 5DD4FB17)
  (descr \"MX-style keyswitch with Kailh socket mount\")
  (tags MX,cherry,gateron,kailh,pg1511,socket)
  (attr smd)
  (fp_text reference REF** (at 0 -8.255) (layer B.SilkS)
    (effects (font (size 1 1) (thickness 0.15)) (justify mirror))
  )
  (fp_text value Kailh_socket_MX (at 0 8.255) (layer F.Fab)
    (effects (font (size 1 1) (thickness 0.15)))
  )
  (fp_line (start -7 -6) (end -7 -7) (layer F.SilkS) (width 0.15))
  (fp_line (start -7 7) (end -6 7) (layer F.SilkS) (width 0.15))
  (fp_line (start -6 -7) (end -7 -7) (layer F.SilkS) (width 0.15))
  (fp_line (start -7 7) (end -7 6) (layer F.SilkS) (width 0.15))
  (fp_line (start 7 6) (end 7 7) (layer F.SilkS) (width 0.15))
  (fp_line (start 7 -7) (end 6 -7) (layer F.SilkS) (width 0.15))
  (fp_line (start 6 7) (end 7 7) (layer F.SilkS) (width 0.15))
  (fp_line (start 7 -7) (end 7 -6) (layer F.SilkS) (width 0.15))
  (fp_line (start -6.9 6.9) (end 6.9 6.9) (layer Eco2.User) (width 0.15))
  (fp_line (start 6.9 -6.9) (end -6.9 -6.9) (layer Eco2.User) (width 0.15))
  (fp_line (start 6.9 -6.9) (end 6.9 6.9) (layer Eco2.User) (width 0.15))
  (fp_line (start -6.9 6.9) (end -6.9 -6.9) (layer Eco2.User) (width 0.15))
  (fp_line (start -7.5 -7.5) (end 7.5 -7.5) (layer F.Fab) (width 0.15))
  (fp_line (start 7.5 -7.5) (end 7.5 7.5) (layer F.Fab) (width 0.15))
  (fp_line (start 7.5 7.5) (end -7.5 7.5) (layer F.Fab) (width 0.15))
  (fp_line (start -7.5 7.5) (end -7.5 -7.5) (layer F.Fab) (width 0.15))
  (fp_arc (start -3.81 -4.445) (end -3.81 -6.985) (angle -90) (layer B.SilkS) (width 0.15))
  (fp_line (start -6.35 -1.016) (end -6.35 -0.635) (layer B.SilkS) (width 0.15))
  (fp_arc (start 0 0) (end 0 -2.54) (angle -75.96375653) (layer B.SilkS) (width 0.15))
  (fp_line (start 5.08 -3.556) (end 5.08 -2.54) (layer B.SilkS) (width 0.15))
  (fp_line (start 5.08 -2.54) (end 0 -2.54) (layer B.SilkS) (width 0.15))
  (fp_line (start -2.464162 -0.635) (end -4.191 -0.635) (layer B.SilkS) (width 0.15))
  (fp_line (start -5.969 -0.635) (end -6.35 -0.635) (layer B.SilkS) (width 0.15))
  (fp_line (start -6.35 -4.445) (end -6.35 -4.064) (layer B.SilkS) (width 0.15))
  (fp_line (start -3.81 -6.985) (end 5.08 -6.985) (layer B.SilkS) (width 0.15))
  (fp_line (start 5.08 -6.985) (end 5.08 -6.604) (layer B.SilkS) (width 0.15))
  (fp_arc (start -3.81 -4.445) (end -3.81 -6.985) (angle -90) (layer B.Fab) (width 0.12))
  (fp_arc (start 0 0) (end 0 -2.54) (angle -75.96375653) (layer B.Fab) (width 0.12))
  (fp_line (start -6.35 -0.635) (end -2.54 -0.635) (layer B.Fab) (width 0.12))
  (fp_line (start -6.35 -0.635) (end -6.35 -4.445) (layer B.Fab) (width 0.12))
  (fp_line (start -3.81 -6.985) (end 5.08 -6.985) (layer B.Fab) (width 0.12))
  (fp_line (start 5.08 -6.985) (end 5.08 -2.54) (layer B.Fab) (width 0.12))
  (fp_line (start 5.08 -2.54) (end 0 -2.54) (layer B.Fab) (width 0.12))
  (fp_line (start 5.08 -6.35) (end 7.62 -6.35) (layer B.Fab) (width 0.12))
  (fp_line (start 7.62 -6.35) (end 7.62 -3.81) (layer B.Fab) (width 0.12))
  (fp_line (start 7.62 -3.81) (end 5.08 -3.81) (layer B.Fab) (width 0.12))
  (fp_line (start -6.35 -1.27) (end -8.89 -1.27) (layer B.Fab) (width 0.12))
  (fp_line (start -8.89 -1.27) (end -8.89 -3.81) (layer B.Fab) (width 0.12))
  (fp_line (start -8.89 -3.81) (end -6.35 -3.81) (layer B.Fab) (width 0.12))
  (fp_text user %R (at -0.635 -4.445) (layer B.Fab)
    (effects (font (size 1 1) (thickness 0.15)) (justify mirror))
  )
  (fp_text user %V (at -0.635 0.635) (layer B.Fab)
    (effects (font (size 1 1) (thickness 0.15)) (justify mirror))
  )
  (pad 1 smd rect (at 6.29 -5.08) (size 2.55 2.5) (layers B.Cu B.Paste B.Mask))
  (pad \"\" np_thru_hole circle (at 2.54 -5.08) (size 3 3) (drill 3) (layers *.Cu *.Mask))
  (pad \"\" np_thru_hole circle (at -3.81 -2.54) (size 3 3) (drill 3) (layers *.Cu *.Mask))
  (pad \"\" np_thru_hole circle (at 0 0) (size 3.9878 3.9878) (drill 3.9878) (layers *.Cu *.Mask))
  (pad \"\" np_thru_hole circle (at 5.08 0) (size 1.7018 1.7018) (drill 1.7018) (layers *.Cu *.Mask))
  (pad \"\" np_thru_hole circle (at -5.08 0) (size 1.7018 1.7018) (drill 1.7018) (layers *.Cu *.Mask))
  (pad 2 smd rect (at -7.56 -2.54) (size 2.55 2.5) (layers B.Cu B.Paste B.Mask))
)
"
  )



(defn kicad->edn
  [kstr]
  (clojure.edn/read-string
   (-> kstr
       (string/replace #"([0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12})" "#uuid \"$1\"")
       (string/replace #"tedit ([0-9a-f]+)" "tedit 0x$1"))))

(def kicadedn
  (kicad->edn kicadstr-kailhsocket))

(defn kicad->map
  [attrs]
  (into {}
        (for [[a & body] attrs]
          [a (vec body)])))

(defn kicad->pads
  [idbase body]
  (->> body
       (keep
        (fn [[f & args]]
          (case f
            pad (let [[padname padtype padshape & attrs] args
                      [[_at x y] [_size sx sy] [_drill drill]] attrs
                      halfw (* 0.5 sx)
                      halfh (* 0.5 sy)
                      prop (kicad->map attrs)]
                  (when (not-empty (str padname))
                    {:id (str idbase "." padname) 
                     :x (- x halfw)
                     :y (- y halfh)
                     :width sx
                     :height sy}))
            nil)))
       (vec)))

#_(defn kicad->elk
  [props [_footprint fpname & body]]
  (let [pads (kicad->pads (:id props) body)
        xmin (apply min (map :x pads))
        xmax (apply max (map #(+ (:x %) (:width %)) pads))
        ymin  (apply min (map :y pads))
        ymax  (apply max (map #(+ (:y %) (:height %)) pads))]
    (merge props
     {:properties {:algorithm "org.eclipse.elk.fixed"
                   
                   #_ #_:org.eclipse.elk.noLayout true}
      :width 100 #_(- xmax xmin)
      :height (- ymax ymin)
      :children (for [p pads]
                  (-> p
                      (update :x - xmin)
                      (update :y - ymin)))})))

(defn kicad->elk
  [props [_footprint fpname & body]]
  (let [pads (kicad->pads (:id props) body)
        xmin (apply min (map :x pads))
        xmax (apply max (map #(+ (:x %) (:width %)) pads))
        ymin  (apply min (map :y pads))
        ymax  (apply max (map #(+ (:y %) (:height %)) pads))]
    (merge props
           {:properties {:algorithm "org.eclipse.elk.fixed"
                         :portConstraints "FIXED_POS"}
            :width (- xmax xmin)
            :height (- ymax ymin)
            :ports (for [p pads]
                     (do
                       (let [rp (-> p
                                    (update :x - xmin)
                                    (update :y - ymin))]
                         (println "Port" rp)
                         rp
                         )))}))) 

(rum/defc kicadsvg
  [svgref [_footprint fpname & body]]
  [:div {}
   (str fpname) 
   [:svg
    {:ref svgref
     :xmlns "http://www.w3.org/2000/svg" 
     :viewBox (str "-8 -8 16 16")
     :style   {:border "1px solid aliceblue"}}
    [:g {:id (str fpname)}
     (for [[f & args] body]
       (case f
         fp_line (let [[[_start sx sy] [_end ex ey] [_layer layer] [_width w]] args]
                   [:line {:x1 sx :y1 sy :x2 ex :y2 ey
                           :data-layer layer
                           :stroke (case layer
                                     "F.SilkS" "#fff"
                                     "F.CrtYd" "cadetblue"
                                     "F.Fab" "#ae81ff"
                                     "tomato")
                           :stroke-width w}])
         fp_text (let [[vu text [_at x y] [_layer layer]
                        [_effects [_font [_size sx sy] [_thickness thk]]]] args]
                   [:text
                    {:x x :y y
                     :stroke "#fff"
                     :stroke-width 0.1
                     :fill "none"
                     :font-size sx
                     :text-anchor "middle"}
                    (str text)])
         fp_arc (let [[[_start sx sy] [_end ex ey] [_angle angle] [_layer layer] [_width w]] args
                      r (js/Math.hypot (- ex sx) (- ey sy))
                      radians (* angle js/Math.PI (/ 1 180.0))
                      dx (* r (js/Math.sin radians))
                      dy (* -1 r (js/Math.cos radians))]
                  (println "Angle " angle
                           "Sine" (js/Math.sin radians)
                           "Cosine " (js/Math.cos radians))
                  [:g {:data-layer layer}
                   [:circle {:stroke-width w :stroke "green" :cx sx :cy sy :r w}]
                   [:circle {:stroke-width w :stroke "blue" :cx ex :cy ey :r w}]
                 
                   [:circle {:stroke-width w :stroke "aliceblue"
                             :cx (+ sx (* r (js/Math.sin radians)))
                             :cy (- sy (* r (js/Math.cos radians)))
                             :r w}]]
                  [:path
                   {:stroke-width w
                    :stroke "yellow"
                    :fill "none"
                    :d
                    (gstring/format "M %f %f A %f %f 0 0 1 %f %f"
                                    (+ sx dx)
                                    (+ sy dy)
                                    r r ex ey)}])
         pad (let [[padname padtype padshape & attrs] args
                   [[_at x y] [_size sx sy] [_drill drill]] attrs
                   halfw (* 0.5 sx)
                   halfh (* 0.5 sy)
                   prop (kicad->map attrs)]
               (case padshape
                 circle [:g [:circle {:cx x :cy y :r halfw
                                      :fill "#fff"
                                      :opacity "0.2"
                                      :stroke "none"}]
                         [:text
                          {:x x :y (+ y (* 0.5 halfh))
                           :fill "none"
                           :stroke-width 0.1
                           :stroke "blue"
                           :font-size halfh
                           :text-anchor "middle"}
                          (str padname)]]
                 oval [:ellipse {:cx x :cy y :rx halfw :ry halfh
                                 :fill "#fff"
                                 :opacity "0.2"
                                 :stroke "none"}]
               
                 (rect roundrect)
                 (let [minor (min halfh halfw)
                       [rratio] (prop 'roundrect_rratio)
                       rxy (* minor
                              (or rratio 0))]
                   [:g
                    [:rect {:x (- x halfw)
                            :y (- y halfh)
                            :width sx
                            :height sy
                            :rx rxy
                            :ry rxy
                            :fill "#fff"
                            :opacity "0.2"
                            :stroke "none"}]
                    [:text
                     {:x x :y (+ y (* 0.5 minor))
                      :fill "none"
                      :stroke "#fff"
                      :stroke-width 0.1
                      :font-size minor
                      :text-anchor "middle"}
                     (str padname)]])
                 nil))
       
         (do (println "Default " f) nil)))]]])

(rum/defc elk-node-svg
  [{:keys [id width height x y ports children labels]}]
  [:g {:transform (str "translate(" x "," y ")")}
   [:rect {:key (str "r" id)
           :id (str "r" id)
           :width width :height height
           :stroke "#fff"}]
   (for [lbl labels]
     [:text {:x (:x lbl)
             :y (:y lbl)
             :stroke "#fff"
             :fill "none"
             :font-size 0.8}
      (:text lbl)])
   (for [p ports]
     [:g
      [:rect {:id (str "p" (:id p))
              :key (str "p" (:id p))
              :x (:x p) :y (:y p) :width (:width p) :height (:height p)
              :stroke "tomato"}]
      (when-some [ls (seq (:labels p))]
        (for [lbl ls]
          [:text {:x (+ x (:x p) (:x lbl))
                  :y (+ y (:y p) (:y lbl))
                  :stroke "#fff"
                  :fill "none"
                  :font-size 0.8}
           (:text lbl)]))])
   (for [c children]
     (elk-node-svg c))])

(rum/defc elksvg
  [r]
  [:g {:stroke-width 0.1}
   (for [c (:children r)]
     (elk-node-svg c))
   (for [{:keys [sources targets sections] :as e} (:edges r)
         {:keys [startPoint endPoint bendPoints] :as s} sections]
     [:path {:stroke "green"
             ;; :stroke-width 1 
             :fill "none"
             :d (str "M" (:x startPoint) " " (:y startPoint)
                     ;; "L " (:x endPoint) " " (:y endPoint)
                     (apply str
                            (for [{:keys [x y]} (conj (or bendPoints []) endPoint)]
                              (str " L " x " " y))))}])])

#_(rum/defcs fakeroot < (rum/local nil ::result)  
  [{::keys [result]}]
  (let [elk (js/ELK.)
        chip (fn [ch]
               (kicad->elk ch kicadedn)) 
        N 20
        graph (clj->js
               {:id "root"
                :properties {:algorithm
                             #_"org.eclipse.elk.fixed"
                             "layered"}
                :layoutOptions {:org.eclipse.elk.spacing.edgeNode 1
                                :org.eclipse.elk.spacing.edgeEdge 1
                                :org.eclipse.elk.spacing.nodeNode 1
                                
                                ;; :org.eclipse.elk.padding  1
                                ;; :org.eclipse.elk.spacing.portsSurrounding 1

                                ;; :org.eclipse.elk.layered.wrapping.additionalEdgeSpacing 1

                                :org.eclipse.elk.layered.spacing.edgeEdgeBetweenLayers 1
                                ;; :org.eclipse.elk.layered.spacing.nodeNodeBetweenLayers 1
                                :org.eclipse.elk.layered.spacing.baseValue 1
                                
                                ;; :org.eclipse.elk.separateConnectedComponents false
                                ;; :org.eclipse.elk.spacing.componentComponent 1
                                ;; :org.eclipse.elk.layered.compaction.connectedComponents true
                                
                                ;; :org.eclipse.elk.edgeRouting "ORTHOGONAL"
                                ;; :org.eclipse.elk.padding 1

                                
                                ;; :org.eclipse.elk.layered.compaction.postCompaction.strategy "EDGE_LENGTH"
                                }
                :children (concat
                           (for [i (range N)]
                             (chip {:id (str "k" (inc i))}))
                           #_(for [i ["a" "b" "c" "d" "e" "f"]]
                               {:id (str "test." i)
                                :width 1
                                :height 1})
                           [(kicad->elk {:id "h"} (kicad->edn kicadstr))]
                           #_[{:id "gnd"
                               :width 1
                               :height 1}])
                :edges (concat
                        (for [i (range N)]
                          {:id (str "e." (inc i))
                           :sources [(str "k" (inc i) ".1")]
                           :targets [(str "h." (inc i))]}))})
        r (some-> @result
                  (js->clj :keywordize-keys true))]
    (when-not r
      (.then (.layout elk graph)
             (fn [elkr]
               (reset! result elkr))))
    (let [svgref (rum/create-ref)]
      [:div  {:style {:display :grid
                      :grid-template-columns "repeat(5, 1fr)"}}
       [:div {:style {:width "500px" :height "500px"}}
        [:button {:on-click (fn []
                              (let [svgxml (.-outerHTML (rum/deref svgref))
                                    svgopt (svgo/optimize svgxml
                                                          #js {:multipass true
                                                               :plugins #js [
                                                                             #js {:name "sortAttrs"
                                                                                  :params #js {:xmlnsOrder "alphabetical"}}]
                                                               :js2svg #js {:pretty true
                                                                            :indent "  "}})]
                                (js/console.log svgopt)
                                (println svgxml #_(.-data svgopt) )))}
         "Go"]
        (kicadsvg svgref kicadedn)]
      
       (when r
         [:svg
          {:width "2000px"
           :height "900px"
           :viewBox #_(gstring/format
                       "%f %f %f %f"
                       -10 -10 100 100)
           (gstring/format
            "%f %f %f %f"
            (:x r)
            (:y r)
            (:width r)
            (:height r))
           :style   {:border "1px solid aliceblue"}}
          (elksvg r)])
     
       [:code 
        [:pre (js/JSON.stringify graph nil 2)]]
       #_[:code
          [:pre (with-out-str (cljs.pprint/pprint r))]]
       ])))

(rum/defcs elkroot < (rum/local nil ::result)  
  [{::keys [result]} elkgraph]
  (let [elk (js/ELK.)
        r (some-> @result
                  (js->clj :keywordize-keys true))]
    (when-not r
      (.then (.layout elk elkgraph)
             (fn [elkr]
               (reset! result elkr))))
    (let [svgref (rum/create-ref)]
      [:div  {:style {:display :grid
                      :grid-template-columns "repeat(5, 1fr)"}}
       (when r
         [:svg
          {:width "2000px"
           :height "900px"
           :viewBox (gstring/format "%f %f %f %f" (:x r) (:y r) (:width r) (:height r))
           :style   {:border "1px solid aliceblue"}}
          (elksvg r)])])))

(defn aabb
  [pad pts]
  (let [xmin (- (apply min (map first pts)) pad)
        xmax (+ (apply max (map first pts)) pad)
        ymin (- (apply min (map second pts)) pad)
        ymax (+ (apply max (map second pts)) pad)]
    [xmin ymin (- xmax xmin) (- ymax ymin)]))

(defn pts->d
  [[[sx sy] & pts]]
  (str "M" sx " " sy
       (apply str
              (for [[x y] pts]
                (str "L " x " " y)))))

(rum/defc gridsvg
  [grid-style elems]
  (let [gridref (rum/create-ref)]
    [:div {:ref gridref
           :style (merge {:display :grid} grid-style)}
    (rum/use-layout-effect! (fn []
                              (let [el (rum/deref gridref)
                                    gc (.getBoundingClientRect el)]
                                (loop [n (.-firstChild el)
                                       acc []]
                                  (if-not n
                                    (prn
                                     (into [:svg {:viewBox (gstring/format "%f %f %f %f"
                                                                           (.-x gc) (.-y gc)
                                                                           (.-width gc) (.-height gc))}]
                                           acc))
                                    (let [c (.getBoundingClientRect n)]
                                      (recur (.-nextElementSibling n)
                                             (conj acc
                                                   [:rect {:x (.-x c)
                                                           :y (.-y c)
                                                           :width (.-width c)
                                                           :height (.-height c)}]))))))
                              nil))
    (for [e elems]
      e)]))

(rum/defc padstack-svg
  [args [[_padstk psid [_shape [shtype shlayer & shpts]] :as stk]]]
  (let [rot? (and (vector? (first args)) (= 'rotate (ffirst args)))
        [pin-id x y] (if-not rot?
                       args
                       (next args))
        deg (when rot? (second (first args)))]
    [:g {:id psid
         :stroke-width 44
         :stroke "tomato"
         :transform (str
                     (str " translate(" x " " y ")")
                     (when rot? (str " rotate(" deg ")")))}
     (case shtype
       rect (let [[xa ya xb yb] shpts
                  xmin (min xa xb)
                  xmax (max xa xb)
                  ymin (min ya yb)
                  ymax (max ya yb )]
              [:rect {:x xmin :y ymin :width (- xmax xmin) :height (- ymax ymin)}])
       polygon [:path {:d (str (pts->d (partition-all 2 (next shpts)))
                               )}]
       circle [:circle {:r (*  0.5 (first shpts))}]
       path [:path {:stroke-width (first shpts)
                    :stroke-linecap "round"
                    :stroke "yellow"
                    :opacity "65%"
                    :d (str (pts->d (partition-all 2 (next shpts))))}]
       (do (prn 'Shty shtype) 
           [:circle {:stroke-width 44
                     :stroke "cyan"
                     :r (* 0.5 (first shpts))}]))]))

(defn padstack-pts
  [[[_padstk _id [_shape [shtype shlayer & shpts]] :as stk]]]
  [shtype shpts]
  (case shtype
    (rect polygon) (partition-all 2 shpts)
    circle (let [r (first shpts)]
             [[(- r) (- r)]
              [r r]])
    path  (let [[thk & pts] (next shpts)]
            (apply concat
                   (for [[x y] (partition-all 2 pts)]
                     [[(- x thk) (- y thk)]
                      [(+ x thk) (+ y thk)]])))))

;; viewbox 74000 -121000 52000 51000
;; 

(rum/defc dsn-library
  [library]
  (let [id->padstack (into (sorted-map)
                           (group-by second (get library 'padstack)))
        cellsize "100px"
        gtc (gstring/format "repeat(auto-fill, minmax(%s, 1fr))" cellsize) ]
    [:div {:style {:margin-left "1ex"}}
     [:h2 "Padstack"]
     [:div {:style {:width "1400px"
                    :display :grid
                    :grid-gap "1em"
                    :grid-template-columns gtc}}
      (for [[id ps] id->padstack ]
        (let [[bx by bw bh] (aabb 1000 (padstack-pts ps))]
          [:div {}
           [:svg {:viewBox (gstring/format "%f %f %f %f" bx by bw bh)
                  :style {:outline "1px solid #111"}
                  :width cellsize
                  :height cellsize}
            (padstack-svg [0 0] ps)]
           [:div {:style {:overflow-wrap :anywhere :width "100%"}} (str id)]]))]
     
     [:h2 "Image"]
     [:div {:style {:display :grid
                    :grid-template-columns "repeat(auto-fill, minmax(200px, 1fr) ) "
                    :grid-gap "1em"
                    :width "1400px"}}
      (for [[_image imgid & body] (sort-by second (get library 'image))]
        (let [img (group-by first body)
              outlines (get img 'outline)
              outline-pts (partition-all 2
                                         (for [[_outline & body] (get img 'outline)
                                               [_path _pathtype thk & coords] body
                                               c coords]
                                           c))
              [obx oby obw obh :as bbox] (aabb 1000 outline-pts)]
          [:div {}
           [:svg {:viewBox (gstring/format "%f %f %f %f" obx oby obw obh)
                  :width "100%"
                  :height "200px"}
            [:g {:id imgid
                 :fill "none"
                 :stroke "#fff"}
             (for [[_outline & body] (get img 'outline)
                   [_path _pathtype thk & coords] body]
               [:path {:stroke-width thk
                       :d (pts->d (partition-all 2 coords))}])
             (for [[_pin padstack & args] (get img 'pin)
                   :let [rot? (and (vector? (first args)) (= 'rotate (ffirst args)))
                         [pin-id x y] (if-not rot?
                                        args
                                        (next args))
                         deg (when rot? (second (first args)))]]
               [:g {:stroke "#fff"
                    :stroke-width 444
                    :transform (str
                                "translate(" x " " y ")"
                                (when rot? (str " rotate(" deg ")")))}
                [:use {:href (str "#" padstack)}]])
             [:g {:stroke "tomato"
                  :stroke-width 100}
              (for [[_ko koname & args] (get img 'keepout)
                    [circle layer d x y] args
                    :when (= circle 'circle)]
                [:circle {:cx x :cy y :r (* 0.5 d)}]
                )]]]
           [:div {:style {:overflow-wrap :anywhere :width "100%"}} (str imgid)]]))]]))

(rum/defc dsn-structure
  [structure]
  (let [cellsize "200px"
        gtc (gstring/format "repeat(auto-fill, minmax(%s, 1fr))" cellsize)
        boundary (group-by first (mapcat next (get structure 'boundary)))
        bpath (group-by second (get boundary 'path ))
        [[_path _pcb _unk & coords ]] (get bpath "pcb")
        boundary-pts (partition-all 2 coords)
        [bx by bw bh] (aabb 1000 boundary-pts)]
    
    [:div {:style {:margin-left "1ex"}}
     [:h2 "Structure"]
     [:div {}
      (for [[_layer layer & args] (structure 'layer)
            :let [arg (group-by first args)
                  [[_type lty]] (arg 'type)
                  [[_prop & pbody]] (arg  'property)
                  props (into {} pbody)]]
        [:div {}
         [:h3 {} layer]
         #_[:div {} "Index: " [:span {} (props 'index)]]
         #_[:div {} "Type: " [:span {} lty]]
         (for [[f a] args]
           [:code [:pre (str f " " (pr-str a ))]])])]
     [:h2 "Boundary"]
     #_[:pre
        (with-out-str (cljs.pprint/pprint structure) )]
     [:div {}
      [:svg {:viewBox (gstring/format "%f %f %f %f" bx by bw bh)
             :width cellsize
             :height cellsize}
       [:g {:fill "none" :stroke "#fff"}
        [:path {:stroke-width 1000
                :d (pts->d boundary-pts)}]]]]
     [:h2 "Plane"]
     [:div {:style {:display :grid
                    :grid-template-columns gtc
                    :grid-gap "1em"}}
      (for [[_plane plane-name [ty layer _unk & coords]] (get structure 'plane)]
        (let [points (partition-all 2 coords)
              [px py pw ph] (aabb 1000 points)
              markup (case ty
                       'polygon
                       [:path {:fill "#fff"
                               :opacity "0.2"
                               :d (pts->d points)}])]
          [:div {}
           [:h4 {} [:span {} plane-name]
            " - "
            [:span {} layer]]
           [:svg {:viewBox (gstring/format "%f %f %f %f" px py pw ph)
                  :width cellsize
                  :height cellsize}
            [:g {:fill "none"
                 :stroke "#fff"}
             markup]]
           [:svg {:viewBox (gstring/format "%f %f %f %f" bx by bw bh)
                  :width cellsize
                  :height cellsize}
            [:path {:stroke-width 1000
                    :stroke "#fff"
                    :d (pts->d boundary-pts)}]
            markup]]))]]))

(rum/defc dsn-placement
  [placement]
  (let [cellsize "200px"
        gtc (gstring/format "repeat(auto-fill, minmax(%s, 1fr))" cellsize)]
    
    ))



(rum/defcs dsnroot < (rum/local nil ::result)  
  [{::keys [result]}]
  (let [[[_pcb dsnfile & body]] (dsn/dsnparse dsn/dsnstr)
        toplevel (group-by first body)
        structure (group-by first (mapcat next (get toplevel 'structure)))
        boundary (group-by first (mapcat next (get structure 'boundary)))
        bpath (group-by second (get boundary 'path ))
        
        [[_path _pcb _unk & coords ]] (get bpath "pcb")
        boundary-pts (partition-all 2 coords)
        [bx by bw bh] (aabb 0 boundary-pts)
        
        library (group-by first (mapcat next (get toplevel 'library )))
        id->image (group-by second (get library 'image))
        [_placement & plcbody] (toplevel 'placement)]
    [:div {:style {:margin-left "1ex"}}
     [:span
      {}
      (pr-str
       (let [[xmin ymin w h] [74000 -121000 (* 9 52000) 101000]]
         [xmin ymin
          (+ w xmin) ymin
          (+ w xmin) (+ ymin h)
          xmin (+ ymin h)]))]
     [:div
      [:div {:style {:width "100px"}} (kicadsvg nil kicadedn)]
      [:svg {:viewBox (gstring/format "%f %f %f %f" bx by bw bh)
             :width "2000px"
             :height "1000px"
             :style {:border "1px solid cadetblue"}}
       [:foreignObject {:x bx :y by :width bw :height bh}
        (let [major 1000
              fs (/ major 2.5)
              nfh (* -3 fs)
              pad (* 0.1 nfh)
              rh (* 5 fs)]
         [:div
          {:style {:width "100%"
                   :height (str rh "px") 
                   :position "relative"
                   :top "46900px"
                   :left "219px"
                   :border "100px solid blue"}}
          [:svg {:viewBox (gstring/format "%f %f %f %f" bx by bw bh)}
           [:g {:stroke "#fff"
                :fill "none"}
            (let [h 10000
                  d major]
              (for [i (range (/ bw d))]
                [:g {:stroke-width (* 0.03 major)}
                 [:path {:d (str "M" (+ bx (* d i)) " " (+ by rh nfh)
                                 " L" (+ bx (* d i))  " " (+ by rh))} ]
                 [:text
                  {:x (+ bx (* d i))  :y (+ by rh nfh pad)
                   :fill "#fff"
                   :stroke "none"
                   :font-size fs
                   :letter-spacing -1
                   :text-anchor (if-not (zero? i) "middle" "left")}
                  (str i (when (zero? i) "mm"))]]))
            (let [sub 5
                  d (/ major sub)]
              (for [i (range (/ bw d))
                    :let [s (rem i sub)]
                    :when (not= 0 s)]
                [:g {:stroke-width (* 0.01 major)}
                 [:path {:d (str "M" (+ bx (* d i)) " " (+ by rh)
                                 " L" (+ bx (* d i))  " " (+ by (- rh fs fs)))}]]))]]])
        #_[:div
         {:style {:width (str bw "px")
                  :height (str bh "px")
                  :position :absolute
                  :display :grid
                  :grid-template-columns "repeat(8, 20000px)"
                  :grid-template-rows "repeat(auto-fill, 20000px)"
                  :grid-gap "1em"
                  :opacity "90%"
                  :font-size "1000px"
                  :top "10000px"
                  :left "50000px"}}
         (for [i (range 24)]
           (let [[sx sy sw sh] [-10000 -9000 20000 20000]]
             [:div {:key (str "dge" i)
                    :style {:width (str sw "px")
                            :height (str sh "px")
                            :border-radius "1ex"}}
              [:svg {:viewBox (gstring/format "%f %f %f %f" sx sy sw sh)}
               [:use {:stroke-width 1
                      :transform "scale(1000,1000)"
                      :font-size 1
                      :href "#Kailh_socket_MX"}]]]))]]
       [:path {:stroke "green"
               :fill "none"
               :stroke-width 100
               :d (pts->d boundary-pts)}]
       ;;            74000 -121000 178000 -121000 178000 -70000 74000 -70000
       [:g {:transform (str "translate(" 0 "," (+ by by bh) ")"
                            " scale(1,-1)")
            :fill "none"
            :stroke "#fff"}
        ;; placements
        (for [[_placement & comps] (get toplevel 'placement)
              [_comp cclass & places] comps
              [_place cname x y layer rot [_pn pn]] places]
          [:use.comp {:transform (str
                                  "translate(" x " " y ")"
                                  " rotate(" rot ")")
                      :id cname
                      :href (str "#" cclass)}])
        
        ;; wires
        [:g.wire {:stroke "#ae81ff"
                  :stroke-linecap "round"}
         (for [[_wiring & wires] (toplevel 'wiring)
               [wire & args] wires
               :when (= 'wire wire)
               :let [[[_path layer thk & pts] & body] args
                     props (into {} body)]]
           
           [:path {:stroke-width thk
                   :d (pts->d (partition-all 2 pts))
                   :data-net (props 'net)}])]
        ;; vias
        (for [[_wiring & wires] (toplevel 'wiring)
              [via & args] wires
              :when (= 'via via)
              :let [[vianame x y & props] args]]
          [:use.via {:transform (str "translate(" x " " y ")")
                     :href (str "#" vianame)}])]]]
     
     (dsn-structure structure)
     (dsn-library library)
     #_(gridsvg
        {:width "800px"
         :grid-template-columns "repeat(auto-fill, minmax(4em, 1fr) ) "
         :grid-gap "1em"}
        (for [i (range 99)]
          [:div {:key (str "ge" i)
                 :style {:background-color "tomato"
                         :border-radius "1ex"}
                 }
           (str i)])
        )
     
     #_[:div {:style {:display :grid
                      :grid-template-columns "repeat(auto-fill, minmax(4em, 1fr) ) "
                      :grid-gap "1em"
                      :width "800px"}}
        (for [i (range 99)]
          [:div {:key (str "ge" i)
                 :style {:background-color "tomato"
                         :border-radius "1ex"}
                 }
           (str i)])]
     
     #_[:svg {:viewBox (gstring/format "%f %f %f %f" bx by bw bh)
              :width "1000px"
              :height "1000px"
              :style {:border "1px solid cadetblue"}}
        [:path {:stroke "#fff"
                :fill "none"
                :stroke-width 999
                :d (pts->d boundary-pts)}]
        (for [[_plane plane-name [ty layer _unk & coords]] (get structure 'plane)]
          (case ty
            'polygon [:path {:fill "#fff"
                             :opacity "0.2"
                             :data-layer layer
                             :data-plane plane-name
                             :d (pts->d (partition-all 2 coords))}]
            nil))

        (for [[_placement & comps] (get toplevel 'placement)
              [_comp cclass & places] comps
              [_place cname x y layer unk & pn] places]
          (do (println "PLacetment cname" cclass)
              #_(println (get id->image cclass))
              [:g {:transform (str "translate(" x "," y ")")
                   :fill "none"
                   :stroke "#fff"}
             
               ]))]
     

     
     #_[:code [:pre
               (with-out-str
                 (cljs.pprint/pprint
                  (get structure 'plane)
                  ))
               ]]
     #_[:code [:pre (with-out-str
                      (cljs.pprint/pprint
                       body)) ]]]
    
    #_[:code [:pre (pr-str (clojure.edn/read-string dsn)) ]]))

(defn dsn->elk
  [pcb]
  (let [[_pcb dsnfile & body] pcb
        toplevel (group-by first body)
        fontsize 4
        cclass->pins  (into {}
                            (for [[_library & comps] (get toplevel 'library)
                                  [_image cclass & body] comps]
                              [cclass
                               (for [[pin psid & args] body
                                     :when (= pin 'pin)
                                     :let [rot? (and (vector? (first args)) (= 'rotate (ffirst args)))
                                           [id x y] (if-not rot?
                                                      args
                                                      (next args))]]
                                 (do
                                   (println "Pin" id " of " cclass)
                                   id))]))
        bignet-dummy-nodes  (for [[_network & nets] (get toplevel 'network)
                                  [net netname & netbody] nets
                                  :when (= net 'net)
                                  [_pins & pins] netbody
                                  :when (< 2 (count pins))]
                              {:id (str "bignet-" netname)
                               :width 2
                               :height 2
                               :labels [{:text netname}]})]
    (clj->js
     {:id "root"
      :properties {:algorithm "force"}
      
      :layoutOptions {:org.eclipse.elk.spacing.edgeNode 1
                      :org.eclipse.elk.spacing.edgeEdge 1
                      :org.eclipse.elk.spacing.nodeNode 1
                      :org.eclipse.elk.layered.spacing.edgeEdgeBetweenLayers 1
                      :org.eclipse.elk.layered.spacing.baseValue 1
                      :org.eclipse.elk.layered.compaction.postCompaction.strategy "EDGE_LENGTH"

                      }
      :children (concat
                 bignet-dummy-nodes
                 (for [[_placement & comps] (get toplevel 'placement)
                       [_comp cclass & places] comps
                       [_place cname x y layer unk & pn] places]
                   {:id cname
                    ;; :x x
                    ;; :y y
                    :width 4
                    :height 4
                    :layoutOptions {:org.eclipse.elk.portAlignment.default "DISTRIBUTED"}
                    :labels [{:text cname
                              :width (* fontsize (count cname ) )
                              :height fontsize }]
                    :ports (for [pid (cclass->pins cclass)]
                             (do
                               #_(prn (str cname "-" pid))
                               {:id (str cname "-" pid)
                                :width 1
                                :height 1}))}))
      :edges (concat
              (for [[_network & nets] (get toplevel 'network)
                    [net netname & netbody] nets
                    :when (= net 'net)
                    [_pins & pins] netbody
                    :when (= 2 (count pins))]
                {:id netname
                 :sources [(first pins)]
                 :targets [(second pins)]})
              (for [[_network & nets] (get toplevel 'network)
                    [net netname & netbody] nets
                    :when (= net 'net)
                    [_pins & pins] netbody
                    :when (< 2 (count pins))
                    p pins
                    :let [dummy-id (str "bignet-" netname)]]
                {:id (str dummy-id "-connector")
                 :sources [p]
                 :targets [dummy-id]}
                ))})))

(defn ^:dev/after-load init
  []
  #_(some-> the-singleton-db meta :listeners (reset! {}))
  (js/document.addEventListener "scroll" set-scroll-user true)
  (let [{:keys [conn bus] :as app} (setup-app the-singleton-db)
        el (.getElementById js/document "root")]
    (reset! the-app app)
    
    #_(when-let [req-title (some-> js/window.location.search
                                   (js/URLSearchParams.)
                                   (.get "title"))]
        (set! js/document.title req-title))
    #_(rum/unmount el)
    (println "Created new app and reset kbdb.  Mount root...")
    (-> #_(cr/root app code/form)
        #_(elkroot (dsn->elk (first (dsn/dsnparse dsn/dsnstr))))
        (dsnroot)
        (rum/mount el))
    (println "Done")))


(defn ^:export become
  [db]
  (-> (fn []
        (rum/unmount (.getElementById js/document "root"))
        (stop)
        (reset! the-singleton-db db)
        (init))
      (js/setTimeout 0))
  "Goodbye")


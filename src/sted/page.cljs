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
   
   [shadow.resource :as rc]
   
   ["svgo/dist/svgo.browser" :as svgo]
   ["svgo/plugins/_applyTransforms" :as svgo-applytransforms]
   ["svgo/plugins/_transforms" :as svgo-transforms]
   ["svgo/plugins/_path" :as svgo-path]
   ["svgo/lib/path" :as svgo-lib-path]
   ["greiner-hormann" :as gh]
   ["martinez-polygon-clipping" :as mpc]
   [sted.eda.dsn :as dsn]
   [sted.eda.kicad-footprint :as kcfp]
   [sted.eda.kicad :as kcnext]
   [sted.eda.path-d :as path-d]
   [sted.eda.schema :as edaschema]
   ["jsts/org/locationtech/jts/io" :as jts-io]
   ["jsts/org/locationtech/jts/operation/buffer" :as jts-buf]
   
   ["spl.js" :default spl]
   
   
   
   )

  (:require-macros
   [cljs.core.async.macros :refer [go
                                   go-loop]]
   [sted.macros :as m]))

(def siquery
  (rc/inline "sted/eda/honker.sql")
  #_"
  select
  fp.name as footprint
  , fpp.name as pad
  , astext(makepoint(fpp.x_mm, fpp.y_mm)) as placexy
  , d.d_mm is not null as has_draw_d
  , kp.d_mm is not null as had_pad_d

  from kicad_footprint fp
  left join kicad_footprint_draw d on d.kicad_footprint_id = fp.id
  left join kicad_footprint_pad fpp on fpp.kicad_footprint_id = fp.id
  left join kicad_pad kp on kp.id = fpp.kicad_pad_id
  
  "
  
  #_"select 
  kicad_footprint_id
  , kf.name
  , group_concat(d_mm, ' ; ') as svg1
  , group_concat(d_mm, ' ; ') as svg3
  , assvg(extent(geomfromtext(wkt))) as svg2
  , extent(geomfromtext(wkt)) as viewbox3
  --, (wkt) as viewbox
from kicad_footprint_draw kfd
left join kicad_footprint kf on kfd.kicad_footprint_id = kf.id
where d_mm is not null
group by 1,2
limit 1000
"
  #_"select * from kicad_pad"
  #_"with data as (select layer, collect(geomfromtext(wkt)) as geom from kicad_footprint_draw group by layer) 
, q as (
select layer, assvg(geom) as svg, geom as viewbox 
, assvg(collect(geom, st_buffer(geom, 0.5))) as svg2
, st_expand(geom, 10) as viewbox2
from data
)
, tq as(
select 
collect(st_translate(geom, 2*i.value*(mbrmaxx(geom) - mbrminx(geom)), 2*j.value*(mbrmaxy(geom) - mbrminy(geom)), 0)) as tiled
from generate_series(1,5) i
join generate_series(1,5) j
join data group by layer
)
select assvg(tiled) as svg, tiled as viewbox from tq"
  
  #_"with data as (select layer, collect(geomfromtext(wkt)) as geom from kicad_footprint_draw group by layer) select layer, assvg(geom) as svg, geom as viewbox from data"
  #_"select layer, assvg(collect(geomfromtext(wkt)))  as svg1, '-100 -100 200 200' as viewbox1 from kicad_footprint_draw group by layer"
  #_"select , wkt, assvg(geomfromtext(wkt)) from kicad_footprint_draw"
  #_"select json_group_array(json_object('d',d_mm,'stroke', case when layer = 'F.Cu' then 'red' else '#fff' end )) as svg_agg_dmm from kicad_footprint_draw")


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
  (rc/inline "sted/eda/qfn.kicad_mod"))

(defn aabb
  [pad pts]
  (let [xmin (- (apply min (map first pts)) pad)
        xmax (+ (apply max (map first pts)) pad)
        ymin (- (apply min (map second pts)) pad)
        ymax (+ (apply max (map second pts)) pad)]
    [xmin ymin (- xmax xmin) (- ymax ymin)]))

(defn pts->d
  [[[sx sy] & pts]]
  (str "M " sx " " sy
       (apply str
              (for [[x y] pts]
                (str " L " x "," y)))))

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
                   [_path pathtype thk & coords] body]
               [:path {:class pathtype
                       :stroke-width thk
                       :d (pts->d (partition-all 2 coords))}])
             (for [[_pin padstack & args] (get img 'pin)
                   :let [rot? (and (vector? (first args)) (= 'rotate (ffirst args)))
                         [pin-id x y] (if-not rot?
                                        args
                                        (next args))
                         deg (when rot? (second (first args)))]]
               [:use {:data-pin pin-id
                      :href (str "#" padstack)
                      :transform (str "translate(" x " " y ")"
                                      (when rot? (str " rotate(" deg ")")))}])
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
        [:div {:key layer}
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
     [:div {:style {:display :flex
                    :grid-template-columns gtc
                    :grid-gap "1em"}}
      (for [[_plane plane-name [ty layer _unk & coords] & windows]
            (get structure 'plane)]
        (let [points (partition-all 2 coords)
              [px py pw ph] (aabb 1000 points)
              markup [:g (case ty
                           'polygon
                           [:path {:fill "#fff"
                                   :opacity "0.2"
                                   :d (pts->d points)}]
                           nil)
                      (for [[_window [ty layer aw & coords]] windows]
                        (case ty
                           'polygon
                           [:path {:stroke "#fff"
                                   :stroke-width 333
                                   :fill "none"
                                   :d (pts->d (partition-all 2 coords))}]))]]
          [:div {}
           [:h4 {} [:span {} plane-name]
            " - "
            [:span {} layer]]
           #_[:div [:code [:pre (pr-str windows)]]]
           #_[:svg {:viewBox (gstring/format "%f %f %f %f" px py pw ph)
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



#_(rum/defc hruler
  [bx by bw bh]
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
                            " L" (+ bx (* d i))  " " (+ by (- rh fs fs)))}]]))]]]))

(rum/defc hruler
  [major bw]
  (let [bx  0
        fs (/ major 2.5)
        nfh (* -3 fs)
        pad (* 0.1 nfh)
        rh (* 5 fs)
        by (- rh)
        maj-stroke (* 0.03 major)
        min-stroke (* 0.01 major)]
    [:svg {:viewBox (gstring/format "%f %f %f %f" bx by bw rh)
           :width "300mm"
           :height "20mm"
           :preserveAspectRatio "xMinYMin slice"}
     [:g {:stroke "#fff"
          :fill "none"}
      (let [d major]
        (for [i (range (/ bw d))]
          [:g {:stroke-width maj-stroke }
           [:path {:d (str "M" (+ bx (* d i)) " " (+ by rh nfh)
                           " L" (+ bx (* d i))  " " (+ by rh))} ]
           [:text
            {:x (+ bx (* d i)
                   (if (zero? i)
                     0
                     (* -0.5 maj-stroke)))
             :y (+ by rh nfh pad)
             :fill "#fff"
             :stroke "none"
             :font-size fs
             ;; :letter-spacing -1.5
             :text-anchor (if-not (zero? i) "middle" "left")}
            (if (zero? i)
              "0mm"
              (str i))]]))
      (let [sub 5
            d (/ major sub)]
        (for [i (range (/ bw d))
              :let [s (rem i sub)]
              :when (not= 0 s)]
          [:g {:stroke-width min-stroke}
           [:path {:d (str "M" (+ bx (* d i)) " " (+ by rh)
                           " L" (+ bx (* d i))  " "
                           (+ by (- rh fs fs)))}]]))]]))

(rum/defc mmruler
  [height mm-div length]
  (let [rh height
        mindig (min 3 (count (str mm-div)))
        maxdig (count (str length))
        fs (* 0.75  (/ mm-div mindig))
        
        nfh (* -3 fs)
        pad (* 0.1 nfh)
        by (- rh)
        ndivs (js/Math.floor (/ length mm-div))
        maj-stroke (/ mm-div 50)
        min-stroke (* 0.4 maj-stroke)
        d mm-div
        sub 5
        ds (/ d sub)]
    [:svg {:viewBox (gstring/format "%f %f %f %f" 0 by length rh)
           :width (str length "mm")
           :height (str height "mm")
           :preserveAspectRatio "xMinYMin slice"}
     [:g {:stroke "#fff"}
      
      [:g.ruler-major {:fill "none" :stroke-width maj-stroke}
       (for [i (range ndivs)]
         [:path {:key i
                 :d (str "M" (* d i) ", " (+ by rh nfh)
                         " v" (- nfh))} ])]
      
      
      [:g.ruler-major {:fill "#fff" :stroke "none" :font-size fs}
       [:text {:x 0 :y (+ by rh nfh pad)} "0"]
       [:text {:x (+ (* 0.6 fs) )
               :y (+ by rh nfh pad)
               :font-size (* 0.8 fs )
               :text-anchor "left"} "mm"]
       
       (for [i (range 1 ndivs)]
         [:text {:key i
                 :x (+ (* d i)
                       (if (zero? i)
                         0
                         (* -0.5 maj-stroke)))
                 :y (+ by rh nfh pad)
                 :text-anchor (if-not (zero? i) "middle" "left")}
          (str (* i mm-div))])]
      
      [:g.ruler-minor {:stroke-width min-stroke}
       (for [i (range ndivs)
             j (range 1 sub)]
         [:path {:key (str i "j" j)
                 :d (str "M" (+ (* d i) (* ds j)) " " (+ by rh)
                         "v " (- (+ fs fs)))}])]]]))


(rum/defc polygondiffc
  []
  [:div
      (let [r->p (fn [x y w h]
                   [[x y]
                    [(+ x w) y]
                    [(+ x w) (+ y h)]
                    [x (+ y h)]
                    [x y]])
            apts (r->p 0 0 10 4)
            bpts (r->p 3 1 2 2)
            ]
        [:div
         [:svg {:viewBox "-10 -10 20 20"
                :width "400px"
                :height "400px"}
          [:g {:stroke "#fff"
               :stroke-width 0.1}
           [:path {:d (pts->d apts)}]
           [:path {:d (pts->d bpts)}]]]
         (let [ghr (mpc/diff (clj->js [apts])
                             (clj->js [bpts
                                       (r->p 6 1 1 1)]))
               gd (if (number? (-> ghr (nth 0) (nth 0)))
                    (pts->d ghr)
                    (apply str
                     (for [poly ghr
                           ring poly]
                       (pts->d ring))))]
           [:div {}
            "Diff"
            [:code [:pre (js/JSON.stringify ghr)]]
            [:code [:pre gd]]
            [:div {}
             [:svg {:viewBox "-10 -10 20 20"
                    :width "500px"
                    :height "300px"}
              [:g {:fill "#fff"}
               [:path {:d gd :fill-rule "evenodd"}]]]]])])])

#_(rum/defc input-field []
  (let [[value set-value!] (rum/use-state "")]
    [:input {:value value
             :on-change #(set-value! (.. % -target -value))}]))

#_(defn gaia->viewbox
  [^ArrayBuffer gaia]
  (let [dv (js/DataView. gaia)
        xmin (.getFloat64 dv 6 true)
        ymin (.getFloat64 dv 14 true)
        xmax (.getFloat64 dv 22 true)
        ymax (.getFloat64 dv 30 true)]
    (println "Viewbox"
             xmin ymin
             xmax ymax)
    [xmin ymin (- xmax xmin) (- ymax ymin)]))

(defn gaia->viewbox
  [^ArrayBuffer gaia]
  (let [dv (js/DataView. gaia)
        xmin (.getFloat64 dv 6 true)
        rymin (.getFloat64 dv 14 true)
        xmax (.getFloat64 dv 22 true)
        rymax (.getFloat64 dv 30 true)
        ;; ymin (min (- rymin) (- rymax))
        ;; ymax (max (- rymin) (- rymax))
        ]
    #_(println "Viewbox"
             xmin ymin
             xmax ymax)
    #_[xmin ymin (- xmax xmin) (- ymax ymin)]
    [xmin rymin (- xmax xmin) (- rymax rymin)]))

(defn classify-result-column
  [c]
  (if-some [[_ ident] (->> c (re-find #"(?i)^svg_agg(.*)"))]
    [:svg_agg ident]
    (if-some [[_ _ ident] (->> c (re-find #"(?i)^(as)?svg(.*)"))]
      [:svg ident]
      (if-some [[_ ident] (->> c (re-find #"(?i)^viewbox(.*)"))]
        [:viewbox ident])))) 


(defn qr->viewbox
  [v]
  (cond
    (string? v) (string/split v " ")
    (instance? js/ArrayBuffer v) (gaia->viewbox v)
    :else nil))

(rum/defc qr-svgview
  [rowdata col-index id cfunc]
  (let [r (get rowdata col-index)
        [x y w h :as vbox] (or (some->> (get cfunc [:viewbox id])
                                        (aget rowdata)
                                        qr->viewbox)
                               [-8 -8 16 16]
                               #_[-20 -20 40 40])
        mmpx 10]
    #_(println "id" (pr-str id) "Cfunc" (pr-str cfunc) )
    #_(println "vbox " vbox (some->>
                           (get cfunc [:viewbox id])
                           (aget rowdata)))
    [:svg {:viewBox (string/join " "vbox)
           :width (str (* w mmpx) "px")
           :height (str (* h mmpx) "px")
           }
     [:g {:stroke-width 0.12
          :stroke "#fff"
          :fill "none"
          :stroke-linecap "round"
          ;; flipsies
          :transform  "scale(1,1)"}
      (for [sp (string/split r ";")]
        [:path {:d sp}])]]))

(rum/defc tabular-svg-item
  [idef iuse ixf iclass isw itext ipathd]
  (cond
    iuse [:use (cond-> {:href (str "#" iuse)}
                 ixf (assoc :transform ixf)
                 iclass (assoc :class iclass)
                 isw (assoc :stroke-width isw))]
    
    itext (let [fs 1
                tw (*  0.62 fs (count itext))
                th fs]
            [:text (cond-> {:stroke "none"
                            :fill "#fff"
                            :text-anchor "middle"
                            :dominant-baseline "middle"
                            :font-size fs}
                     ixf (assoc :transform ixf)
                     iclass (assoc :class iclass)
                     isw (assoc :stroke-width isw)
                     idef (assoc :id idef))
             itext]
            #_[:rect {:x (- (* 0.5 tw))
                      :y (* -0.5 fs)
                      :width tw
                      :height th
                      :transform ixf
                      :vector-effect "non-scaling-stroke"
                      :stroke-width 1}])
    
    (nil? ipathd)
    (do (println "Don't understand this tsvg row (no path)"
                 {:idef idef
                  :iuse iuse
                  :itext itext}))
    
    :else [:path (cond-> {:d ipathd :stroke-linecap "round"}
                   ixf (assoc :transform ixf)
                   iclass (assoc :class iclass)
                   isw (assoc :stroke-width isw)
                   (nil? isw) (assoc :vector-effect "non-scaling-stroke" :stroke-width 1)
                   idef (assoc :id idef))]
    ))

#_(rum/defc tabular-svg < rum/static
  [col->idx rows]
  (let [idef   (col->idx "def")
        iuse   (col->idx "use")
        ixf    (col->idx "transform")
        iclass (col->idx "class")
        isw    (col->idx "stroke_width")
        itext  (col->idx "text")
        ipathd (col->idx "d")
        
        svgref (rum/create-ref)
        
        def-groups (group-by (fn [r] (aget r "def")) rows)]
    [:div
     [:button {:style {:width "10ex"}
               :on-click (fn []
                           (->> (doto (.cloneNode (rum/deref svgref) true)
                                  (.setAttribute "xmlns:sodipodi" "http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd")
                                  (.prepend (doto (js/document.createElement "sodipodi:namedview")
                                              (.setAttribute "pagecolor" "#000"))))
                                (.serializeToString (js/XMLSerializer.))
                                (ask-download-file "image.svg")))}
      "save svg"]
     [:svg {:ref svgref
            :viewBox "-12 -8 16 24"
            :xmlnsXlink "http://www.w3.org/1999/xlink"
            :width "1300px"
            :height "1300px"}
      [:style {} (rc/inline "sted/eda/pcb.css")]
      [:g {:stroke "#fff" :fill "none"
           :stroke-width "0.1"
           :font-size "2"}
       (for [irow (range (count rows))
             :let [r (aget rows irow)]]
         (rum/with-key
           (tabular-svg-item (aget r idef) (aget r iuse) (aget r ixf) (aget r iclass) (aget r isw) (aget r itext) (aget r ipathd))
           irow
           ))]]]))


(rum/defc tabular-svg < rum/static
  [col->idx rows]
  (let [idef   (col->idx "def")
        iuse   (col->idx "use")
        ixf    (col->idx "transform")
        iclass (col->idx "class")
        isw    (col->idx "stroke_width")
        itext  (col->idx "text")
        ipathd (col->idx "d")
        
        svgref (rum/create-ref)
        
        def-groups (into (sorted-map) (group-by (fn [r] (aget r idef)) rows))
        srefs (volatile! [])]
    #_(rum/use-layout-effect! (fn []
                                (doseq [s @srefs]
                                  (let [el (rum/deref s)
                                        bbox (.getBBox el)]
                                    (js/console.log el)
                                    (js/console.log bbox)
                                    (.setAttribute el "viewBox"
                                                   (gstring/format "%f %f %f %f"
                                                                   (.-x bbox) (.-y bbox)
                                                                   (.-width bbox) (.-height bbox)))))
                                nil))
    [:div
     #_[:style {} (rc/inline "sted/eda/pcb.css")]
     [:svg {:ref svgref :style {:display :none}}
      (for [[dname [r & more :as drows]] def-groups]
        (if-not more
          (tabular-svg-item (aget r idef) (aget r iuse) (aget r ixf) (aget r iclass) (aget r isw) (some-> (aget r itext) str) (aget r ipathd))
          [:g {:id dname}
           (for [z drows]
             (tabular-svg-item nil (aget z iuse) (aget z ixf) (aget z iclass) (aget z isw) (some-> (aget z itext) str) (aget z ipathd)))]))]
     [:div {:style {:display "grid"
                    :grid-template-columns "repeat(auto-fill, minmax(100mm, 1fr) ) "
                    :grid-gap "1em"}}
      (for [d (keys def-groups)
            :let [dref (rum/create-ref)
                  _ (vswap! srefs conj dref)]]
        [:div {:style {:width "100%" :border "1px solid cadetblue"}}
         (str d)
         [:svg {:ref dref
                :viewBox "-20 -20 40 40"}
          [:g {:stroke "#fff" :fill "none"
               :stroke-width "0.1"
               :font-size "2"}
           [:use {:href (str "#" d)}]]]])]]))

(def svg-atlas-cache (atom (sorted-map)))

(rum/defc svg-atlas < rum/reactive
  []
  (let [idef 0
        iuse 1
        iclass 2
        ixf 3
        itext 4
        isw 5
        ipathd 6
        
        def-groups (rum/react svg-atlas-cache)]
    (println "Re-prendder atlas11111")
    [:div
     (str "svg atlas size: "
          (count def-groups) )
     #_[:code [:pre (pr-str (keys def-groups))]]
     [:svg {:style {:display :none}}
      (for [[dname [r & more :as drows]] def-groups]
        (if-not more
          (rum/with-key (tabular-svg-item (aget r idef) (aget r iuse) (aget r ixf) (aget r iclass) (aget r isw) (some-> (aget r itext) str) (aget r ipathd))
            dname)
          [:g {:id dname :key dname}
           (for [z drows]
             (tabular-svg-item nil (aget z iuse) (aget z ixf) (aget z iclass) (aget z isw) (some-> (aget z itext) str) (aget z ipathd)))])) ]]))

(defn ensure-svg-deps!
  [db svg-def-ids]
  #_(println "Ensure svg deps" svg-def-ids)
  (a/let [rs (-> (.exec db (rc/inline "sted/eda/svg_deps.sql") #js [(js/JSON.stringify (into-array svg-def-ids))])
                 (.-get)
                 (.-sync))]
    (let [cols       (.-cols rs)
          rows       (.-rows rs)
          my-defs    (into #{} (map (fn [r] (aget r 0))) rows)
          known-defs (keys @svg-atlas-cache)
          new-defs   (reduce disj my-defs known-defs)]
      (.free rs)
      #_(println "My-defs" my-defs)
      #_(println "Known" known-defs)
      #_(println "New" new-defs)
      (a/let [ars (-> (.exec db (str "select a.*"
                                     "\nfrom json_each(?) je"
                                     "\n, svg_atlas a where a.def = je.value")
                             #js [(js/JSON.stringify (into-array new-defs))])
                      (.-get)
                      (.-sync))]
        (->> (.-rows ars)
             (group-by (fn [r] (aget r 0)))
             (swap! svg-atlas-cache merge)))
      
      #_(let [new-groups (group-by (fn [r] (aget r 0)) rows)]
          (println "New-groups for" svg-def-ids (keys new-groups))
          (swap! svg-atlas-cache merge new-groups)))))




#_(rum/defc fancy-results < rum/static
  [cols rows]
  (let [idx->cc (into-array (map classify-result-column cols))
        cfunc (into {}
                    (for [i (range (count idx->cc))
                          :when (some? (aget idx->cc i))]
                      [(aget idx->cc i) i]))]
    (println "Re-render the fancy results")
    [:table {:style {:width "100%" }}
     [:tbody
      [:tr {} (for [i (range (count cols))
                    :let [c (aget cols i)]]
                (case (aget idx->cc i)
                  nil [:th {:scope "col"} c]
                   
                  (:svg :svg_agg)
                  [:th {:scope "col"}
                   [:span {} [:span {:style {:background-color "blue"
                                             :border-radius "1ex"
                                             :margin "0 1ex 0 0"}} "SVG"] c]]
                  nil))]
      (for [i (range (count rows))
            :let [rs (aget rows i)]]
        [:tr {:key i}
         (for [j (range (count rs))
               :let [r (aget rs j)
                     cc (aget idx->cc j)]
               :when (case (first cc) :viewbox nil true)]
           [:td {:style {:max-width "25em" :overflow-wrap :anywhere}
                 :key (str "r" i "c" j)}
            (if (nil? cc)
              (if (nil? r)
                "NULL"
                (str r))
              (let [[f id] cc]
                (case f
                  :svg_agg
                  [:svg {:viewBox "-10 -10 20 20"
                         :width "400px"
                         :height "400px"}
                   (for [pd (js/JSON.parse r)]
                     (cond
                       (nil? pd) nil
                       (string? pd) [:path {:stroke-width 0.1
                                            :stroke "#fff"
                                            :fill "none"
                                            :d pd}]
                       (object? pd) [:path {:d (aget pd "d")
                                            :fill "none"
                                            :stroke (or (aget pd "stroke") "#fff")
                                            :stroke-width (or (aget pd "stroke-width") 0.1)}]))]
                  :svg
                  (qr-svgview rs j id cfunc)
                  
                  [:code [:pre "??" (str r)]]
                  )))])])]]))

(rum/defc fancy-results < rum/static
  [db cols rows]
  (let [idx->cc (into-array (map classify-result-column cols))
        cfunc (reduce 
               (fn [a i]
                 (cond-> a
                   (some? (aget idx->cc i))
                   (assoc-in (aget idx->cc i) i)))
               {}
               (range (count idx->cc)))
        svgcols (into-array (vals (cfunc :svg)))
        allsvg (set (for [r rows
                          sci svgcols]
                      (aget r sci)))]
    #_(println "Cols" cols)
    #_(println "Cfunc" cfunc)
    #_(println "Svgcols" svgcols)
    
    #_(println "Allsvg" allsvg)
    
    (if-not db
      (println "No db?????????")
      (ensure-svg-deps! db allsvg))
    [:table {:style {:width "100%" }}
     [:tbody
      [:tr {}
       (for [i (range (count cols))
             :let [c (aget cols i)]]
         [:th {:key c :scope "col"} c])]
      (for [i (range (count rows))
            :let [rs (aget rows i)]]
        [:tr {:key i}
         (for [j (range (count rs))
               :let [r (aget rs j)
                     cc (aget idx->cc j)]
               :when (case (first cc) :viewbox nil true)]
           [:td {:style {:max-width "25em" :overflow-wrap :anywhere}
                 :key (str "r" i "c" j)}
            (if (nil? cc)
              (if (nil? r) "NULL" (str r))
              (let [[f id] cc]
                (case f
                  :svg [:svg {:viewBox "-10 -10 20 20"
                              :width "500px"
                              :height "300px"}
                        [:g {:fill "none"}
                         [:use {:href (str "#" r)}]]]
                  [:code [:pre "??" (str r)]])))])])]]))

(defn promise-chain
  [ps]
  (reduce
   (fn [acc-pr next-pr]
     (a/let [pval acc-pr]
       next-pr))
   nil
   ps))

(defn create-schema!
  [db]
  (promise-chain
   (for [stmt edaschema/schema-statements]
     (.exec db stmt))))

(defn setup-db!
  [db]
  (a/let [_ (println "Creating schema")
          _sch (create-schema! db)
          _ (println "Schema created")
          a (kcfp/insert-footprint! db (first (dsn/dsnparse (rc/inline "sted/eda/qfn.kicad_mod"))))
          b (kcfp/insert-footprint! db (first (dsn/dsnparse (rc/inline "sted/eda/gauge.kicad_mod"))))
          c (kcfp/insert-footprint! db (first (dsn/dsnparse (rc/inline "sted/eda/Kailh_socket_MX_6.kicad_mod"))))
          d (kcfp/insert-footprint! db (first (dsn/dsnparse (rc/inline "sted/eda/7segment.kicad_mod"))))
          e (.exec db "insert into svg_atlas select * from svg_atlas_view")]
    :ok))

(defn regen-svg-atlas!
  [db]
  (let [ts (js/performance.now)]
    (a/let [d (.exec db "delete from svg_atlas")
            e (.exec db "insert into svg_atlas select * from svg_atlas_view")]
      (println "Svga time" (-  (js/performance.now) ts))
      :ok)))

(rum/defc spltest
  []
  (let [ref (rum/create-ref)
        [db set-db!] (rum/use-state nil)
        [query set-query!] (rum/use-state siquery)
        [[cols rows qdur] set-results!] (rum/use-state [[] []])
        [display-mode set-display-mode!] (rum/use-state :table #_:tabular-svg)
        spl-options #js {:autoGeoJSON false
                         :autoJSON false}]
    (rum/use-effect!
     (fn []
       (a/let [s (spl. #js [] spl-options)
               the-db (.db s)
               spr (setup-db! the-db)
               result (-> (.exec the-db query)
                          (.-get))]
         (set-db! the-db)
         #_(println "SPR" spr)
         (a/let [cols (.-cols result)
                 rows (.-rows result)]
           #_(println "SR" cols rows)
           (set-results! [cols rows])))
       nil)
     [])
    [:div {:style {:display :grid
                   :grid-template-columns "1fr"
                   :width "90%"}}
     [:textarea.code-font {:value query
                           :spellCheck "false"
                           :style {:background-color "#000"
                                   :color "#fff"
                                   :height "40ex"
                                   :margin-top "0.5ex"}
                           :on-change (fn [ev] (set-query! (.-value (.-target ev))))
                           :on-key-down (fn [ev]
                                          (when (and (.-ctrlKey ev)
                                                     (= "Enter" (.-key ev)))
                                            (set-query! (.-value (.-target ev)))))}]
     [:div {:style {:display :flex}}
      [:button {:style {:width "10ex" :margin-right "4ex"}
                :on-click (fn []
                            (let [tqs (js/performance.now)]
                              (a/let [result (-> (.exec db query)
                                                 (.-get))
                                      cols (.-cols result)
                                      rows (.-rows result)]
                                (.free result)
                                (set-results! [cols rows (- (js/performance.now) tqs)]))))}
       "Go"]
      (str (count rows) " rows"
           (when qdur (str " in " (.toFixed qdur 1) "ms")))
      
      [:label {:style {:margin-left "4ex"}}
       ".kicad_mod"
       [:input {:type "file"
                :style {:width "50ex"}
                :multiple true
                :on-change (fn [ev]
                             (let [ps (for [file (seq (.-files (.-target ev)))
                                            :let [fr (js/FileReader.)
                                                  rfp (js/Promise.
                                                       (fn [resolve reject]
                                                         (set! (.-onload fr) (fn [ev] (resolve (.-result (.-target ev)))))
                                                         (set! (.-onerror fr) reject)
                                                         (println "I read the text " (.-name file))
                                                         (.readAsText fr file)))]]
                                        (a/let [content rfp]
                                          (-> (for [top (dsn/dsnparse content)]
                                                (kcfp/insert-footprint! db top))
                                              (promise-chain)
                                              (.catch (fn [ex]
                                                        (js/console.error "In file" file ex )
                                                        ))))
                                        #_(kcfp/insert-footprint! db (first (dsn/dsnparse (vec (.-files (.-target ev)))))))
                                   ]
                               (a/let [done (promise-chain ps)]
                                 (js/console.log "Done" done))))}]]
      [:label
       "display mode"
       [:select {:on-change (fn [ev]
                              (prn "SETDM" (keyword (.-value (.-target ev))))
                              (set-display-mode! (keyword (.-value (.-target ev)))))}
        (for [o [:table :tabular-svg]]
          [:option {:key (name o)
                    :value (name o)
                    :selected (= o display-mode)}
           (name o)])]]

      [:button {:style {:width "20ex" :margin "0 4ex 0 4ex"}
                :on-click (fn []
                            (when db (regen-svg-atlas! db)))}
       "Regen atlas"]
      ]
     
     [:style {} (rc/inline "sted/eda/pcb.css")]
     (case display-mode
       :table (fancy-results db cols rows)
       :tabular-svg (tabular-svg
                     (into {} (for [i (range (count cols))]
                                [(aget cols i) i]))
                     rows))]))



(rum/defc retest
  []
  (let [example "M0,0 v100 l5,4"]
    [:div {}
     [:h2 "Path D"]
     [:code [:pre
             (pr-str (path-d/svg-d-lex example))]]
     [:code [:pre
             (pr-str
              (svgo-lib-path/parsePathData example))]]]))

(rum/defc kcptest
  []
  (let [parsed (->> #_(rc/inline "sted/eda/qfn.kicad_mod")
                    (rc/inline "sted/eda/Kailh_socket_MX_6.kicad_mod")
                    dsn/dsnparse first kcnext/kicad->edn)
        
        printed (kcnext/print-as-sexp
                 (kcnext/footprint->sexp
                  (kcnext/grid-of
                   parsed
                   3 3
                   30 30)))
        reparsed (->> printed dsn/dsnparse first kcnext/kicad->edn)
        w 180
        ]
    [:div {}
     [:h2 "Kicad->edn " (pr-str (= parsed reparsed))]
     [:div {:style {:display :flex :flex-direction :row
                    :font-size "10px"}}
      [:code [:pre
              (do (zp-hacks/set-options! {:style :fast-hang :width w})
                  (zp-hacks/zprint-file-str (pr-str parsed) "1"))]]
      [:code [:pre
              (do (zp-hacks/set-options! {:style :fast-hang :width w})
                  (zp-hacks/zprint-file-str (pr-str reparsed) "1"))]]
      #_[:code [:pre
              (do (zp-hacks/set-options! {:style :fast-hang :width w})
                  (zp-hacks/zprint-file-str (pr-str (kcfp/footprint->sexp parsed)) "1"))]]

      [:code [:pre printed]]]]))

(rum/defc capture-inner
  [el]
  (let [bound (.getBoundingClientRect el)
        vbox (gstring/format "%f %f %f %f"
                             (.-x bound) (.-y bound)
                             (.-width bound) (.-height bound))]
    [:div {:style {:margin-left "2ex"}}
     [:svg {:style {:outline "1px dashed #fff"}
            :viewBox vbox}
      [:g {:stroke-width "1" :fill "none" :stroke "#fff"}
       (for [e (.querySelectorAll el "svg")]
         (let [c (.getBoundingClientRect e)
               v (.-baseVal ^js (.-viewBox e))
               cw (.-width c)
               ch (.-height c)
               vw (.-width v)
               vh (.-height v)
               xscl (/ cw vw)
               yscl (/ ch vh)]
           [:use {:href (str "#" (.-id e))
                  :x (.-x c)
                  :y (.-y c)
                  :width cw
                  :height ch}]))]]]))

(rum/defc ct-key
  [i]
  [:svg {:id (str "tpat" i)
         :width "16mm"
         :height "16mm"
         :viewBox "-8 -8 16 16"}
   [:rect {:x -8
             :stroke "#fff"
             :stroke-width "1"
             :y -8
             :width 16
             :height 16}]
   [:text {:stroke "none"
           :fill "#fff"
           :stroke-width "0.1"
           :font-size 9
           :x 0
           :y 3
           :text-anchor "middle"}
    (str i)]
   [:g {:stroke "#fff" :fill "none"
        :stroke-width "0.1"
        :font-size "2"}
    [:use {:href (str "#kcf3" )}]]])

(rum/defc capturetest
  []
  (let [[scale set-scale!] (rum/use-state 1)
        [el set-el!] (rum/use-state nil)
        container (rum/create-ref)]
    (rum/use-layout-effect!
     (fn [] (set-el! (rum/deref container)))
     #js [scale])
    [:div {:style {:display :flex
                   :flex-direction "row"}}
     [:div {}
      [:label {}
        (str "Scale:" (.toFixed scale 4))
        [:input {:type "range"
                 :min "1"
                 :max "10"
                 :step "0.2"
                 :value scale
                 :on-change #(set-scale! (js/parseFloat (.. % -target -value)))}]]
      [:div {:style {:overflow "auto"
                     :outline "1px solid #eee"}}
       [:div {:style {:transform (str "scale(" scale ")")
                      :transform-origin "top left"
                      ;; :transform-origin "50% 50%"
                      }}
        (mmruler 20 10 100)
        [:div {:ref container
               :style {:outline "1px dashed cadetblue"
                       :display :flex
                       :flex-direction "column"
                       :width :max-content}}
         
         [:div.screen {:style {:display :flex :gap "4mm" :padding "4mm" :flex-direction "row"}}
  ;; (fp_line (start 33 21) (end 33 -3.5) (layer "F.CrtYd") (width 0.05) (tstamp 2fe8eab0-a49c-4bd1-a4e3-4940f67f2ff2))
  ;; (fp_line (start -3 -3.5) (end -3 21) (layer "F.CrtYd") (width 0.05) (tstamp 30571ab7-7bf1-46ef-bbc0-984836a45485))
  ;; (fp_line (start -3 21) (end 33 21) (layer "F.CrtYd") (width 0.05) (tstamp 73487adf-c479-439b-be7e-5656df49ecf7))
  ;; (fp_line (start 33 -3.5) (end -3 -3.5) (layer "F.CrtYd") (width 0.05) (tstamp 9ff77f87-91a1-4d04-80d6-e862bcc4085e))

          
          [:div.screen {:style {:display :flex
                                :flex-direction "row"
                                :width ""}}
           (for [i (range 4)]
             [:div.wtferl {:key (str "B" i)}
              [:svg {:id (str "Binkus" i)
                     :width "25.5mm"
                     :height "36mm"
                     :viewBox "-3.5 -33 25.5 36"}
               [:g {:stroke "#fff" :fill "none" :stroke-width "0.1" :font-size "2"
                    :transform "rotate(-90)"}
                [:rect {:x -3 :y -3.5 :width 36 :height 25}]
                [:use {:href (str "#kcf4" )}]]]])]]
         
         [:div.buttons
          {:style {:display :flex
                   :gap "4mm"
                   :padding "4mm"
                   :flex-direction "row"
                   :justify-content "space-between"}}
        
          [:div.numpad
           {:style
            {:display :grid
             :grid-template-columns "repeat(3, 1fr)"
             :grid-gap "3mm"}}
           (for [i [7 8 9 4 5 6 1 2 3 0 "+/-" "."]]
             (rum/with-key (ct-key i) i))]
        
          [:div.functions
           {:style
            {:display :grid
             :grid-template-columns "repeat(2, 1fr)"
             :grid-gap "3mm"}}
           (for [i ["-=" "C" "/" "<->" "x" "MR" "+=" "M+"]]
             (rum/with-key (ct-key i) i))]]]]]]
     
     [:div {:style {:height "700px"
                    :width "600px"
                    :outline "1px dashed blue"}}
      (if-not el
        "No element"
        (capture-inner el))]]))

(rum/defcs dsnroot < (rum/local nil ::result)  
  [{::keys [result]}]
  (let [[[_pcb dsnfile & body]] (dsn/dsnparse
                                 (rc/inline "sted/eda/RP2040_minimal.dsn"))
        toplevel (group-by first body)
        structure (group-by first (mapcat next (get toplevel 'structure)))
        boundary (group-by first (mapcat next (get structure 'boundary)))
        bpath (group-by second (get boundary 'path ))
        
        [[_path _pcb _unk & coords ]] (get bpath "pcb")
        boundary-pts (partition-all 2 coords)
        [bx by bw bh] (aabb 0 boundary-pts)
        
        library (group-by first (mapcat next (get toplevel 'library )))
        id->image (group-by second (get library 'image))
        id->padstack (group-by second (get library 'padstack))
        [_placement & plcbody] (toplevel 'placement)]
    [:div {:style {:margin-left "1ex"}}
     #_(kicad-footprint-debug
        (first (dsn/dsnparse kicadstr-kailhsocket))
        #_(kicad->edn kicadstr-kailhsocket))
     
     (spltest)
     #_(retest)
     #_(kcptest)
     [:div {}
      #_[:h2 "Area"]
      #_(hruler  2 300)
      (capturetest)
      #_[:div {:style {:outline "1px dashed cadetblue"
                       :width "300mm"
                       :display :grid
                       :grid-template-columns "repeat(auto-fit, 20mm)"}}
       
         (for [i (range 9)]
           [:svg {:key i
                  :width "16mm"
                  :height "16mm"
                  :viewBox "-9 -9 18 18"}
            [:g {:stroke "#fff" :fill "none"
                 :stroke-width "0.1"
                 :font-size "2"}
             [:use {:href (str "#kcf3" )}]]])]]
     (svg-atlas)
     #_[:span
        {}
        (pr-str
         (let [[xmin ymin w h] [74000 -121000 (* 9 52000) 101000]]
           [xmin ymin
            (+ w xmin) ymin
            (+ w xmin) (+ ymin h)
            xmin (+ ymin h)]))]
     
     (comment
       "The big one"
       
       [:div
        [:div {:style {:width "100px"}} (kicadsvg nil kicadedn)]
        [:svg {:viewBox (gstring/format "%f %f %f %f" bx by bw bh)
               :width "1000px"
               :height "1000px"
               :style {:border "1px solid cadetblue"}}
         #_[:foreignObject {:x bx :y by :width bw :height bh}
            (hruler bx by bw bh)
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
         [:g {:transform (str "translate(" 0 "," (+ by by bh) ")"
                              " scale(1,-1)")
              :fill "none"
              :stroke "#fff"}
        
          ;; placements
          (for [[_placement & comps] (toplevel 'placement)
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
                     :stroke (case layer
                               "F.Cu" "tomato"
                               "blue")
                     :d (pts->d (partition-all 2 pts))
                     :data-net (props 'net)}])]
        
          ;; vias
          (for [[_wiring & wires] (toplevel 'wiring)
                [via & args] wires
                :when (= 'via via)
                :let [[vianame x y & props] args]]
            [:use.via {:transform (str "translate(" x " " y ")")
                       :href (str "#" vianame)}])

          ;; planes
          (for [[_plane plane-name [ty layer _unk & coords] & windows] (structure 'plane)
                :let [planepts (partition-all 2 coords)]
                :when (case [plane-name layer]
                        [ "GND" "B.Cu"] nil true)]
            [:g (case ty
                  'polygon
                
                  [:path { ;; :fill "#fff"
                          :fill "none"
                          ;; :opacity "0.2"
                          :stroke "cadetblue"
                          :stroke-width 99
                          :d (pts->d planepts)}]
                  nil)
           
             (when-some [window-pts (not-empty
                                     (for [[_window [ty layer aw & coords]] windows]
                                       (partition-all 2 coords)))]
               (let [comprects (for [[_placement & comps] (toplevel 'placement)
                                     [_comp cclass & places] comps
                                     [_place cname x y layer rot [_pn pn]] places
                                     [_image imgid & body] (id->image cclass)
                                     [pin padstack & args] body
                                     :when (= pin 'pin)
                                     :let [rot? (and (vector? (first args)) (= 'rotate (ffirst args)))
                                           [pin-id px py] (if-not rot?
                                                            args
                                                            (next args))
                                           deg (when rot? (second (first args)))]
                                     [_padstack _ps & psbody] (id->padstack padstack)
                                     [shape [shtype layer & shpts]] psbody
                                     :when (= [shape layer] ['shape "F.Cu"])
                                     :when (#{'rect 'polygon} shtype)]
                                 (let [pathdata (case shtype
                                                  rect (let [[xa ya xb yb] shpts]
                                                         (str (pts->d [[xa ya] [xa yb] [xb yb] [xb ya]]) "z"))
                                                  polygon (pts->d (partition-all 2 (next shpts))))
                                       pdjs (svgo-path/path2js #js {:attributes #js {:d pathdata}})
                                       xfpts (svgo-applytransforms/applyTransforms
                                              #js {:attributes #js {:transform (str "translate(" x " " y ")" 
                                                                                    "rotate(" rot ")"
                                                                                    "translate(" px " " py ")")}
                                                   :computedAttr (fn [] nil)}
                                              pdjs
                                              #js {:transformPrecision 99})]
                                   (loop [i 0
                                          acc []]
                                     (if (= i (count pdjs))
                                       acc
                                       (let [p (nth pdjs i)]
                                         (recur (inc i)
                                                (case (.-command p)
                                                  ("L" "M") (conj acc (.-args p))
                                                  "z" (conj acc (nth acc 0))))))))
                                 #_(println (str "#" cclass padstack xa ya xb yb)))
                     diff-subj (clj->js [planepts])
                     diff-arg (clj->js [window-pts])
                     _ (js/console.time "Geo")
                     buffered (.-coordinates
                               (.write
                                (jts-io/GeoJSONWriter.)
                                (jts-buf/BufferOp.bufferOp
                                 (.read (jts-io/GeoJSONReader.)
                                        #js {:type "MultiPolygon"
                                             :coordinates (clj->js (for [c comprects] [c]))})
                                 450)))
                     mpcr (mpc/diff
                           (mpc/diff diff-subj (do diff-arg))
                           buffered)
                   
                     pathdata (cond
                                (empty? mpcr) nil
                              
                                (number? (-> mpcr (nth 0) (nth 0)))
                                (pts->d mpcr)
                              
                                :else
                                (apply str
                                       (for [poly mpcr
                                             ring poly]
                                         (str (pts->d ring) "  "))))
                     _ (js/console.timeEnd "Geo")]
               
                 #_(js/console.log "Subj" (js/JSON.stringify diff-subj))
                 #_(js/console.log "Argu" (js/JSON.stringify diff-arg))
                 #_(js/console.log "Resu" (js/JSON.stringify mpcr))
                 #_(js/console.log "Pthd" pathdata)
                 [:g.allwin
                  [:g.punch
                   [:path {:fill "red"
                           :opacity 0.4
                           :fill-rule "evenodd"
                           :stroke-width 50
                           :stroke "none"
                           :d pathdata}]]
                  (for [c comprects]
                    [:path.dink
                     {:stroke-width 99
                      :stroke "#fff"
                      :d (pts->d c)}])]))])]]])
     
     (comment
       (dsn-structure structure)
       (dsn-library library))]))

(defonce the-yosys (atom nil))

(defn new-yosys!
  []
  (reset! the-yosys
          (.create_worker js/window.YosysJS
                          (fn [] (js/console.log "Yosys created")))))

(rum/defc elk-node-svg
  [{:keys [id width height x y ports children labels edges cell_type] :as cc}]
  [:g {:transform (str "translate(" x "," y ")")}
   (case cell_type
     "rv" [:use {:href (str "#R_US")
                 :width width
                 :height height
                 }]
     nil nil
     )
   
   [:rect {:key (str "r" id)
           :id (str "r" id)
           :width width :height height
           :stroke "#fff"
           :fill "none"}]
   (for [lbl labels]
     [:text {:x (:x lbl)
             :y (:y lbl)
             :fill "#fff"
             :stroke "none"
             :font-size 12}
      (:text lbl)])
   (for [p ports]
     [:g
      [:rect {:id (str "p" (:id p))
              :key (str "p" (:id p))
              :x (:x p) :y (:y p) :width (:width p) :height (:height p)
              :stroke "tomato"}]
      (when-some [ls (seq (:labels p))]
        (for [lbl ls]
          [:text {:x (+ (:x p) (:x lbl))
                  :y (+ (:y p) (:y lbl))
                  :fill "#fff"
                  :stroke "none"
                  :font-size 12}
           (:text lbl)]))])
   (for [{:keys [sources targets sections] :as e} edges
         {:keys [startPoint endPoint bendPoints] :as s} sections]
     [:path { 
             :stroke "green"
             :stroke-width 1
             :fill "none"
             :d (str "M" (:x startPoint) " " (:y startPoint)
                     ;; "L " (:x endPoint) " " (:y endPoint)
                     (apply str
                            (for [{:keys [x y]} (conj (or bendPoints []) endPoint)]
                              (str " L " x " " y))))}])

   (for [{:keys [junctionPoints]} edges
         {:keys [x y]} junctionPoints]
     [:circle {:stroke "none"
               :fill "green"
               :cx x
               :cy y
               :r 4}])
   
   (for [c children]
     (elk-node-svg c))])

(defn ys-flatten-module
  [modname ^js mod]
  (concat
   (for [[portname port] (js/Object.entries (.-ports mod))
         :let [dir (.-direction port)]
         bit (.-bits port)]
     #js {:bit bit
          ;; :module modname
          ;; :port portname
          :node (str modname "." portname)
          :direction dir})
   
   (for [[cellname cell] (js/Object.entries (.-cells mod))
         :let [pdir (.-port_directions cell)]
         [cname cvec] (js/Object.entries (.-connections cell))
         bit cvec]
     #js {:bit bit
          ;; :module modname
          ;; :cell cellname
          ;; :connection cname
          :node (str cellname "." cname)
          :direction (aget pdir cname)})))

(defn dingu
  [^js ys]
  (reduce
   (fn [acc [bit-id attrs]]
     (update acc bit-id (fnil conj []) attrs))
   {}
   (for [[modname mod] (js/Object.entries (.-modules ys))
         [netname net] (js/Object.entries (.-netnames mod))
         :let [as (.-attributes net)]
         b (.-bits net)]
     [b as])))




(def ^:const n-safe-bits (js/Math.log2 js/Number.MAX_SAFE_INTEGER))
(defn ys-numeric-attr
  [attrs key]
  (when attrs
   (let [v (aget attrs key)]
     (cond
       (nil? v)                  nil
       (number? v)               v
       (not (string? v))         (throw (js/Error. (str "What is this numeric attribute? " (pr-str v) " at " (.-src attrs))))
       (< n-safe-bits (count v)) (throw (js/Error. (str "Sorry, we only have " n-safe-bits " bits")))
       :else                     (js/parseInt v 2)))))

(defn ys->elk
  [^js e]
  (let [bit->attrs (dingu e)
        top-module (last (js/Object.keys (.-modules e )))

        cc (volatile! 0)]
    {:id            "root"
     ;; :properties {:algorithm "layered"}
     :layoutOptions {"org.eclipse.elk.edgeRouting"           "ORTHOGONAL"
                     "elk.algorithm"                         "layered"
                     "elk.spacing.portPort"                  20
                     "org.eclipse.elk.portAlignment.default" "DISTRIBUTED"}
     ;; modules
     :children
     (for [[mn m] (js/Object.entries (.-modules e))]
       (let [bit-reverse-index (group-by (fn [^js f] (.-bit f)) (ys-flatten-module mn m))]
         (println "Module " mn "Attrs" (.-attributes m))
         {:id            mn
          :labels        [{:text mn}]
          :width         (or (ys-numeric-attr (.-attributes m) "elk_width")
                             500)
          :height        (or (ys-numeric-attr (.-attributes m) "elk_height")
                             500)
          :layoutOptions {"org.eclipse.elk.portConstraints" "FIXED_SIDE"
                          "org.eclipse.elk.direction"       "DOWN"}
          :ports         (for [[pn p] (js/Object.entries (.-ports m))]
                           {:width         30
                            :height        10
                            :layoutOptions {"org.eclipse.elk.port.side" (case (.-direction p) "input" "WEST" "output" "EAST" "UNDEFINED")}
                            :id            (str mn "." pn)
                            :labels        [{:text pn}]})
          
          :edges (concat
                  ;; constant
                  (for [const  ["0" "1"]
                        ^js pc (bit-reverse-index const)]
                    {:id      (str "Ec" mn "_" const (vswap! cc inc))
                     :sources [(str mn "_cport" const)]
                     :targets [(.-node pc)]})
                  
                  ;; dummy
                  #_(for [[bit-id ^js pcs] bit-reverse-index
                          :when            (and (number? bit-id)
                                                (< 2 (count pcs)))
                          [i n]            (map vector (range) pcs)]
                      (do
                        (println "Dummy" bit-id n)
                        {:id      (str mn "_de_" bit-id "_" i)
                         :sources [(str mn "_dummy_" bit-id)]
                         :targets [(.-node n)]}))

                  (for [[bit-id ^js pcs] bit-reverse-index
                        :when            (and (number? bit-id)
                                              (< 2 (count pcs)))
                        :let             [arb (first pcs)]
                        other            (next pcs)]
                    (do
                      (println "Dummy Edge " (.-node arb)
                               " -> "
                               (.-node other)
                               )
                      
                      (if (= "input" (.-direction arb))
                        {:id      (str mn "_de_" (vswap! cc inc))
                         :sources [(.-node arb)]
                         :targets [(.-node other)]}
                        {:id      (str mn "_de_" (vswap! cc inc))
                         :sources [(.-node other)]
                         :targets [(.-node arb)]})))

                  ;; connection
                  (->> (for [[bit-id ^js pcs] bit-reverse-index
                             :when            (and (number? bit-id)
                                                   (= 2 (count pcs)))]
                         (let [[a b] pcs]
                           (case [(.-direction a) (.-direction b)]
                             ;; it is important that ELK knows about edge directions
                             ["input" "output"]    [(.-node b) (.-node a)]
                             (["input" "input"]
                              ["output" "input"]
                              ["output" "output"]) [(.-node a) (.-node b)]
                             (throw (ex-info "Do not understand this connection" pcs)))))
                       (frequencies)
                       (map
                        (fn [i [[srcn dstn] nbits]]
                          (println "Connect" nbits srcn "  ->  " dstn)
                          {:id      (str "Ee" mn "_" i)
                           :sources [srcn]
                           :targets [dstn]})
                        (range))))
          
          ;; cells
          :children (concat
                     ;; constant
                     (for [const ["0" "1"]
                           :when (bit-reverse-index const)]
                       {:id     (str "const" const)
                        :width  20
                        :height 20
                        :labels [{:text const}]
                        :ports  [{:id (str mn "_cport" const)}]})
                     
                     ;; dummy
                     (filterv some?
                              (for [[bit-id ^js pcs] bit-reverse-index
                                    :when            (and (number? bit-id)
                                                          (< 2 (count pcs)))]
                                (println "Dummy" pcs)
                                #_{:id     (str mn "_dummy_" bit-id)
                                   :width  1
                                   :height 1
                                   :labels [{:text (str "Dummy" bit-id)}]
                                   }))
                     
                     ;; cell
                     (for [[cn c] (js/Object.entries (.-cells m))
                           :let   [pdir (.-port_directions c)
                                   ^js proto (aget (.-modules e) (.-type c))]]
                       (let []
                         (println "Cell" cn #_(get module->attrs (.-type c)))
                         {:id            cn
                          :cell_type     (.-type c)
                          :width         (or (ys-numeric-attr (.-attributes proto) "elk_width")
                                             50)
                          :height        (or (ys-numeric-attr (.-attributes proto) "elk_height")
                                             100)
                          :labels        [{:text cn}]
                          :layoutOptions {"org.eclipse.elk.portConstraints" "FIXED_SIDE"}
                          :ports         (for [[pn bits] (js/Object.entries (.-connections c))]
                                           {:id            (str cn "." pn)
                                            :width         20
                                            :height        10
                                            :labels        [{:text pn}]
                                            :layoutOptions {"org.eclipse.elk.port.side"
                                                            (or (some-> (.-netnames proto)
                                                                        (aget pn)
                                                                        (.-attributes)
                                                                        (aget "elk_port_side"))
                                                                (case (aget pdir pn) "input" "WEST" "output" "EAST" "FREE"))}})})))}))}))

(defonce the-elk (js/ELK.))

(rum/defc view-ys-json
  [jst]
  (let [e   (js/eval (str "(" jst ")")) ; eval because output has comments
        clj-graph (ys->elk e)
        [result set-result!] (rum/use-state nil)]
    
    (rum/use-effect!
     (fn []
       #_(.then (.layout elk graph) set-result!)
       #_(js/console.log "GRAPH" (clj->js clj-graph))
       (a/let [r (.layout the-elk (clj->js clj-graph))]
         #_(js/console.log r)
         (set-result!  r #_(js->clj r :keywordize-keys true)))
       nil)
     [jst])
    
    (if-not result
      [:div {} "No result yet"]
      [:div {:style {:display :flex}}
       [:svg {:style {:width  "800px"
                      :height "800px"
                      :border "1px solid aliceblue"}}
        (elk-node-svg (js->clj result :keywordize-keys true))]
       [:textarea.code-font {:value (js/JSON.stringify result nil 2)
                             :spellCheck "false"
                             :readOnly true
                             :style {:background-color "#000"
                                     :color "#fff"
                                     :height "40ex"
                                     :margin-top "0.5ex"
                                     :font-variant-ligatures "no-contextual"}}]
       ])))


(rum/defc symbol-svg
  [sym]
  [:div {:style {:width "200px"
                 :height "200px"
                 :style {:outline "1px dashed #fff"}}}
   (for [top (sym 'symbol)]
     [:svg {:id (str (top 'id))
            ;; :style {:outline "1px dashed #fff"}
            ;; :viewBox "-2 -5 4 10"
            :viewBox "-5 -10 10 20"
            ;; :width "200px"
            ;; :height "200px"
            }
      #_[:rect {:stroke "tomato" :stroke-width "0.1"
              :x -2
              :y -5
              :width 4 :height 10}]
      (for [unit (top 'symbol)]
        [:g {:stroke-width "0.1" :fill "none" :stroke "#fff"
             :transform "scale(1,-1)"}
         (for [draw (unit 'draw)]
           (case (draw 'draw)
             polyline [:path {:d (pts->d (draw 'pts))
                              :fill (case (second (draw 'fill))
                                      (nil none) "none"
                                      outline "#fff")}]
             circle (let [[cx cy] (draw 'center)]
                      [:circle {:cx cx
                                :cy cy
                                :r (draw 'radius)
                                :stroke-width (max 0.1 (get (draw 'stroke) 'width 0))}])
             rectangle (let [[sx sy] (draw 'start)
                             [ex ey] (draw 'end)
                             xmin (min sx ex)
                             ymin (min sy ey)
                             xmax (max sx ex)
                             ymax (max sy ey)
                             w (- xmax xmin)
                             h (- ymax ymin)]
                         [:rect {:x xmin :y ymin :width w :height h}])
             text (let [[x y r] (draw 'at)
                        [fw fh] (get-in draw '[effects font size])]
                    [:g {:transform (str
                                     "translate(" x "," y ")"
                                     " rotate(" (* 0.1 r) ") "
                                     "scale(1,-1)")}
                     [:text {:y (* 0.5 fh)
                             :stroke "none"
                             :text-anchor "middle"
                             :fill "#fff"
                             :font-size fw}
                      (draw 'text)]])
             
             (println "What is this draw?" (draw 'draw))))
         (for [{:syms [graphical at length name number] :as pin} (unit 'pin)]
           (let [[x y r] at
                 sideways? (< 90 r)
                 radians (* r js/Math.PI (/ 1 180.0))]
             [:g.pins
              {:transform (str "translate(" x "," y ")"
                               " rotate(" r ") ")}
              (case graphical
                line [:path {:d (str "M" 0 "," 0 " h" length)}]
                nil)
               
              [:text {:y 0.3
                      :text-anchor (if-not sideways? "end" "start")
                      :stroke "none"
                      :fill "#fff"
                      :font-size 1
                      :transform (str "scale(1,-1)"
                                      (when sideways? " rotate(-180)"))}
               (str (pin 'electrical))]
              
              (let [[num-fw] (get-in number '[effects font size])
                    fw (max 1 (or num-fw 1))
                    tx (* 0.5 length)
                    ty (cond-> (* 0.3 fw) sideways? -)]
                [:text {:text-anchor "middle"
                        :stroke "none"
                        :fill "#fff"
                        :font-size fw
                        :transform (str "translate(" tx "," ty ")"
                                        " scale(1,-1)"
                                        (when sideways? " rotate(-180)"))}
                 (number 'text)])]))])])])

(rum/defc ysroot
  [ys]
  (let [[code set-code!] (rum/use-state
                          (rc/inline "sted/eda/counter.v"))
        [result set-result!] (rum/use-state nil)
        ^js ys @the-yosys
        go (fn go []
             (doto ys
               (.verbose true)
               (.write_file "in.v" code)
               (.run (string/join "; " ["design -reset"
                                        "design -reset-vlog"
                                        "read_verilog in.v"
                                        "hierarchy -auto-top"
                                        ;; "uniquify"
                                        ;; "proc"
                                        "write_json -aig out.json"]))
               (.read_file "out.json"
                           (fn [rf]
                             (set-result! rf)
                             #_(js/console.log rf)))))]
    #_(rum/use-effect! (fn [] nil) [])
    [:div {:style {:display :grid
                   :grid-template-columns "1fr"
                   :width "90%"}}
     [:textarea.code-font {:value code
                           :spellCheck "false"
                           :style {:background-color "#000"
                                   :color "#fff"
                                   :height "40ex"
                                   :margin-top "0.5ex"
                                   :font-variant-ligatures "no-contextual"}
                           :on-change (fn [ev] (set-code! (.-value (.-target ev))))}]
     [:div {:style {:display :flex}}
      [:button {:style {:width "10ex" :margin-right "4ex"}
                :on-click go}
       "Yosys"]
      [:button {:style {:width "10ex" :margin-right "4ex"}
                :on-click new-yosys!}
       "Reboot"]]
     
     (symbol-svg
      (kcnext/kicad->edn* {} [(kcnext/edn-reader-hack (rc/inline "sted/eda/q_npn_cbe.kicad_sym")
                                                      #_(rc/inline "sted/eda/r_us.kicad_sym")
                                                      #_(rc/inline "sted/eda/ad630.kicad_sym"))]))
     (when result
       [:div {:style {:display :flex :flex-direction "row"}}
        (view-ys-json result)
        [:textarea.code-font {:value result
                              :spellCheck "false"
                              :readOnly true
                              :style {:background-color "#000"
                                      :color "#fff"
                                      :height "40ex"
                                      :margin-top "0.5ex"
                                      :font-variant-ligatures "no-contextual"}}]
        
        ])]))





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
    (when-not @the-yosys
      (new-yosys!)
      #_(reset! the-yosys
                (.create_worker js/window.YosysJS
                                (fn [] (js/console.log "Yosys created")))))
    
    (println "Created new app and reset kbdb.  Mount root...")
    
    (-> #_(cr/root app code/form)
        #_(elkroot (dsn->elk (first (dsn/dsnparse dsn/dsnstr))))
        #_(dsnroot)
        (ysroot nil)
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



;; https://regexper.com/#%5B-%2B%5D%3F%28%28%28%5Cd%2B%5C.%5Cd%2B%29%7C%5Cd%2B%29%7C%5C.%5Cd%2B%29%28%5BeE%5D%5B-%2B%5D%3F%5Cd%2B%29%3F
;; https://regexper.com/#%5B-%2B%5D%3F%28%28%28%5Cd%2B%5C.%5Cd%2B%29%7C%5Cd%2B%29%7C%5C.%5Cd%2B%29%28%5BeE%5D%5B-%2B%5D%3F%5Cd%2B%29%3F


;; [x] Import the footprints
;; [ ] Arrange them on-screen with CSS
;; [ ] Capture arrangement from DOM
;; [ ] Export back to footprint
;; 

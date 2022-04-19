(ns sted.eda.kicad
  (:require
   [sted.eda.dsn :as dsn]
   [clojure.string :as string]
   #?(:clj [clojure.java.io :as io]))
  #?(:clj
     (:import [java.text DecimalFormat])))

(defn kicad->edn*
  [iacc body]
  (reduce
   (fn [acc b]
     (if-not (coll? b)
       #_(do
           (println "Scalar param" (pr-str b))
           (assoc acc b true))
       (case b
         (hide "hide") (assoc acc 'hide true)
         (do (println "don't understand kicad sexp symbol: " (pr-str b))
             (assoc acc b true)))
       
       (let [[f & args] b]
         (case f
           ;; unquoted rest arguments 
           attr
           (assoc acc f (mapv symbol args))
           
           ;; unquoted scalar argument
           justify
           (assoc acc f (symbol (first args)))
           
           ;; simple recursive, no accumulation
           (font effects scale offset rotate stroke)
           (assoc acc f (kicad->edn* {} args))
           
           ;; simple recursive, accumulated
           zone
           (update acc f (fnil conj []) (kicad->edn* {} args))
           
           ;; recursive with positional parameters, accumulated
           pad 
           (let [[number type shape & rest] args]
             (->> rest
                  (kicad->edn* {'number number 'type (symbol type) 'shape (symbol shape)})
                  (update acc 'pad (fnil conj []))))
           
           pin
           (let [[e g [_at & pos] [_length len] [_name name & name-fx] [_num num & num-fx]] args]
             (update acc 'pin (fnil conj [])
                     {'electrical e
                      'graphical g
                      'at pos
                      'length len
                      'name (kicad->edn* {'text name} name-fx)
                      'number (kicad->edn* {'text num} num-fx)}))
           
           fp_text
           (let [[type text & rest] args]
             (->> rest
                  (kicad->edn* {'draw 'fp_text 'type (symbol type) 'text text})
                  (update acc 'draw (fnil conj []))))

           text
           (let [[text & rest] args]
             (->> rest
                  (kicad->edn* {'draw 'text 'text text})
                  (update acc 'draw (fnil conj []))))
           
           
           (fp_line fp_arc fp_poly fp_rect fp_circle fp_curve
                    polyline circle rectangle
                    )
           (->> args
                (kicad->edn* {'draw f} )
                (update acc 'draw (fnil conj [])))
           
           pts
           (assoc acc 'pts (mapv (comp vec next) args))
           
           
           property
           (let [[pkey pval & rest] args]
             (->> rest
                  (kicad->edn* {'key pkey 'value pval})
                  (update acc 'property (fnil conj []))))
           
           model
           (let [[file & body] args]
             (assoc acc f (kicad->edn* {'file file} body)))
           
           symbol
           (let [[sym-id & rest] args]
             (->> rest
                  (kicad->edn* {'id sym-id})
                  (update acc 'symbol (fnil conj []))))
           
           ;; scalar or e.g. coordinates
           (if (= 2 (count b))
             (assoc acc f (first args))
             (assoc acc f (next b) #_(subvec b 1)))))))
   iacc
   body))

(defn kicad->edn
  [[footprint fpname & body]]
  (when (not= footprint 'footprint)
    (throw (ex-info "not a footprint" {:f footprint})))
  (kicad->edn* {'name fpname} body))

(defn symbol->edn
  [[symbol sname & body]]
  (when (not= symbol 'symbol)
    (throw (ex-info "not a symbol" {:s symbol})))
  (kicad->edn* {'name sname} body))

(defn footprint->sexp*
  [root init key-order]
  (reduce
   (fn [acc k]
     (if-not (contains? root k)
       acc
       (let [v (get root k)]
         (conj acc (if-not (coll? v)
                     [k v]
                     (into [k] v))))))
   init
   key-order))

(defn footprint->sexp
  [root]
  (into
   (footprint->sexp* root
                     ['footprint (root 'name)]
                     '[version generator layer tedit descr tags path
                       autoplace_cost90 autoplace_cost180
                       solder_mark_margin solder_paste_margin solder_paste_ratio
                       clearance zone_connect thermal_width thermal_gap
                       attr])
   (concat
    (for [d    (root 'draw)
          :let [f (d 'draw)]]
      (case f
        fp_text   (into (footprint->sexp* d [f (d 'type) (d 'text)] '[at layer])
                        (let [{:syms [hide effects]} d]
                          (concat
                           (when hide '[hide])
                           [(into ['effects (footprint->sexp* (effects 'font) '[font] '[size thickness])]
                                  (when-some [just (effects 'justify)]
                                    [['justify just]]))
                            ['tstamp (d 'tstamp)]])))
        fp_line   (footprint->sexp* d [f] '[start end layer width tstamp])
        fp_rect   (footprint->sexp* d [f] '[start end layer width fill tstamp])
        fp_circle (footprint->sexp* d [f] '[center end layer width fill tstamp])
        fp_arc    (footprint->sexp* d [f] '[start mid end layer width fill tstamp])
        ;; fp_poly
        
        (println "Unknown draw" d)))
    (for [p (root 'pad)]
      (footprint->sexp* p
                        ['pad (p 'number) (p 'type) (p 'shape)]
                        '[at size drill layers property
                          roundrect_rratio chamfer_ratio chamfer
                          net
                          tstamp
                          pinfunction pintype
                          die_length solder_mask_margin solder_paste_margin
                          solder_paste_margin_ratio clearance zone_connect
                          thermal_width thermal_gap
                          ;; custom pad options & primitives
                          ]))
    (when-some [{:syms [offset scale rotate] :as model} (root 'model)]
      [['model (model 'file)
        ['offset (into ['xyz] (offset 'xyz))]
        ['scale  (into ['xyz] (scale  'xyz))]
        ['rotate (into ['xyz] (rotate 'xyz))]]]
      #_[(footprint->sexp* model ['model (model 'file)] '[offset scale rotate])]))))


#?(:clj
   (def kicad-decimal-format #_(DecimalFormat. "0.######")
     (DecimalFormat. "0.########")))

(defn print-as-sexp
  [top]
  (loop [[a & more] [top ::end]
         stack      []
         acc        ""]
    (cond
      (= a ::end) acc
      (nil? a)    (recur (peek stack) (pop stack) (str acc ")\n"))
      (number? a) (recur more stack #?(:cljs (str acc " " a)
                                       :clj (str acc " "
                                                 (.format kicad-decimal-format a))))
      (string? a) (recur more stack (str acc " " (pr-str a)))
      (symbol? a) (recur more stack (str acc " " a))
      (uuid? a)   (recur more stack (str acc " " a))
      (coll? a)   (let [[f & args] a]
                    (recur args (conj stack more)
                           (str acc
                                (subs "                    " 0 (count stack))
                                "(" f)))
      
      :else (println "What's this?" a))))



(defn grid-of
  [root nx ny xs ys]
  (letfn [(translate [o k dx dy]
            (update o k
                    (fn [[x y]]
                      [(+ x dx)
                       (+ y dy)])))]
   (assoc root
          'pad (vec
                (for [i (range nx)
                      j (range ny)
                      p (root 'pad)]
                  (-> p
                      (update 'number str "_" i "_" j)
                      (translate 'at (* i xs) (* j ys)))))
          'draw (vec (for [i (range nx)
                           j (range ny)
                           :let [dx (* i xs)
                                 dy (* j ys)]
                           {:syms [draw start end text] :as d} (root 'draw)]
                       (case draw
                         fp_text (translate d 'at dx dy)
                         fp_line (-> d
                                     (translate 'start dx dy)
                                     (translate 'end dx dy))
                         fp_arc (-> d
                                    (translate 'start dx dy)
                                    (translate 'mid dx dy)
                                    (translate 'end dx dy))
                         (println "Do not understand this drawing:" draw)))))))



(defn edn-reader-hack
  [kstr]
  (clojure.edn/read-string
   (-> kstr
       (string/replace #"([0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12})" "#uuid \"$1\"")
       (string/replace #"tedit ([0-9a-f]+)" "tedit 0x$1"))))

#?(:clj
   (defn test-roundtrip
     [s]
     (let [pimpl edn-reader-hack #_(comp first dsn/dsnparse)
           parsed (->> s pimpl kicad->edn)
           printed (print-as-sexp (footprint->sexp parsed))
           reparsed (->> printed pimpl kicad->edn)
           p (fn [z]
               (dissoc z 'tedit 'zone))
           equal? (= (p parsed) (p reparsed))]
       (or equal?
           (do (spit "rt.printed.edn" printed)
               (spit "rt.parsed.edn" (with-out-str (clojure.pprint/pprint parsed)))
               (spit "rt.reparsed.edn" (with-out-str (clojure.pprint/pprint reparsed)))
               false)))))

#?(:clj
   (defn test-symbol-roundtrip
     [s]
     (let [pimpl edn-reader-hack #_(comp first dsn/dsnparse)
           parsed (->> s pimpl symbol->edn)
           printed (print-as-sexp (footprint->sexp parsed))
           reparsed (->> printed pimpl symbol->edn)
           p (fn [z]
               (dissoc z 'tedit 'zone))
           equal? (= (p parsed) (p reparsed))]
       (or equal?
           (do (spit "rt.printed.edn" printed)
               (spit "rt.parsed.edn" (with-out-str (clojure.pprint/pprint parsed)))
               (spit "rt.reparsed.edn" (with-out-str (clojure.pprint/pprint reparsed)))
               false)))))

#_(let [df (doto (DecimalFormat. "0.######"))]
    (.format df 0.234))

(def exclude-from-test
  #{
    ;; zones
    "SW_SPST_SKQG_WithoutStem.kicad_mod" 
    "SW_SPST_SKQG_WithStem.kicad_mod"    
    "Connector_SFP_and_Cage.kicad_mod"   
    ;; 
    
    })

(comment
  (clojure.pprint/pprint
   (->> (slurp "c:/Program Files/KiCad/6.0/share/kicad/footprints/Connector.pretty/Tag-Connect_TC2030-IDC-FP_2x03_P1.27mm_Vertical.kicad_mod")
        dsn/dsnparse first kicad->edn)))


(comment
  (clojure.pprint/pprint (kicad->edn* {} [(edn-reader-hack (slurp "src/sted/eda/q_npn_cbe.kicad_sym"))]))
  (clojure.pprint/pprint (kicad->edn* {} [(edn-reader-hack (slurp "src/sted/eda/ad630.kicad_sym"))]))
  (clojure.pprint/pprint (kicad->edn* {} [(edn-reader-hack (slurp "src/sted/eda/r_us.kicad_sym"))])))
#_(clojure.pprint/pprint (kicad->edn (edn-reader-hack (slurp "src/sted/eda/ovaltest.kicad_mod"))))


(comment
  (doseq [f (file-seq (io/file "c:/Program Files/KiCad/6.0/share/kicad/footprints/"))]
    (when-not (or (.isDirectory f)
                  (contains? exclude-from-test (.getName f)))
      #_(println (.getName f))
      (when-not (test-roundtrip (slurp f))
        (throw (ex-info "Did not match" {:name (.getName f)
                                         :path (str f)})))))
  
  
  (symbol->edn (edn-reader-hack (slurp "src/sted/eda/r_us.kicad_sym")))
  
  (future
    (println (.getName (Thread/currentThread)))
    (time
     (count
      (dsn/dsnparse
       (slurp
        (io/file "c:/Program Files/KiCad/6.0/share/kicad/symbols/MCU_Microchip_AVR.kicad_sym"))))))
  
  (future
    (println (.getName (Thread/currentThread)))
    (time
     (count
      (edn-reader-hack
       (slurp "c:/Program Files/KiCad/6.0/share/kicad/footprints/Connector.pretty/Tag-Connect_TC2030-IDC-FP_2x03_P1.27mm_Vertical.kicad_mod")
       #_(slurp
          (io/file "c:/Program Files/KiCad/6.0/share/kicad/symbols/MCU_Texas_MSP430.kicad_sym"))))))

  (time
   (count
    (clojure.edn/read-string
     (slurp
      (io/file "c:/Program Files/KiCad/6.0/share/kicad/symbols/MCU_Microchip_AVR.kicad_sym")))))
  
  
  (test-roundtrip
   (slurp
    )))


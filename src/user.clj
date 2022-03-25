(ns user
  (:require
   [shadow.cljs.devtools.api :as shadow]
   [shadow.cljs.devtools.server :as server]
   [nrepl.server :as nrepl-server]
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [clojure.walk :as walk]
   [clojure.string :as string]))

(defonce shadow-server (future (server/start!)))

(defonce shadow-watch
  (future
    @shadow-server
    (Thread/sleep 200)
    #_(println "Starting watch")
    #_(shadow/watch :br)))

(defonce nrepl-server
  (let [port-file (io/file ".nrepl-port")
        {:keys [port]} (nrepl-server/start-server #_#_:handler cnr/cider-nrepl-handler)]
    (spit ".nrepl-port" port)))

(comment (shadow/watch :br))

(comment
  (shadow/release :elec)
  (shadow/release :br))

(comment
  (do
    (server/stop!)
    (Thread/sleep 223)
    (server/start!))
  (shadow/watch :br)
  (shadow/watch :ptr)
  (do
    (shadow/release :elec)
    (shadow/release :br)))

(defn release-cljs-and-exit
  [& what]
  (future-cancel shadow-watch)
  @shadow-server
  (shadow/release :br)
  (shadow/release :elec)
  (shadow/release :ptr)
  (System/exit 0))

(defn release-cljs!
  []
  (future-cancel shadow-watch)
  @shadow-server
  (shadow/release :br)
  (shadow/release :elec)
  (shadow/release :ptr)
  :ok)

(comment
  (shadow/watch :ptr))

(count
 (slurp
  (str
   "C:/Program Files/KiCad/6.0/share/kicad/symbols/"
   "4xxx.kicad_sym")))

(spit
 "testpattern.bin"
 (let [ns [0 0xff 0xf0 0xff00 0x0f0 0x0fff]
       b (java.nio.ByteBuffer/allocate (* 4 (count ns)))]
   (doseq [n ns]
     (.putInt b n))
   (.array b)))

#_(->>   "4xxx.kicad_sym"
       (str "C:/Program Files/KiCad/6.0/share/kicad/symbols/")
       slurp
       clojure.edn/read-string
       (tree-seq coll? seq)
       next
       (filter (fn [e]
                 (and (list? e) (= 'symbol (first e)))))
       (take 3)
       (run! (fn [[_symbol s & body]]
               (clojure.pprint/pprint
                (into {:symbol s}
                      (for [[p v & vs] body]
                        [p (if-not vs v (into [v] vs))])))
               #_(run! prn e))))


(vec (.split (System/getProperty "java.class.path") ";"))
 ["src"
  "C:\\Users\\rmcq\\.m2\\repository\\datascript\\datascript\\1.3.4\\datascript-1.3.4.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\http-kit\\http-kit\\2.5.3\\http-kit-2.5.3.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\nrepl\\nrepl\\0.8.3\\nrepl-0.8.3.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\babashka\\sci\\0.2.8\\sci-0.2.8.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\clojure\\1.10.1\\clojure-1.10.1.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\rewrite-clj\\rewrite-clj\\1.0.644-alpha\\rewrite-clj-1.0.644-alpha.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\rum\\rum\\0.12.6\\rum-0.12.6.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\thheller\\shadow-cljs\\2.12.2\\shadow-cljs-2.12.2.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\zprint\\zprint\\1.2.0\\zprint-1.2.0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\persistent-sorted-set\\persistent-sorted-set\\0.1.4\\persistent-sorted-set-0.1.4.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\borkdude\\edamame\\0.0.18\\edamame-0.0.18.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\borkdude\\sci.impl.reflector\\0.0.1\\sci.impl.reflector-0.0.1.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\core.specs.alpha\\0.2.44\\core.specs.alpha-0.2.44.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\spec.alpha\\0.2.176\\spec.alpha-0.2.176.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\tools.reader\\1.3.5\\tools.reader-1.3.5.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\cljsjs\\react\\16.8.6-0\\react-16.8.6-0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\cljsjs\\react-dom\\16.8.6-0\\react-dom-16.8.6-0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\cider\\piggieback\\0.5.0\\piggieback-0.5.0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\com\\bhauman\\cljs-test-display\\0.1.1\\cljs-test-display-0.1.1.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\com\\cognitect\\transit-clj\\1.0.324\\transit-clj-1.0.324.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\com\\cognitect\\transit-cljs\\0.8.264\\transit-cljs-0.8.264.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\com\\google\\javascript\\closure-compiler-unshaded\\v20210302\\closure-compiler-unshaded-v20210302.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\com\\wsscode\\pathom\\2.2.31\\pathom-2.2.31.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\expound\\expound\\0.8.5\\expound-0.8.5.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\fipp\\fipp\\0.6.23\\fipp-0.6.23.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\hiccup\\hiccup\\1.0.5\\hiccup-1.0.5.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\clojurescript\\1.10.844\\clojurescript-1.10.844.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\core.async\\1.3.610\\core.async-1.3.610.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\data.json\\1.0.0\\data.json-1.0.0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\google-closure-library\\0.0-20201211-3e6c510d\\google-closure-library-0.0-20201211-3e6c510d.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\google-closure-library-third-party\\0.0-20201211-3e6c510d\\google-closure-library-third-party-0.0-20201211-3e6c510d.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\test.check\\1.1.0\\test.check-1.1.0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\tools.cli\\1.0.194\\tools.cli-1.0.194.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\graalvm\\js\\js\\20.1.0\\js-20.1.0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\graalvm\\js\\js-scriptengine\\20.1.0\\js-scriptengine-20.1.0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\ring\\ring-core\\1.8.1\\ring-core-1.8.1.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\thheller\\shadow-client\\1.3.3\\shadow-client-1.3.3.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\thheller\\shadow-cljsjs\\0.0.21\\shadow-cljsjs-0.0.21.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\thheller\\shadow-undertow\\0.1.0\\shadow-undertow-0.1.0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\thheller\\shadow-util\\0.7.0\\shadow-util-0.7.0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\borkdude\\sci\\0.2.5\\sci-0.2.5.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\javax\\xml\\bind\\jaxb-api\\2.3.1\\jaxb-api-2.3.1.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\com\\cognitect\\transit-java\\1.0.343\\transit-java-1.0.343.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\com\\cognitect\\transit-js\\0.8.861\\transit-js-0.8.861.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\com\\wsscode\\spec-inspec\\1.0.0-alpha2\\spec-inspec-1.0.0-alpha2.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\edn-query-language\\eql\\0.0.9\\eql-0.0.9.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\spec-coerce\\spec-coerce\\1.0.0-alpha6\\spec-coerce-1.0.0-alpha6.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\core.rrb-vector\\0.1.1\\core.rrb-vector-0.1.1.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\tools.analyzer.jvm\\1.1.0\\tools.analyzer.jvm-1.1.0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\com\\ibm\\icu\\icu4j\\66.1\\icu4j-66.1.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\graalvm\\regex\\regex\\20.1.0\\regex-20.1.0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\graalvm\\sdk\\graal-sdk\\20.1.0\\graal-sdk-20.1.0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\graalvm\\truffle\\truffle-api\\20.1.0\\truffle-api-20.1.0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\ow2\\asm\\asm\\7.1\\asm-7.1.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\ow2\\asm\\asm-analysis\\7.1\\asm-analysis-7.1.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\ow2\\asm\\asm-commons\\7.1\\asm-commons-7.1.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\ow2\\asm\\asm-tree\\7.1\\asm-tree-7.1.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\ow2\\asm\\asm-util\\7.1\\asm-util-7.1.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\commons-fileupload\\commons-fileupload\\1.4\\commons-fileupload-1.4.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\commons-io\\commons-io\\2.6\\commons-io-2.6.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\crypto-equality\\crypto-equality\\1.0.0\\crypto-equality-1.0.0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\crypto-random\\crypto-random\\1.2.0\\crypto-random-1.2.0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\ring\\ring-codec\\1.1.2\\ring-codec-1.1.2.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\io\\undertow\\undertow-core\\2.2.4.Final\\undertow-core-2.2.4.Final.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\javax\\activation\\javax.activation-api\\1.2.0\\javax.activation-api-1.2.0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\com\\fasterxml\\jackson\\core\\jackson-core\\2.8.7\\jackson-core-2.8.7.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\msgpack\\msgpack\\0.6.12\\msgpack-0.6.12.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\core.memoize\\1.0.236\\core.memoize-1.0.236.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\tools.analyzer\\1.0.0\\tools.analyzer-1.0.0.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\commons-codec\\commons-codec\\1.11\\commons-codec-1.11.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\jboss\\logging\\jboss-logging\\3.4.1.Final\\jboss-logging-3.4.1.Final.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\jboss\\threads\\jboss-threads\\3.1.0.Final\\jboss-threads-3.1.0.Final.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\jboss\\xnio\\xnio-api\\3.8.0.Final\\xnio-api-3.8.0.Final.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\jboss\\xnio\\xnio-nio\\3.8.0.Final\\xnio-nio-3.8.0.Final.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\com\\googlecode\\json-simple\\json-simple\\1.1.1\\json-simple-1.1.1.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\javassist\\javassist\\3.18.1-GA\\javassist-3.18.1-GA.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\core.cache\\1.0.207\\core.cache-1.0.207.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\wildfly\\client\\wildfly-client-config\\1.0.1.Final\\wildfly-client-config-1.0.1.Final.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\wildfly\\common\\wildfly-common\\1.5.2.Final\\wildfly-common-1.5.2.Final.jar"
  "C:\\Users\\rmcq\\.m2\\repository\\org\\clojure\\data.priority-map\\1.0.0\\data.priority-map-1.0.0.jar"]






(let [s (slurp "/mnt/c/users/rmcq/Desktop/clojure/struc/linkdump")]
  (reduce
   (fn [s [m rep]]
     (.replaceAll s m rep))
   s
   [["\[1\]" "<a href=\"http://pepijndevos.nl/2019/07/18/vhdl-to-pcb.html\">{{{1}}}</a>"]
    ["\[2\]" "<a href=\"https://github.com/YosysHQ/yosys\">{{{2}}}</a>"]
    ["\[3\]" "<a href=\"https://freerouting.org/\">{{{3}}}</a>"]
    ["\[4\]" "<a href=\"https://cdn.hackaday.io/files/1666717130852064/specctra.pdf\">{{{4}}}</a>"]
    ["\[5\]" "<a href=\"https://www.kicad.org/\">{{{5}}}</a>"]
    ["\[6\]" "<a href=\"https://www.ucamco.com/files/downloads/file_en/451/gerber-layer-format-specification-revision-2021-11_en.pdf\">{{{6}}}</a>"]
    ["\[7\]" "<a href=\"rder PCB fabs - oshpark.com, pcbway.com, jlcpcb.com, etc\">{{{7}}}</a>"]
    ["\[8\]" "<a href=\"https://qmk.fm/\">{{{8}}}</a>"]
    ["\[9\]" "<a href=\"https://config.qmk.fm/#/handwired/heisenberg/LAYOUT_planck_mit\">{{{9}}}</a>"]
    ["\[10\]" "<a href=\"https://docs.qmk.fm/#/newbs_building_firmware_configurator\">{{{10}}}</a>"]
    ["\[11\]" "<a href=\"https://github.com/evyd13/plain60-c\">{{{11}}}</a>"]
    ["\[12\]" "<a href=\"https://github.com/adereth/dactyl-keyboard\">{{{12}}}</a>"]
    ["\[13\]" "<a href=\"https://old.reddit.com/r/MechanicalKeyboards/\">{{{13}}}</a>"]
    ["\[14\]" "<a href=\"https://github.com/qmk/keyboard_awesome\">{{{14}}}</a>"]
    ["\[15\]" "<a href=\"https://github.com/qmk/qmk_firmware/pull/15872/files\">{{{15}}}</a>"]
    ["\[16\]" "<a href=\"https://github.com/qmk/qmk_toolbox\">{{{16}}}</a>"]
    ["\[17\]" "<a href=\"https://github.com/pepijndevos/74xx-liberty/tree/devel\">{{{17}}}</a>"]
    ["\[18\]" "<a href=\"https://github.com/pepijndevos/74xx-liberty/blob/devel/synth_74.ys\">{{{18}}}</a>"]
    ["\[19\]" "<a href=\"http://yosyshq.net/yosys/documentation.html\">{{{19}}}</a>"]
    ["\[20\]" "<a href=\"http://www.vlsitechnology.org/html/libraries.html\">{{{20}}}</a>"]
    ["\[21\]" "<a href=\"https://people.eecs.berkeley.edu/~alanmi/abc/\">{{{21}}}</a>"]
    ["\[22\]" "<a href=\"https://github.com/pepijndevos/74xx-liberty/blob/devel/kicad/generate_netlist.py\">{{{22}}}</a>"]
    ["\[23\]" "<a href=\"http://bygone.clairexen.net/yosys/cmd_write_json.html\">{{{23}}}</a>"]
    ["\[24\]" "<a href=\"https://buildmedia.readthedocs.org/media/pdf/skidl/latest/skidl.pdf\">{{{24}}}</a>"]
    ["\[25\]" "<a href=\"https://github.com/pepijndevos/74xx-liberty/blob/devel/kicad/parts.py\">{{{25}}}</a>"]
    ["\[26\]" "<a href=\"https://kicad.github.io/symbols/\">{{{26}}}</a>"]
    ["\[27\]" "<a href=\"ymbols/-/blob/master/74xx.kicad_sym\">{{{27}}}</a>"]
    ["\[28\]" "<a href=\"https://gridbyexample.com/examples/\">{{{28}}}</a>"]
    ["\[29\]" "<a href=\"https://commons.wikimedia.org/wiki/SVG_examples\">{{{29}}}</a>"]
    ["\[30\]" "<a href=\"https://github.com/jaseg/gerbolyze\">{{{30}}}</a>"]
    ["\[31\]" "<a href=\"https://digitaljs.tilk.eu/     \">{{{31}}}</a>"]
    ["\[32\]" "<a href=\"https://github.com/kieler/elkjs\">{{{32}}}</a>"]
    ["\[33\]" "<a href=\"https://www.eclipse.org/elk/\">{{{33}}}</a>"]
    ["\[34\]" "<a href=\"https://github.com/pepijndevos/74xx-liberty/blob/devel/benchmarks/VexRiscv.v\">{{{34}}}</a>"]
    ["\[36\]" "<a href=\"https://store.steampowered.com/app/504210/SHENZHEN_IO/\">{{{36}}}</a>"]
    ["\[37\]" "<a href=\"https://opensource.adobe.com/dc-acrobat-sdk-docs/standards/pdfstandards/pdf/PDF32000_2008.pdf\">{{{37}}}</a>"]
    ["\[38\]" "<a href=\"https://github.com/mozilla/pdf.js\">{{{38}}}</a>"]
    ["\[39\]" "<a href=\"https://datasheets.raspberrypi.com/rp2040/rp2040-datasheet.pdf\">{{{39}}}</a>"]]))

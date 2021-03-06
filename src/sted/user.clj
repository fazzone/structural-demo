(ns sted.user)


(let [ch (->chain sel)
      file (or (:chain/filename ch) "noname.clj")]
  (a/let [res (.writeFile (js/secret_electron_require "fs/promises") file (e/->string ch))]
    [:wrote file]))


(defn open-file
  [path]
  (a/let [buf (.readFile (js/secret_electron_require "fs/promises") path)]
    (send! [:open-chain (.toString buf) {:chain/filename path}])))


(open-file "src/sted/comp/code.cljc")


(open-file "src/sted/comp/scroll.cljs")


(a/let [resp (js/fetch "https://raw.githubusercontent.com/mdn/content/main/files/en-us/web/javascript/reference/global_objects/promise/index.md")
        text (.text resp)]
  (send! [:ingest-markdown text]))


(transact! [{:db/id (:db/id (first (:coll/_contains sel))) :chain/filename "src/sted/user.clj"}])


hn/stories


(ingest (keep (fn [[a b]]
                (when-some [args (:arglists (meta b))] (list* a args)))
              (get-in (deref (:env (deref (datafy *ns*))))
                      [:namespaces 'user])))


(defn open-url
  [u]
  (a/let [text (fetch-text u)]
    (send! [:open-chain text])))


(open-url "https://raw.githubusercontent.com/fazzone/structural-demo/master/src/sted/page.cljs")


(count (js/JSON.stringify (d/freeze (d/entity-db sel))))


(a/let [resp (js/fetch "https://www.wikidata.org/w/api.php?action=wbgetentities&languages=en&ids=Q42&format=json&origin=*")
        data (.json resp)]
  (ingest (js->clj data)))


(meta (var open-file))


(:arglists (meta (var get-repo)))


(ingest (for [el (dom/qsa ":hover")]
          [(.toLowerCase (.-tagName el))
           (js->clj (js/Array.from (.-classList el)))]))


(send! [:transact! [{:db/id 2 :my/attr "Whatever"}]])


(meta (var slurp))


(let [scope "foo/"]
  (a/let [prev (.getRegistration js/navigator.serviceWorker scope)
          unreg (when prev (.unregister prev))
          reg (.register js/navigator.serviceWorker "sw.js" #js {:scope scope})]
    (js/console.log "Registration is" reg)
    (js->clj reg)))


(js/fetch "/foo/bar")


#_(.register js/navigator.serviceWorker "sw.js" #js {:scope "dingus"})


(let [ws (js/WebSocket. "ws://localhost:9102")]
  (doto ws
    (.addEventListener "open" (b/encode {:op :eval :code "(+ 1 2 3)"}))))


(defn open-file
  [path]
  (then (slurp path)
        (fn [text]
          (send! [:open-chain text {:chain/filename path}]))))


(if (or (= :nav action)
        (some-> r meta (get `p/nav)))
  (core/send! bus [:ingest-result eval-target r])
  (core/send! bus [:eval-result eval-target (str r) print-output]))


(new-window "http://localhost:8087?title=SH"
            "_blank"
            "top=500,left=3000")


(list (list 1 2 3) (list 1 2 3) (list 1 2 3) (list 1 2 3) (list 1 2 3))


(defn get-repo [ident ref-name]
  (a/let [ref (fetch-json (str "https://api.github.com/repos/" ident "/git/ref/heads/" ref-name))
          commit (fetch-json (-> ref :object :url))
          tree (fetch-json (str (-> commit :tree :url) "?recursive=true"))]
    (ingest tree)))


(get-repo "fazzone/structural-demo" "master")


(ls)


(defn el->h [el] (cond-> [(clojure.string/lower-case (.-tagName el))] (.hasAttributes el)))


(defn explore-tree
  [{:keys [tree]  :as t}]
  (let [by-type (group-by :type tree)]
    (a/let [rec (js/Promise.all
                 (into-array
                  (for [{:keys [url path] :as st} (by-type "tree")]
                    (a/let [f (fetch-json url) sf (explore-tree f)] [path sf]))))]
      (into (sorted-map)
            (concat (for [{:keys [path] :as b} (by-type "blob")] [path b])
                    rec)))))


(a/let [res (get-repo "fazzone/structural-demo" "master")
        load-all (js/Promise.all
                  (clj->js
                   (vec (for [{:keys [path]} (:tree (first res))
                              :when (or (clojure.string/ends-with? path ".cljc")
                                        (clojure.string/ends-with? path ".clj")
                                        (clojure.string/ends-with? path ".cljs"))]
                          (str "https://raw.githubusercontent.com/fazzone/structural-demo/master/" path)))))]
  {:ok (count load-all)})


(let [fz (d/freeze (d/entity-db sel))
      ps (js/Object.getOwnPropertyNames fz)]
  (doseq [p ps]
    (.setItem js/window.localStorage p (aget fz p)))
  (js->clj ps))


(let [b (js/Blob. (clj->js [(js/JSON.stringify (d/freeze (d/entity-db sel)) nil 2)])
                  (clj->js {:type "application/json"}))
      u (js/URL.createObjectURL b)
      a (js/document.createElement "a")]
  (set! (.-href a) u)
  (set! (.-download a) "file.txt")
  (.click a))


(let [b (js/Blob. (clj->js [(js/JSON.stringify (d/freeze (d/entity-db sel)) nil 2)])
                  (clj->js {:type "application/json"}))
      u (js/URL.createObjectURL b)
      a (js/document.createElement "a")]
  (js/window.open u))


(a/let [resp (js/fetch "https://raw.githubusercontent.com/fazzone/structural-demo/dbgz/srv/db.json")
        json (.json resp)]
  (js/sted.page.become (d/thaw json)))


(a/let [resp (js/fetch "https://raw.githubusercontent.com/clojure-emacs/clojuredocs-export-edn/master/exports/export.edn")
        text (.text resp)]
  (def cljdocs (clojure.edn/read-string text)))
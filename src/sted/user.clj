(ns sted.user)


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

(defn open-file [path]
  (then
   (.readFile (js/require "fs/promises") path)
   (fn [buf]
     (send! [:open-chain (.toString buf) {:chain/filename path}]))))

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

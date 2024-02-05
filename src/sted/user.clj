(ns sted.user)

;; eval with `e`
(+ 1 1)

;; test stacktraces when you eval exceptions
(do
  (defn f [] (throw (ex-info "f" {:where 'f})))
  (defn g [] (f))
  (defn h [] (g))
  (h))

;; demo of nav/eval support - eval with `e` to nav
;; indent with tab
hn/stories

;; open other files
;; note - eval this defn with `e` before calling it
(defn open-url
  [u]
  (a/let [text (fetch-text u)]
    (send! [:open-chain text])))

(open-url "https://raw.githubusercontent.com/fazzone/structural-demo/master/src/sted/page.cljs")

;; demo of silly dom interop
(ingest (for [el (dom/qsa ":hover")]
          [(.toLowerCase (.-tagName el))
           (js->clj (js/Array.from (.-classList el)))]))

;; transact random stuff
[transact! [{:db/id 2 :my/attr "Whatever"}]]

;; read it back
(-> (d/entity-db sel) (d/entity 2) :my/attr)

;; play with github api
(defn get-repo [ident ref-name]
  (a/let [ref (fetch-json (str "https://api.github.com/repos/" ident "/git/ref/heads/" ref-name))
          commit (fetch-json (-> ref :object :url))
          tree (fetch-json (str (-> commit :tree :url) "?recursive=true"))]
    tree))

;; raw response
;; use tab to indent, `e` to expand ellipses
(a/let [res (get-repo "fazzone/structural-demo" "master")]
  (ingest res))

;; list of blobs
(a/let [[res] (get-repo "fazzone/structural-demo" "master")]
  (for [{:keys [path type]} (:tree res)
        :when (= type "blob")]
    path))

;; save db to file
(let [b (js/Blob. (clj->js [(js/JSON.stringify (d/freeze (d/entity-db sel)) nil 2)])
                  (clj->js {:type "application/json"}))
      u (js/URL.createObjectURL b)
      a (js/document.createElement "a")]
  (set! (.-href a) u)
  (set! (.-download a) "sted.db.json")
  (.click a))

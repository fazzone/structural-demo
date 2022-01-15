(ns hk
  (:require
   [org.httpkit.server :as hk]
   [ring.util.response :as ring-resp]
   [ring.middleware.params :as rm-p]
   [ring.middleware.keyword-params :as rm-kp]
   [clojure.java.io :as io]
   [clojure.string :as string])
  (:import
   [java.io PushbackReader]))

(defn read-all
  [ins]
  (let [rdr (-> ins io/reader (PushbackReader.))]
    ((fn iter []
       (when (.ready rdr)
         (some-> (read rdr)
                 (cons (lazy-seq (iter)))))))))

(defn handler
  [{:keys [request-method uri] :as req}]
  (prn request-method uri (keys req))
  (case uri
    "/save"
    (let [subtree (io/file "subtree")
          outf (io/file subtree (get (:query-params req) "file"))]
      (.mkdirs (.getParentFile outf))
      (io/copy (:body req) outf)
      {:status 200 :body (str "wrote " (str outf))})
    (or (ring-resp/file-response (str "srv" uri))
        {:status 404 :body "not found"})))

(def my-app
  (-> #'handler
      (rm-kp/wrap-keyword-params)
      (rm-p/wrap-params)))

(defonce servers (atom []))

(do @servers)

(defn start-dev
  []
  (doseq [s @servers] (s))
  (reset! servers [])
  (swap! servers conj
         (hk/run-server
          my-app
          {:port 9102})))

(comment (start-dev))
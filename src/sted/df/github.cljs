(ns sted.df.github
  (:require [clojure.core.protocols :refer [nav]])
  (:require [sted.df.async :as m]))

(def root "https://hacker-news.firebaseio.com/v0")

(def item-doc
  {:id          "The item's unique id."
   :deleted     "true if the item is deleted."
   :type        "The type of item. One of \"job\", \"story\", \"comment\", \"poll\", or \"pollopt\"."
   :by          "The username of the item's author."
   :time        "Creation date of the item, in Unix Time."
   :text        "The comment, story or poll text. HTML."
   :dead        "true if the item is dead."
   :parent      "The comment's parent: either another comment or the relevant story."
   :poll        "The pollopt's associated poll."
   :kids        "The ids of the item's comments, in ranked display order."
   :url         "The URL of the story."
   :score       "The story's score, or the votes for a pollopt."
   :title       "The title of the story, poll or job. HTML."
   :parts       "A list of related pollopts, in display order."
   :descendants "In the case of stories or polls, the total comment count."})

(def user-doc
  {:id        "The user's unique username. Case-sensitive. Required."
   :delay     "Delay in minutes between a comment's creation and its visibility to other users."
   :created   "Creation date of the user, in Unix Time."
   :karma     "The user's karma."
   :about     "The user's optional self-description. HTML."
   :submitted "List of the user's stories, polls and comments."})

(defn fetch-json [url]
  (-> (js/fetch url)
      (.then #(.json %))
      (.then #(js->clj % :keywordize-keys true))))

(defn as-url [s] (js/URL. s))

(defn as-date [^long timestamp] (js/Date. timestamp))

(defn fetch-hn [path]
  (m/let [url   (as-url (str root path))
          res   (fetch-json url)
          item  (with-meta res {:hacker-news/api-url url})]
    (if-not (map? item)
      item
      (case (:type item)
        "story"   (apply array-map
                       (mapcat (juxt identity item)
                               [:type :title :url :score :by :id
                                :time :descendants
                                :kids]))
        "comment" (apply array-map
                         (mapcat (juxt identity item)
                                 [:type :by :id :parent :time :text ])))
      
      #_(cond-> item
          (contains? item :url)
          (update :url #(as-url %))
          (contains? item :time)
          (update :time #(as-date (* % 1000)))
          (contains? item :created)
          (update :created #(as-date (* % 1000)))
          (contains? item :type)
          (update :type keyword)))))

(declare nav-hn)

(defn fetch-user [user]
  (m/let [res (fetch-hn (str "/user/" user ".json"))]
    (vary-meta res assoc `nav #'nav-hn)))

(defn nav-item [_coll _k v]
  (println "NAVitem" v)
  (m/let [res (fetch-hn (str "/item/" v ".json"))]
    (println "The res!!!")
    (vary-meta res assoc `nav #'nav-hn)))

(def stories
  (with-meta
    #{:topstories :newstories :beststories
      :askstories :showstories :jobstories}
    {`nav #'nav-hn}))

(defn fetch-stories [type]
  (println "Fstories")
  (m/let [res (fetch-hn (str "/" (name type) ".json"))]
    (vary-meta (take 15 res) assoc `nav #'nav-item)))

(defn nav-hn [coll k v]
  (println "NAv HN" k v 'in coll)
  (cond
    (stories v)             (fetch-stories v)
    (keyword? v)            (get (if (contains? coll :type)
                                   item-doc
                                   user-doc) v v)
    (= k :by)               (fetch-user v)
    (= k :parent)           (nav-item coll k v)
    (#{:kids :submitted} k) (vary-meta v assoc `nav #'nav-item)
    :else v))

;; Do not understand why these cannot be required or imported normally
(def fsp (js/require "fs/promises"))
(def path (js/require "path"))

(defrecord File [f])

#_(defn datafy-path
  [{:keys [f]}]
  {:name (.basename path f)
   :path f})

(defn nav-fs
  [c k v]
  (println "NAv-fs" k v "in" c)
  )

(declare ls)

#_(defn datafy-stat
  [abs st]
  (m/let [des (when (.isDirectory st)
                (.readdir fsp abs  #js {:withFileTypes true}))]
    (cond-> {:path abs :name (.basename path abs)}
      des        (assoc :files (with-meta (->> des
                                               (mapv (fn [de]
                                                       (with-meta
                                                         ((if (.isDirectory de)
                                                            list
                                                            vector)
                                                          (.-name de))
                                                         {:name (.-name de)
                                                          `nav (fn []
                                                                 (println "AAAA" (.-name de))
                                                                 (ls (.join path abs (.-name de)))
                                                                 #_(.stat fsp (.join path abs (.-name de))))}))))
                                 {`nav #'nav-fs}
                                 #_{:coll/type :grid
                                    :coll/data-type :vec}))
      (nil? des) (assoc :mode (.-mode st) :size (.-size st))
      true (vary-meta assoc `nav #'nav-fs))))
(defn datafy-stat
  [p n st]
  {:name n
   :abs ()
   :mode (.-mode st)
   :size (.-size st)}  
  )

(defn uhh
  [p n]
  (let [j (.join path p n)]
    (m/let [st (.stat fsp j)]
      (if-not (.isDirectory st)
        j
        (m/let [des (.readdir fsp j #js {:withFileTypes true})]
          (with-meta {n (->> des
                             (mapv (fn [de]
                                     (cond-> (.-name de)
                                       (.isDirectory de) (str "/")))))}
            {`nav (fn [_ k _]
                    (println "Sub-nav" k)
                    (cond (= 'pwd k) j
                          :else (uhh j k )))})))))
  
  #_(cond-> {:path abs :name (.basename path abs)}
      des        (assoc :files (with-meta (->> des
                                               (mapv (fn [de]
                                                       (with-meta
                                                         ((if (.isDirectory de)
                                                            list
                                                            vector)
                                                          (.-name de))
                                                         {:name (.-name de)
                                                          `nav (fn []
                                                                 (println "AAAA" (.-name de))
                                                                 (ls (.join path abs (.-name de)))
                                                                 #_(.stat fsp (.join path abs (.-name de))))}))))
                                 {`nav #'nav-fs}
                                 #_{:coll/type :grid
                                    :coll/data-type :vec}))
      (nil? des) (assoc :mode (.-mode st) :size (.-size st))
      true (vary-meta assoc `nav #'nav-fs)))



(defn ls
  ([] (ls "."))
  ([p]
   (let [abs (.resolve path p)]
     (uhh (.dirname path abs) (.basename path abs)))))

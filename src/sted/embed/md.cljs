(ns sted.embed.md
  (:require
   [sted.embed.common :as ec]
   [mdast-util-from-markdown :as mdast]
   ["mdast-util-frontmatter" :as mdast-ufm]
   ["micromark-extension-frontmatter" :as mm-efm]
   ["js-yaml" :as jsyaml]
   [goog.object :as gobj]))


(defn md->tx*
  [me]
  (if-not (object? me)
    (do (js/console.log "What are you" me) {:what me})
    (let [type     (gobj/get me "type")
             
          value    (gobj/get me "value")
          children (gobj/get me "children")]
      (letfn [(coll-tx [coll-type xs]
                (let [id (ec/new-tempid)]
                  (cond-> {:db/id id  :coll/type coll-type}
                    (seq xs) (merge
                              (ec/seq-tx (for [x xs]
                                           (assoc (md->tx* x)
                                                  :coll/_contains id)))))))]
        (case type
          "root"       (coll-tx :md/root children)
          "paragraph"  (coll-tx :md/para children)
          "list"       (coll-tx :md/list children)
          "listItem"   (coll-tx :md/li children)
          "text"       {:token/type :md/text :token/value value}
          "inlineCode" {:token/type :md/inline-code :token/value value}
          "code"       {:token/type :md/code :token/value value}
          "heading"    (coll-tx :md/heading children)
          "strong"     (coll-tx :md/strong children)
          "emphasis"   (coll-tx :md/emphasis children)
          "link"       (coll-tx :md/link children)
          "blockquote" (coll-tx :md/blockquote children)
          ;; "link" (let [title (gobj/get me "title")
          ;;              url (gobj/get me "url")]
          ;;          {:token/type :md/link
          ;;           :token/value (or title url)
          ;;           :link/href url})
             
          {:token/type :verbatim :token/value (pr-str me)})))))


(defn go
  [text]
  (let [t (-> text
              (mdast/fromMarkdown
               #js{:extensions #js [(mm-efm/frontmatter "yaml")]
                   :mdastExtensions #js [(mdast-ufm/frontmatterFromMarkdown "yaml")]}))]
    (-> t
        (.-children)
        (aget 0)
        (.-value)
        jsyaml/load)))

(defn parse
  [md-text]
  (mdast/fromMarkdown md-text))

(defn md->tx [md-text]
  (let [ast (parse md-text)]
    (md->tx* ast)))



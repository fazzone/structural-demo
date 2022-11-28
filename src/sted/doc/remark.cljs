(ns sted.doc.remark
  (:require
   [mdast-util-from-markdown :as mdast]
   ["mdast-util-frontmatter" :as mdast-ufm]
   ["micromark-extension-frontmatter" :as mm-efm]
   ["js-yaml" :as jsyaml]))


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

#_(defn parse
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

(ns sted.comp.modeline
  (:require
   [datascript.core :as d]
   [rum.core :as rum]
   #_[sted.comp.edit-box]
   [sted.core :as core]
   [sted.comp.common :as cc]
   
   [sted.sys.eval.sys-eval :as se]
   [goog.string :as gstring]))

(def save-status (atom nil))

(def kick (atom 0))

#_(rum/defc modeline-inner < rum/reactive
  [sel bus rec]
  (let [db (d/entity-db sel)]
    (rum/fragment
     [:span.modeline-content.mlc {}
      [:span {:style {:margin-left "1ex"}}
       [:span.mlgloss {} "eid"]
       (str (:db/id sel))]
      [:span {}
       [:span.mlgloss {} "tx"]
       (str (.toString (:max-tx db) 16))]
      
      [:span {}
       [:span.mlgloss {} "type"]
       (str (or (:token/type sel) (:coll/type sel)))]
      

      (when-let [v (some-> sel :nav/pointer meta)]
        [:span {} [:span.mlgloss {} "nav/pointer"] (str v)])

      (when-let [v (:db/ident sel)]
        [:span {} [:span.mlgloss {} "ident"] (str v)])

      (when-let [v (loop [s sel]
                     (or (:chain/filename s)
                         (some->> s :coll/_contains core/exactly-one recur)))]
        
        [:span {} [:span.mlgloss {} "file"] (str v)])

      (when-let [v (loop [s sel]
                     (or (:eval/action s)
                         (some->> s :coll/_contains core/exactly-one recur)))]
        [:span {} [:span.mlgloss {} "action"] (str v)])
      
      (when-let [v (:handle/token sel )]
        [:span {} [:span.mlgloss {} "handle"] (str v)])

      (when-let [v (:eval/of sel)]
        [:span {} [:span.mlgloss {} "eval/of"] (str (:db/id v))])])))


(rum/defc modeline-inner < rum/reactive
  [sel bus rec]
  (let [db (d/entity-db sel)
        nav-pointer (some-> sel :nav/pointer meta)
        db-ident (:db/ident sel)
        handle-token (:handle/token sel)
        eval-of (:eval/of sel)
        chain-filename (loop [s sel]
                         (or (:chain/filename s)
                             (some->> s :coll/_contains core/exactly-one recur)))
        eval-action (loop [s sel]
                      (or (:eval/action s)
                          (some->> s :coll/_contains core/exactly-one recur)))]
    (rum/fragment
     [:span.modeline-content.mlc {}
      [:span {:style {:margin-left "1ex"}}
       [:span.mlgloss {} "eid"]
       (str (:db/id sel))]
      
      #_[:span {} [:span.mlgloss {} "tx"] (str (.toString (:max-tx db) 16))]
      
      [:span {}
       [:span.mlgloss {} "type"]
       (str (or (:token/type sel) (:coll/type sel)))]
      
      (when nav-pointer
        [:span {} [:span.mlgloss {} "nav/pointer"] (str nav-pointer)])
      
      (when db-ident
        [:span {} [:span.mlgloss {} "ident"] (str db-ident)])
      
      (when chain-filename
        [:span {} [:span.mlgloss {} "file"] (str chain-filename)])
      
      (when eval-action
        [:span {} [:span.mlgloss {} "action"] (str eval-action)])
      
      (when handle-token
        [:span {} [:span.mlgloss {} "handle"] (str handle-token)])
      
      (when eval-of
        [:span {} [:span.mlgloss {} "eval/of"] (str (:db/id eval-of))])])))


(rum/defc modeline-nest-next < rum/reactive
  [sel bus rec]
  (let [ k (rum/react kick)]
    (rum/with-context [my-ref cc/*modeline-ref*]
      (some->> my-ref
               (rum/deref)
               (rum/portal (modeline-inner sel bus rec))))))

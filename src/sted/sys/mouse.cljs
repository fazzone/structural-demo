(ns sted.sys.mouse
  (:require [sted.core :as core]))

(defn mousedown
  [^MouseEvent ev st bus]
  (cond
    (= 3 (.-buttons ev))
    (do (swap! st assoc :mousechord true)
        true)
    
    (< 1 (.-detail ev))
    (do
      (swap! st assoc :detail (.-detail ev))
      false)
    #_(.contains (.-classList (.-target ev)) "selected")
    #_(do #_(swap! st assoc :expanded true)
          #_(core/send! bus [:parent])
          #_(.stopPropagation ev)
          true)
    
    :else true))

(defn mouseup
  [^MouseEvent ev st bus]
  true)

(defn contextmenu
  [^MouseEvent ev st bus]
  (if-not (:mousechord @st)
    true
    (do (.preventDefault ev)
        (swap! st dissoc :mousechord)
        false)))

(defn selectstart
  [^Event ev st bus]
  (if-not (:detail @st)
    true
    (do (swap! st dissoc :detail)
        (.preventDefault ev))))

(defn cleanup!
  [{::keys [listeners] :as app}]
  (doseq [[e f] listeners]
    (js/document.removeEventListener e f true)
    (js/document.removeEventListener e f false))
  (dissoc app ::listeners))

(defn setup!
  [{:keys [bus] :as app}]
  (if (::listeners app)
    (recur (cleanup! app))
    (let [st (atom {})
          ls {"mousedown"   (fn [ev] (mousedown ev st bus))
              "mouseup"     (fn [ev] (mouseup ev st bus))
              "contextmenu" (fn [ev] (contextmenu ev st bus))
              "selectstart" (fn [ev] (selectstart ev st bus))}]
      (doseq [[e f] ls]
        (js/document.addEventListener e f false))
      (assoc app ::listeners ls))))

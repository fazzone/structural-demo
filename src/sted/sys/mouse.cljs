(ns sted.sys.mouse
  (:require [sted.core :as core]
            [datascript.core :as d]))

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
              "selectstart" (fn [ev] (selectstart ev st bus))
              "wheel"       (fn [ev]
                              (let [el (js/document.elementFromPoint
                                        (.-clientX ev)
                                        (.-clientY ev))
                                    eid (aget (.-dataset el) "eid" )]
                                (when (.matches el ".tk.n")
                                  (core/send! bus [::arith
                                                   (js/parseInt eid)
                                                   (js/Math.sign (.-wheelDeltaY ev))]))))}]
      
      (doseq [[e f] ls]
        (js/document.addEventListener e f false))
      
      (-> app
          (assoc ::listeners ls)
          (core/register-simple! ::arith (fn [sel eid x]
                                           (let [e (d/entity (d/entity-db sel) eid)]
                                             (when (= :number (:token/type e))
                                               [[:db/add (:db/id e) :token/value (+ x (:token/value e))]]))))))))

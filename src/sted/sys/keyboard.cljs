(ns sted.sys.keyboard
  (:require [sted.core :as core]
            [sted.sys.kbd.map :as skm]
            [sted.sys.kbd.evt :as ske]))


(defonce generation-track (atom 0))


(def my-generation (swap! generation-track inc))


(def ^:const super "F24")


(def ^:const hyper "F23")

#_(def ^:export event-log #js [])

#_(defn event->kbd
  [^KeyboardEvent ev mods]
  (let [ms @mods]
    (ske/event->kbd ev (:super ms) (:hyper ms))))

(defn keydown
  [^KeyboardEvent ev mods keymap bus]
  #_(js/console.log "KBD" (core/uniqueid bus))
  (if-not (identical? js/document.body js/document.activeElement)
    (do
      
      #_(js/console.log (str "KBD" [my-generation] "The document is not active") js/document.activeElement))
    (let [ms @mods
          kbd (ske/event->kbd ev (:super ms) (:hyper ms))]
      
      #_(js/console.log "KBD" (pr-str ms) kbd (pr-str (get @keymap kbd)))
      
      #_(.push event-log #js { :kbd kbd :time (js/Date.now)})
      #_(js/console.log event-log)
      
      (cond
        (= super (.-key ev))
        (swap! mods conj :super)
        (= hyper (.-key ev))
        (swap! mods conj :hyper)
        :else
        (when-some [mut (get @keymap kbd)]
          (.preventDefault ev)
          (.stopPropagation ev)
          (core/send! bus [mut]))))))




(defn keyup
  [^KeyboardEvent ev mods keymap bus]
  (when (identical? js/document.body js/document.activeElement)
    #_(prn "Keyup" (.-key ev) @mods)
    (if (= super (.-key ev))
      (swap! mods disj :super))))


(defn cleanup!
  [{::keys [listeners] :as app}]
  (doseq [[e f] listeners]
    (js/document.removeEventListener e f true)
    (js/document.removeEventListener e f false))
  (println "KBD" [my-generation] "Cleanup")
  (dissoc app ::listeners))




(defn setup!
  [{:keys [bus] :as app}]
  (if (::listeners app)
    (recur (cleanup! app))
    (let [keymap (atom skm/default)
          mods (atom #{})
          ls {"keydown" (fn [ev]
                          #_(println "This keydown")
                          (keydown ev mods keymap bus))
              "keyup" (fn [ev]
                        (keyup ev mods keymap bus))}]
      (doseq [[e f] ls]
        (js/document.addEventListener e f true))
      (println "KBD" [my-generation] "Setup")
      (assoc app
             ::listeners ls
             ::keymap keymap))))

(ns sted.sys.keyboard
  (:require [sted.core :as core]
            [sted.sys.kbd.map :as skm]
            [sted.sys.kbd.evt :as ske]))

(defonce generation-track (atom 0))

(def my-generation (swap! generation-track inc))

(defn keydown
  [^KeyboardEvent ev keymap bus]
  (if-not (identical? js/document.body js/document.activeElement)
    (js/console.log (str "KBD" [my-generation] "The document is not active") js/document.activeElement)
    (let [kbd (ske/event->kbd ev)]
      (js/console.log "KBD" my-generation  kbd (pr-str (get @keymap kbd))
                      "UBid" (core/uniqueid bus))
      (when-some [mut (get @keymap kbd)]
        (.preventDefault ev)
        (.stopPropagation ev)
        (core/send! bus [mut])))))

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
          ls {"keydown" (fn [ev]
                          (println "This keydown")
                          (keydown ev keymap bus))}]
      (doseq [[e f] ls]
        (js/document.addEventListener e f true))
      (println "KBD" [my-generation] "Setup")
      (assoc app ::listeners ls))))

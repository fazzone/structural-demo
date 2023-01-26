(ns sted.sys.keyboard
  (:require [datascript.core :as d]
            [sted.core :as core]
            [sted.sys.kbd.map :as skm]
            [sted.sys.kbd.evt :as ske]))


(defonce generation-track (atom 0))


(def my-generation (swap! generation-track inc))


(def ^:const super "F24")


(def ^:const hyper "F23")


(defn keydown
  [^KeyboardEvent ev mods keymap bus]
  #_(js/console.log "KBD" (core/uniqueid bus))
  (if-not (identical? js/document.body js/document.activeElement)
    (do
      #_(js/console.log (str "KBD" [my-generation] "The document is not active") js/document.activeElement))
    (let [ms  @mods
          kbd (ske/event->kbd ev (:super ms) (:hyper ms))]
      #_(js/console.log "KBD" (pr-str ms) kbd (pr-str (get @keymap kbd)))
      (cond
        (= super (.-key ev))     (swap! mods conj :super)
        (= hyper (.-key ev))     (swap! mods conj :hyper)
        ;; (= "Shift" (.-key ev))   (swap! mods conj :shift)
        ;; (= "Alt" (.-key ev))     (do (swap! mods conj :meta)
        ;;                              ;; tapping alt switches keyboard focus to browser chrome
        ;;                              (.preventDefault ev)
        ;;                              (.stopPropagation ev))
        ;; (= "Control" (.-key ev)) (swap! mods conj :control)
        
        (= "Alt" (.-key ev))
        (do ;; tapping alt switches keyboard focus to browser chrome
          (.preventDefault ev)
          (.stopPropagation ev))
        
        :else
        (when-some [mut (get @keymap kbd)]
          (.preventDefault ev)
          (.stopPropagation ev)
          (core/send! bus [mut]))))))


(defn keyup
  [^KeyboardEvent ev mods keymap bus]
  (when (identical? js/document.body js/document.activeElement)
    #_(prn "Keyup" (.-key ev) @mods)
    (cond
      (= super (.-key ev))     (swap! mods disj :super)
      (= hyper (.-key ev))     (swap! mods disj :hyper)
      ;; (= "Shift" (.-key ev))   (swap! mods disj :shift)
      ;; (= "Alt" (.-key ev))     (swap! mods disj :meta)
      ;; (= "Control" (.-key ev)) (swap! mods disj :control)
      
      )))

(defn cleanup!
  [{:keys [conn]  :as app}]
  (let [{::keys [listeners]  :as st} (d/entity @conn :sted.page/state)]
    (doseq [[e f] listeners]
      (js/document.removeEventListener e f true)
      (js/document.removeEventListener e f false))
    (d/transact! conn [[:db/retract (:db/id st) ::listeners listeners]])
    (println "KBD" [my-generation] "Cleanup")
    app))


(defn setup!
  [{:keys [conn bus]  :as app}]
  (let [{::keys [listeners keymap mods]  :as st} (d/entity @conn :sted.page/state)]
    (if listeners
      (recur (cleanup! app))
      (let [keymap (if-not keymap
                     (atom skm/default)
                     (doto keymap (reset! skm/default)))
            mods (if-not mods
                   (atom #{})
                   (doto mods (reset! #{})))
            ls {"keydown" (fn [ev]
                            #_(println "This keydown")
                            #_(println "Keydown" mods)
                            (keydown ev mods keymap bus))
                "keyup" (fn [ev]
                          #_(println "Keyup" mods)
                          (keyup ev mods keymap bus))}]
        (doseq [[e f] ls]
          (js/document.addEventListener e f true))
        (d/transact! conn
                     [{:db/id (:db/id st)
                       ::listeners ls
                       ::keymap keymap
                       ::mods mods}])
        (println "KBD" [my-generation] "Setup")
        app))))

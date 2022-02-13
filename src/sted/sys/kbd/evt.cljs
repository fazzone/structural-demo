(ns sted.sys.kbd.evt)

(defn event->kbd
  [^KeyboardEvent ev]
  (str (when (.-ctrlKey ev)  "C-")
       (when (.-altKey ev)   "M-")
       (when (.-shiftKey ev) "S-")
       (.-key ev)))

(ns sted.sys.kbd.evt)

(defn event->kbd
  ([^KeyboardEvent ev]
   (event->kbd ev nil nil))
  ([^KeyboardEvent ev super hyper]
   (str (when (.-ctrlKey ev)  "C-")
        (when (.-altKey ev)   "M-")
        (when super "s-")
        (when hyper "h-")
        (when (.-shiftKey ev) "S-")
        (.-key ev))))

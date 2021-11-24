(ns svg-helpers
  (:refer-clojure :exclude [use])
  (:require [clojure.string :as string]
            [goog.string :as gstring]))

(def ^:const svg-props
  {"xmlns" "http://www.w3.org/2000/svg"
   "xmlns:xlink" "http://www.w3.org/1999/xlink"})

#_(defn svg
  [& body]
  (let [[props & children] body]
    (str "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n"
         (h/html
          (if (map? props)
            (into [:svg (merge svg-props props)] children)
            (into [:svg svg-props] body))))))

(defn- transform*
  [txx txy rot elem]
  [:g {:transform (gstring/format "translate(%s,%s) rotate(%s)" txx txy rot)} elem])

(defn rotate
  [angle elem]
  [:g {:transform (gstring/format "rotate(%s)" angle)} elem])

(defn translate
  [x y elem]
  [:g {:transform (gstring/format "translate(%s,%s)" x y)} elem])

(defn use
  [id]
  [:use {:xlink:href (str "#" (name id))}])

(defn points
  [& pts]
  (->> pts
         (partition 2)
         (map (fn [[x y]] (str x "," y)))
         (string/join " ")))

(ns sted.eda.schema
  (:require [clojure.string :as string])
  (:require-macros
   [sted.macros :as m]))

(def schema-statements
  (string/split (string/trim (m/macro-slurp "src/sted/eda/schema.sql"))
                #";\s*\n"))

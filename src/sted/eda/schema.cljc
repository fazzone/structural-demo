(ns sted.eda.schema
  (:require [clojure.string :as string])
  (:require  [shadow.resource :as rc]))

(def schema-statements
  (string/split (string/trim
                 (rc/inline "sted/eda/schema.sql"))
                #";\s*\n\s*"))

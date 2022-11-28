(ns sted.comp.common
  (:require [rum.core :as rum]))

(defn modeline-portal-id [eid] (str eid "mp"))

(rum/defcontext *modeline-ref*)
(rum/defcontext *outer-chain*)
(rum/defcontext *indenter*)



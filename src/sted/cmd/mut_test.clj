(ns sted.cmd.mut-test
  (:require [sted.cmd.mut :as mut]
            [sted.teleport :as teleport]
            [sted.embed :as e]
            [sted.core :as core :refer [get-selected-form move-selection-tx]]
            [clojure.test :as t]))

(let [{:keys [history]} (teleport/run-steps-next
                         '(x (evalme) d)
                         [{:tag :start :do [:ingest-replacing 4 ]}])
      sel (get-selected-form (:db-after (peek history)))]
  (e/->form sel)
  )

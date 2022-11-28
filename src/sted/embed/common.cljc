(ns sted.embed.common)

(defonce tempid-counter (atom 0))

(defn new-tempid [] (swap! tempid-counter dec))

(defn seq-tx
  [xs]
  (if-let [x (first xs)]
    (cond-> {:seq/first x}
      (next xs) (assoc :seq/next (seq-tx (next xs))))))


(ns sted.eda.path-d
  
  )

(def lexeme
  {:ws #"^(?:(?:\s+,?\s*)|(?:,\s*))"
   :cmd #"^[achlmqstvzACHLMQSTVZ]"
   :num #"^[-+]?(?:(?:(?:\d+\.\d+)|\d+)|\.\d+)(?:[eE][-+]?\d+)?"})

#_(defn svg-d-lex
  [s]
  (let [{:keys [ws cmd num]} lexeme]
   (loop [acc []
          s s]
     (if (empty? s)
       {:acc acc}
       (if-some [s-ws (re-find ws s)]
         (recur acc (subs s (count s-ws)))
         (if-some [s-cmd (re-find cmd s)]
           (recur (conj acc s-cmd) (subs s (count s-cmd)))
           (if-some [s-num (re-find num s)]
             (recur (conj acc (js/parseFloat s-num))
                    (subs s (count s-num))))))))))

(defn svg-d-lex
  [s]
  (let [{:keys [ws cmd num]} lexeme
        v (volatile! nil)
        try-parse (fn [pat suffix]
                    (vreset! v (re-find pat suffix)))]
    (loop [acc []
           s s]
      (if (empty? s)
        {:acc acc}
        (condp try-parse s
          ws  (recur acc (subs s (count @v)))
          cmd (recur (conj acc @v) (subs s (count @v)))
          num (recur (conj acc (js/parseFloat @v)) (subs s (count @v))))
        #_(if-some [s-ws (re-find ws s)]
            (recur acc (subs s (count s-ws)))
            (if-some [s-cmd (re-find cmd s)]
              (recur (conj acc s-cmd) (subs s (count s-cmd)))
              (if-some [s-num (re-find num s)]
                (recur (conj acc (js/parseFloat s-num))
                       (subs s (count s-num))))))))))



(ns sted.eda.dsn
  (:require
   [clojure.string :as string])
  )

#?(:clj (set! *warn-on-reflection* true))

(defn quote-tokens
  [dsn-string]
  (string/replace dsn-string
                  ;; Quote every atom which is not:
                  ;; - already quoted (starting with quote)
                  ;; - in function position (starting with paren)
                  ;; - a number (starting with digit or minus) 
                  #"\s+([^\"\(\d\-][^\s\(\)]+)"
                  " \"$1\" "))

(defn remove-stringquote
  [dsn-string]
  (string/replace dsn-string
                  #"\(\s*string_quote\s*\"\s*\)"
                  ""))

(defn dsn->edn
  [dsn-string]
  (-> dsn-string 
      (remove-stringquote)
      (quote-tokens)
      (string/replace #"\\" "")))



(defn dsnparse
  [s]
  (loop [acc []
         stack []
         ^String s s]
    (cond
      (string/starts-with? s ")")
      (recur (conj (peek stack) acc) (pop stack) (subs s 1))
        
      (string/starts-with? s "(")
      (recur [] (conj stack acc) (subs s 1))
        
      (and (empty? s) (empty? stack))
      acc
      
      (empty? s)
      (do (println "?Stack not empty" )
          (dotimes [i (count stack)]
            (println "Stack" i)
            #_(cljs.pprint/pprint (nth stack i)))
          (println "Acc to follow")
          acc)
      
      :else
      (if-some [spaces (re-find #"^\s+" s)]
        (recur acc stack (.substring s (count spaces)))
        ;; note that due to this hack we are unable to parse a number as the last top level form
        (if-some [[parsed left right hack] (re-find #"^(-?\d+)(\.\d*)?([\s\)])" s)]
          (recur (conj acc #?(:cljs (js/parseFloat (str left right))
                              :clj #_(if-not right
                                     (Long/parseLong left)
                                     (Double/parseDouble (str left right)))
                              (Double/parseDouble (str left right))))
                 stack
                 (.substring s (- (count parsed) (count hack))))
          (if-some [token (re-find #"^[^\s\)\"]+" s)]
            (recur (conj acc (cond-> token (empty? acc) symbol))
                   stack
                   (.substring s (count token)))
            (if-some [justquote (re-find #"^\"\s*\)" s)]
              (recur (conj (peek stack) (conj acc "\""))
                     (pop stack)
                     (.substring s (count justquote)))
              (if-some [[parsed quoted] (re-find #"^\"([^\"]*?)\"" s)] 
                (recur (conj acc quoted) stack (.substring s (count parsed)))
                (prn "IUknnown" s)))))))))



;;-- pcb boundary (abs, ref)
;;-- plane poly (abs)
;;-- window poly (abs)
;;-- wire (abs)

;;-- pad shape (rel)
;;-- comp outline (rel)
;; keepout (rel)



#_(format "%f" (last (first (dsnparse "(version 2010204944)"))))

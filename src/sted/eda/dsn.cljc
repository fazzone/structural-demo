(ns sted.eda.dsn
  (:require
   [clojure.string :as string])
  (:require-macros
   [sted.macros :as m]))

(def dsnstr
  (m/macro-slurp "srv/eda/RP2040_minimal.dsn")
  
  #_(m/macro-slurp "srv/eda/hacked.dsn")
  #_(m/macro-slurp "srv/eda/graebert.dsn")
  #_(m/macro-slurp "srv/eda/kit-dev-coldfire-xilinx_5213.dsn"))


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
  ( println
   "Re-find"
   (pr-str
    (re-find
     #"\(([a-z]+)\s+"
     "(a (b (c x y)))"
                       
     )))
  #_(or (when-some [[parsed func] (re-find #"\(([a-z]+)\s+" s) ]
          (into [(symbol func)]
                [(subs s (count parsed))])))
  (loop [acc []
         stack []
         s s]
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
            (cljs.pprint/pprint (nth stack i)))
          (println "Acc to follow")
          acc)
      
      :else
      (if-some [spaces (re-find #"^\s+" s)]
        (recur acc stack (subs s (count spaces)))
        (if-some [[parsed left right end] (re-find #"^(-?\d+)(\.\d*)?([\s\)])" s)]
          (recur (conj acc (js/parseFloat (str left right)))
                 stack
                 (subs s (- (count parsed) (count end))))
          (if-some [token (re-find #"^[^\s\)\"]+" s)]
            (recur (conj acc (cond-> token (empty? acc) symbol))
                   stack
                   (subs s (count token)))
            (if-some [justquote (re-find #"^\"\s*\)" s)]
              (recur (conj (peek stack) (conj acc "\""))
                     (pop stack)
                     (subs s (count justquote)))
              (if-some [[parsed quoted] (re-find #"^\"([^\"]*?)\"" s)]
                
                (recur (conj acc quoted) stack (subs s (count parsed)))
                (prn "IUknnown" s)))))))))



;;-- pcb boundary (abs, ref)
;;-- plane poly (abs)
;;-- window poly (abs)
;;-- wire (abs)

;;-- pad shape (rel)
;;-- comp outline (rel)
;; keepout (rel)


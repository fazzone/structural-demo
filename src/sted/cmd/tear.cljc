(ns sted.cmd.tear
  (:require
   [sted.embed.common :as ec]
   [clojure.string :as string]
   [sted.embed :as e]
   [sted.cmd.edit :as edit]
   [sted.cmd.insert :as insert]
   [sted.cmd.nav :as nav]
   [sted.core :as core :refer [get-selected-form move-selection-tx]]))

(defn stitch*
  [sel e]
  (when-let [head (:seq/first e)]
    (let [text (->> (tree-seq :coll/type e/seq->vec e)
                    (keep :token/value)
                    (apply str))
          tok (insert/unify-with "tok" (insert/parse-editbox-tx text))]
      (into [tok]
            (concat (edit/form-overwrite-tx e "tok")
                    (move-selection-tx (:db/id sel) "tok"))))))
(defn restitch
  [sel c]
  (some->> (nav/parents-seq c)
           (filter (comp #{:tear} :coll/type))
           #_(first)
           (last)
           (stitch* sel)))

(defn do-re-tear
  [s delim-re]
  (let [pat (re-pattern (str "^(.*?)(" delim-re ")" ))]
    (loop [s s
           acc []]
      (if (empty? s)
        acc
        (if-let [[_ m d] (re-find pat s)]
          (recur (subs s (+ (count m) (count d)))
                 (cond-> acc
                   (not= "" m) (conj m)
                   true (conj d)))
          (conj acc s))))))

(defn tear-loop
  [s]
  (letfn [(classify [ch]
            (cond
              (string/includes? ":.-_$/<>()[]{}=$?" ch) :punct
              (string/includes? "0123456789" ch)        :number

              :else
              (let [upper (string/upper-case ch)
                    lower (string/lower-case ch)]
                (cond (= upper lower) :other
                      (= ch lower)    :lower
                      (= ch upper)    :upper
                      :else           :what))))
          
          (emit [acc start end]
            (if (= end start)
              acc
              (conj acc (subs s start end))))
          
          (go [i start prev acc]
              (if (= i (count s))
                (emit acc start i)
                (let [ch  (subs s i (inc i))
                      cls (classify ch)
                      
                      continue?
                      (or (nil? type)
                          (and (= cls prev) (not= cls :punct))
                          (and (= prev :upper) (= cls :lower)))]
                  
                  (if continue?
                    (recur (inc i) start cls acc)
                    (recur (inc i) i cls (emit acc start i))))))
          
          (check [ans]
            (when (not= [s] ans)
              ans))]

    (check (go 0 0 nil []))))

(defn tear-preference-order
  [s]
  (letfn [(try-split [[head & more :as groups]]
            (when more groups))]
    (or (try-split (do-re-tear s "\\s+"))
        (tear-loop s))))

(defn tear*
  [sel torn]
  (let [tt (:token/type sel)
        pre (case tt
              (:string)
              {:db/id "tpre"
               :coll/_contains "torn"
               :token/type :symbol
               :token/value "\""}
              nil)
        tels (for [e torn]
               {:token/value e
                :token/type :symbol
                :coll/_contains "torn"})
        tels+ (cond->> tels pre (cons pre))]
    
    (into [(assoc (ec/seq-tx tels+)
                  :db/id "torn"
                  :coll/type :tear)]
          
          (concat (edit/form-overwrite-tx sel "torn")
                  
                  (move-selection-tx (:db/id sel) "torn")))))

(defn tear-tx
  [sel]
  (if-some [parts (some-> sel :token/value tear-preference-order)]
    (tear* sel parts)
    (restitch sel sel)))

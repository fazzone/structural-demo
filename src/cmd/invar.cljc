(ns cmd.invar
  (:require [cmd.move :as move]
            [embed :as e]
            [datascript.core :as d]
            [clojure.set :as set]))

(defn check-linkage
  [e]
  (when-let [contained (:coll/contains e)]
    (let [contained (set (map :db/id contained))
          linked (set (map :db/id (e/seq->vec e)))
          cbnl (seq (set/difference contained linked))
          lbnc (seq (set/difference linked contained))]
      (when cbnl
        (println "Contained but not linked: " cbnl)
        (run! (comp prn d/touch (partial d/entity (d/entity-db e))) cbnl))
      (when lbnc
        (println "Linked but not contained: " lbnc)
        (run! (comp prn d/touch (partial d/entity (d/entity-db e))) lbnc))
      (not (or cbnl lbnc)))))

(defn check-contains
  [e]
  (case (count (:coll/_contains e))
    0 true
    1 true
    (do (println (:db/id e) "contained by" (:coll/_contains e)))))

(defn check
  [e]
  (and (check-linkage e) (check-contains e)))

(defn check-all
  [root]
  (loop [[here & more] [root]]
    #_(println "Check" here (:db/id here))
    (let [contained (:coll/contains here)]
      (cond
        (and (nil? here) (empty? more)) (do (println "OK") true)
        (empty? contained) (recur more)
        (check here) (recur (into more contained))
        :else (println "Failed")))))

(defn tx-listen-fn
  [{:keys [tx-data db-after] :as tx-report}]
  (let [{:state/keys [bar] :as state} (d/entity db-after :page/state)]
    #_(when-not (check-all bar)
      (println "Tempids")
      (prn (:tempids tx-report))
      (println "Tx data")
      (run! prn tx-data))
    (check-all bar)
    (println "Tempids")
    (prn (:tempids tx-report))
    (println "Tx data")
    (run! prn tx-data))
  #_(let [changed (dedupe (for [[_ _ v _] tx-data]
                            v))]
      (doseq [c changed]
        (check (d/entity db-after c)))))

#_(defn check
  [e]
  (when-let [contained (:coll/contains e)]
    (let [c-id (-> (map :db/id contained)
                   set
                   (conj (:db/id e)))
          ts (tree-seq :coll/type e/seq->vec e)
          explored (set (map :db/id ts))
          missing (set/difference explored c-id)
          extra (set/difference c-id explored)]
      (println "C-id" c-id)
      (println "D-id" (conj c-id (:db/id e)))
      (println "Missing" missing)
      (doseq [m missing]
        (prn (d/touch (d/entity (d/entity-db e) m))))
      (println "Explored")
      (doseq [t ts]
        (prn (d/touch t)))
      (println "Extra" extra)
      (println "Cmonn" (set/intersection explored c-id)))))
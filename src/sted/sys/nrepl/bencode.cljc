(ns sted.sys.nrepl.bencode
  "Lifted from https://gitlab.com/jamesleonis/bencode-cljc"
  (:require [clojure.string :as string]))

(defn- parse-int [s]
  (when s
    #?(:clj (Long/parseLong s)
       :cljs (js/parseInt s))))

(declare serialize)

(defn- map-reducer-fn [s [key val]]
  (when s
    (when-let [sk (serialize key)]
      (when-let [sv (serialize val)]
        (str s sk sv)))))

(defn- serialize-map [m]
  (when (every? (some-fn string? keyword?) (keys m))
    (when-let [serial (reduce map-reducer-fn "" (into (sorted-map) m))]
      (str "d" serial "e"))))

(defn- serialize-list [coll]
  (let [serialized-list (map serialize coll)]
    (when (not-any? nil? serialized-list)
      (str "l" (string/join (map serialize coll)) "e"))))

(declare deserialize-next)

;; Deserialization worker functions return a tuple. The first value contains
;; the parsed value, the second contains the rest of the unparsed string. The
;; unparsed string is formed by dropping the parsed section from the original
;; string. This mimics stream processing while keeping each function pure.
;;
;; A BEncoded string is considered finished when the unparsed end of the tuple
;; is empty, representing a fully parsed BEncoded string.
;;
;; As an implementation detail, this dangling stream is not exposed in the public
;; interface. However it is only necessary while implementing the containers map
;; and list. Any dangling stream that escapes the parser is caught in `deserialize`
;; and returns a `nil` value to indicate malformed strings.

(defn- deserialize-int [s]
  (when-let [[found stripped] (re-find #"i((?:-?[1-9]{1}[0-9]*)|(?:0))e" s)]
    (vector
      (parse-int stripped)
      (string/join (drop (count found) s)))))

(defn- deserialize-string [s]
  (when-let [[size-shard size-str] (re-find #"(\d+):" s)]
    (when-let [size (parse-int size-str)]
      (let [[string leftover] (split-at size (drop (count size-shard) s))]
        (when (= (count string) size)
          (vector
            (string/join string)
            (string/join leftover)))))))

(defn- deserialize-list [s]
  (loop [leftover (string/join (next s))
         peek-ahead (first leftover)
         coll (vector)]
    (if (= \e peek-ahead)
      (vector (sequence coll) (string/join (next leftover)))
      (when-let [[found rest] (deserialize-next leftover)]
        (recur rest (first rest) (conj coll found))))))

(defn- deserialize-map [s]
  (when-let [[map-list rest] (deserialize-list s)]
    (when (and (even? (count map-list))
               (every? string? (take-nth 2 map-list)))
      (vector (apply hash-map map-list) rest))))

(defn- deserialize-next
  [encoded]
  (case (first (seq encoded))
    (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) (deserialize-string encoded)
    \i (deserialize-int encoded)
    \l (deserialize-list encoded)
    \d (deserialize-map encoded)
    nil))

(defn serialize
  "Serialize a given Clojure(script) object to a BEncode string.

  Supports integers, strings, maps, and lists natively. Map keys must be strings, or coercable to strings.
  Coerces keywords to strings using 'name. Caution, this truncates namespaced keywords.
  Coerces vectors to list. Sets are not supported.
  Nil values are not supported. Consider an empty list.

  Returns the BEncoded string on success, and nil on failure"
  [object]
  (condp #(%1 %2) object
    integer? (str "i" object "e")
    string? (str (count object) ":" object)
    keyword? (serialize (name object))
    sequential? (serialize-list object)
    map? (serialize-map object)
    nil))

(defn deserialize
  "Deserialize a BEncoded string into a Clojure(script) data structure.

  Returns a data structure on success, and nil on failure"
  [bencoded-str]
  (when (string? bencoded-str)
    (let [[parsed rest] (deserialize-next bencoded-str)]
      (when (string/blank? rest)
        parsed))))

(comment
  (deserialize (slurp "./resources/ubuntu.torrent" :encoding "US-ASCII"))
  (deserialize (slurp "./resources/debian.torrent" :encoding "US-ASCII"))
  (deserialize-string (string/join (next (slurp "./resources/ubuntu.torrent" :encoding "US-ASCII"))))
  (deserialize-next (slurp "./resources/ubuntu.torrent")))


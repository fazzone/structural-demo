(ns sted.qq
  (:require [clojure.java.io :as io])
  (:import [java.util Base64]
           [java.util.zip ZipOutputStream ZipInputStream ZipEntry]
           [java.io ByteArrayOutputStream]))

[a b]
(defn bb-selfextractor
  [exe args]
  (let [name "bb.exe"
        exe (io/file exe)
        zipped (with-open [b (ByteArrayOutputStream.)]
                 (with-open [z (ZipOutputStream. b)]
                   (.putNextEntry z (ZipEntry. name))
                   (io/copy exe z))
                 (.toByteArray b))
        encoded (with-open [b (ByteArrayOutputStream.)]
                  (io/copy zipped b)
                  (.encodeToString (Base64/getEncoder) (.toByteArray b)))]
    (with-open [s (io/writer "hacks.ps1")]
      (doto s
        (.write "$b = [Convert]::FromBase64String('")
        (.write encoded)
        (.write "')\n")
        (.write "[IO.File]::WriteAllBytes(\"$pwd/z.zip\", $b)\n")
        (.write "Expand-Archive -Force -Path z.zip -DestinationPath extracted\n")
        (.write ".\\extracted\\bb.exe ")
        (.write args)
        (.write "\n")))))

(comment
  (time
   (bb-selfextractor
    (io/file "bb.exe")
    "nrepl-server")))







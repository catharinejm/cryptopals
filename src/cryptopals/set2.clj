(ns cryptopals.set2
  (:require [cryptopals.utils :refer :all]
            [clojure.java.io :as io])
  (:import javax.crypto.Cipher
           javax.crypto.spec.SecretKeySpec))

(defn challenge9
  []
  (let [input "YELLOW SUBMARINE"
        expected "YELLOW SUBMARINE\4\4\4\4"
        result (apply str (map char (pkcs7 input 20)))]
    (assert (= expected result))
    result))

(defn challenge10
  []
  (let [bytes (decode-base64-file "resources/10.txt")]
    (println (apply str (sequence bytes->chars*
                                  (cbc-decrypt bytes (repeat 16 0) "YELLOW SUBMARINE"))))))

(ns cryptopals.set2
  (:require [cryptopals.utils :refer :all]
            [clojure.java.io :as io])
  (:import javax.crypto.Cipher
           javax.crypto.spec.SecretKeySpec))

(defn pkcs7
  [block blen]
  (take blen (concat block (repeat (- blen (count block))))))

(defn cbc-decrypt
  [bytes iv ^String key]
  (let [decrypter (aes-decrypter key)]
    (loop [chunks (reverse (partition 16 bytes))
           decoded ()]
      )))

(defn challenge9
  []
  (let [input "YELLOW SUBMARINE"
        expected "YELLOW SUBMARINE\4\4\4\4"
        result (apply str (map char (pkcs7 input 20)))]
    (assert (= expected result))
    result))

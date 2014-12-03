(ns cryptopals.set3
  (:require [cryptopals.utils :refer :all]
            [clojure.java.io :as io]))

(defn generate-padding-oracle
  [bytes]
  (let [key (random-buffer 16)
        iv (random-buffer 16)
        enc (cbc-encrypt bytes iv key)
        validate (fn [in]
                   (try (strip-pkcs7 (cbc-decrypt in iv key) 16)
                        true
                        (catch Exception e
                          (if (:padding-error (ex-data e))
                            false
                            (throw e)))))]
    [enc validate]))

(defn cbc-padding-crack
  [bytes]
  (let [[encrypted valid?] (generate-padding-oracle bytes)
        decrypt-block (fn [block]
                        (loop [n 15
                               byte 0
                               suffix (list)
                               decrypted (list)]
                          (if (>= n 0)
                            (let [dummy (concat (repeat n 0) (list byte) suffix)]
                              (if (and (valid? (concat dummy block))
                                       (or (< n 15)
                                           (valid? (concat (repeat (dec n) 0)
                                                           (list 1 byte)
                                                           block))))
                                (let [ptext (xor byte (- 16 n))]
                                  (recur (dec n)
                                         0
                                         (map #(xor %1 %2 (- 17 n))
                                              (drop n block)
                                              (cons ptext decrypted))
                                         (conj decrypted ptext)))
                                (if (< byte 255)
                                  (recur n (inc byte) suffix decrypted)
                                  (throw (ex-info (str "Failed to decrypt byte " n)
                                                  {:suffix suffix
                                                   :decrypted decrypted})))))
                            decrypted)))]
    (sequence (comp (partition-all 16)
                    (mapcat decrypt-block)
                    bytes->chars*)
              encrypted)))

(defn challenge17
  []
  (let [inputs (line-seq (io/reader "resources/17.txt"))]
    (doseq [i inputs]
      (println (-> i
                   base64->bytes
                   cbc-padding-crack
                   bytes->string)))))

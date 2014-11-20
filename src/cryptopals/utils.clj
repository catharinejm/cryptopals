(ns cryptopals.utils
  (:require [clojure.java.io :as io])
  (:import javax.crypto.Cipher
           javax.crypto.spec.SecretKeySpec))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro dbg [x]
  `((fn [y#]
      (println "Form: " '~(second &form))
      (println "Result: " y#)
      y#) ~x))

(defmacro bit-ops
  []
  `(do
     ~@(for [[s l] '[[<< bit-shift-left]
                     [>> bit-shift-right]
                     [>>> unsigned-bit-shift-right]
                     [| bit-or]
                     [& bit-and]
                     [xor bit-xor]
                     [!b bit-not]]]
         `(do
            (def ~s ~(deref (resolve l)))
            (.setMeta (var ~s) (meta ~(resolve l)))))))
(bit-ops)

(def hex->bytes*
  (let [nibl (fn [chr]
               (case chr
                 (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) (- (int chr) (int \0))
                 (\a \b \c \d \e \f) (+ 10 (- (int chr) (int \a)))
                 (throw (IllegalArgumentException. (str "invalid hexidecimal character: " chr)))))
        to-byte (fn [[h l]]
                  (+ (* 16 (nibl h)) (nibl l)))]
    (comp (partition-all 2)
          (map to-byte))))

(def bytes->hex*
  (let [xnibl (fn [n]
                (when (or (neg? n)
                          (> n 15))
                  (throw (IllegalArgumentException. (str "nibble out of range: " n))))
                (cond
                 (< n 10)
                 (char (+ n (int \0)))

                 :else (char (+ (- n 10) (int \a)))))
        hex (fn [b]
              [(xnibl (>>> b 4))
               (xnibl (& b 0xF))])]
    (mapcat hex)))

(def ^String base64string
  (apply str (concat (map char (range (int \A) (inc (int \Z))))
                     (map char (range (int \a) (inc (int \z))))
                     (map char (range (int \0) (inc (int \9))))
                     (list \+ \/))))

(def bytes->base64*
  (let [b64* (fn [b]
               (if (or (neg? b) (> b 63))
                 (throw (IllegalArgumentException. (str "invalid base64 number: " b)))
                 (.charAt base64string b)))
        b64 (fn [[b1 b2 b3]]
              (let [chrs (cond
                          b3
                          (map b64* [(>>> b1 2)
                                     (| (<< (& b1 3) 4)
                                        (>>> b2 4))
                                     (| (<< (& b2 0xF) 2)
                                        (>>> b3 6))
                                     (& b3 0x3F)])

                          b2
                          (map b64* [(>>> b1 2)
                                     (| (<< (& b1 3) 4)
                                        (>>> b2 4))
                                     (<< (& b2 0xF) 2)])

                          :else
                          (map b64* [(>>> b1 2)
                                     (<< (& b1 3) 4)]))]
                (take 4 (concat chrs (repeat \=)))))]
      (comp (partition-all 3)
            (mapcat b64))))

(def base64->bytes*
  (let [b64int (fn [n]
                 (let [i (.indexOf base64string (int n))]
                   (if (and (= -1 i)
                            (not= \= (char n)))
                     (throw (IllegalArgumentException. (str "Illegal base64 character: " (char n))))
                     i)))
        to-bytes (fn [cs]
                   (let [[n1 n2 n3 n4] (map b64int cs)]
                     (cond
                      (> n4 -1)
                      [(| (<< n1 2) (>>> n2 4))
                       (| (<< (& n2 0xF) 4)
                          (>>> n3 2))
                       (| (<< (& n3 3) 6) n4)]

                      (> n3 -1)
                      [(| (<< n1 2) (>>> n2 4))
                       (| (<< (& n2 0xF) 4)
                          (>>> n3 2))]

                      :else
                      [(| (<< n1 2) (>>> n2 4))])))]
    (comp (partition-all 4)
          (mapcat to-bytes))))

(def bytes->chars*
  (map #(char (& 0xFF %))))

(defn sanitize-hex
  ^String [^String hex]
  (.toLowerCase (if (odd? (count hex))
                  (str "0" hex)
                  hex)))

(defn hamming-distance
  [buf1 buf2]
  (reduce #(+ %1 (Long/bitCount %2))
          0 (map #(xor %1 %2) buf1 buf2)))

(defn aes-decrypter
  ^Cipher [^String key]
  (when (not= (.length key) 16)
    (throw (IllegalArgumentException. (str "invalid key length: " (.length key) " (key: " key ")"))))
  (doto (javax.crypto.Cipher/getInstance "AES/ECB/NoPadding")
    (.init javax.crypto.Cipher/DECRYPT_MODE
           (SecretKeySpec. (.getBytes key) "AES"))))

(defn aes-ecb-decrypt
  [bytes ^String key]
  (let [decrypter (aes-decrypter key)]
    (sequence (comp (partition-all 16)
                    (mapcat #(.doFinal decrypter (byte-array %)))
                    (map #(char (& 0xFF %))))
              bytes)))

(def xor-buffers*
  (map xor))

(defn xor-buffers
  [buf1 buf2]
  (when-not (= (count buf1) (count buf2))
    (throw (IllegalArgumentException. "Unequal input lengths")))
  (sequence xor-buffers* buf1 buf2))

(defn decode-base64-file
  [file]
  (sequence (comp cat base64->bytes*) (line-seq (io/reader file))))


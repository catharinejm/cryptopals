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

(def ^String letter-order
  "etaoin shrdlcumwfgypbvkjxqz")

(defn score-string
  [^String string]
  (let [s (.toLowerCase string)
        freq (frequencies s)
        comp (reify java.util.Comparator
               (compare [this c1 c2]
                 (let [f1 (freq c1 0)
                       f2 (freq c2 0)]
                   (cond
                    (< f1 f2) 1
                    :else -1))))
        result (apply sorted-set-by comp (keys freq))
        ^String letters (apply str (filter freq letter-order))]
    (loop [r result
           sum 0
           idx 0]
      (if (seq r)
        (if ((set letters) (first r))
          (recur (rest r)
                 (+ sum (Math/abs (- idx (.indexOf letters (int (first r))))))
                 (inc idx))
          (recur (rest r) (+ sum (.length letter-order)) idx))
        sum))))

(defn sanitize-hex
  ^String [^String hex]
  (.toLowerCase (if (odd? (count hex))
                  (str "0" hex)
                  hex)))

(defn hamming-distance
  [buf1 buf2]
  (reduce #(+ %1 (Long/bitCount %2))
          0 (map #(xor %1 %2) buf1 buf2)))

(defn validate-key!
  [key]
  (when (not= (count key) 16)
    (throw (IllegalArgumentException. (str "invalid key length: " (count key) " (key: " key ")")))))

(defn get-bytes
  ^bytes [buf]
  (if (string? buf)
    (.getBytes ^String buf)
    (byte-array buf)))

(defn aes-encrypter
  ^Cipher [key]
  (validate-key! key)
  (doto (Cipher/getInstance "AES/ECB/NoPadding")
    (.init Cipher/ENCRYPT_MODE
           (SecretKeySpec. (get-bytes key) "AES"))))

(defn aes-decrypter
  ^Cipher [key]
  (validate-key! key)
  (doto (javax.crypto.Cipher/getInstance "AES/ECB/NoPadding")
    (.init javax.crypto.Cipher/DECRYPT_MODE
           (SecretKeySpec. (get-bytes key) "AES"))))

(defn ecb-decrypt
  [bytes key]
  (let [decrypter (aes-decrypter key)]
    (sequence (comp (partition-all 16)
                    (mapcat #(.update decrypter (byte-array %)))
                    bytes->chars*)
              bytes)))

(defn pad-bytes
  [bytes pad-len]
  (concat bytes (repeat pad-len pad-len)))

(defn pkcs7
  [block blen]
  (pad-bytes block (- blen (count block))))

(defn encrypt-block*
  [^Cipher encrypter]
  (comp (map #(pkcs7 % 16))
        (map #(.update encrypter (byte-array %)))))

(defn encrypt-block
  [encrypter block]
  (sequence (encrypt-block* encrypter) block))

(defn ecb-encrypt
  [bytes key]
  (let [encrypter (aes-encrypter key)
        res (sequence (comp (partition-all 16)
                            (encrypt-block* encrypter)
                            cat)
                      bytes)]
    (.doFinal encrypter)
    res))

(def xor-buffers*
  (map xor))

(defn xor-buffers
  [buf1 buf2]
  (when-not (= (count buf1) (count buf2))
    (throw (IllegalArgumentException. "Unequal input lengths")))
  (sequence xor-buffers* buf1 buf2))

(defn cbc-block-encrypt*
  [iv ^Cipher encrypter]
  (let [prev (volatile! iv)]
    (fn [xf]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
           (let [p @prev
                 xord (xor-buffers p input)
                 encrypted (seq (.update encrypter (byte-array xord)))]
             (vreset! prev encrypted)
             (xf result encrypted)))))))

(defn cbc-encrypt
  [bytes iv key]
  (let [encrypter (aes-encrypter key)]
    (sequence (comp (partition-all 16)
                    (map #(pkcs7 % 16))
                    (cbc-block-encrypt* iv encrypter)
                    cat)
              bytes)))

(defn cbc-decrypt
  [bytes iv key]
  (let [decrypter (aes-decrypter key)]
    (loop [chunks (partition 16 bytes)
           key iv
           decoded []]
      (if (seq chunks)
        (let [dec (.update decrypter (byte-array (first chunks)))
              xord (xor-buffers dec key)]
          (recur (rest chunks)
                 (first chunks)
                 (conj decoded xord)))
        (apply concat decoded)))))

(defn decode-base64-file
  [file]
  (sequence (comp cat base64->bytes*) (line-seq (io/reader file))))

(defn average-hamming-distance
  [sample-size bytes ksize]
  (letfn [(avg-dist [[key & keys :as sample]]
            (/ (reduce (fn [s k]
                         (+ s (hamming-distance key k)))
                       0 keys)
               (count sample)))]
    (loop [keys (take sample-size (partition ksize bytes))
           sum 0]
      (if (next keys)
        (recur (next keys)
               (+ sum (avg-dist keys)))
        (/ sum sample-size)))))

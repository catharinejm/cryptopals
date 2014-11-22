(ns cryptopals.set2
  (:require [cryptopals.utils :refer :all]
            [clojure.java.io :as io])
  (:import javax.crypto.Cipher
           javax.crypto.spec.SecretKeySpec
           java.security.SecureRandom))

(defn random-buffer
  [len]
  (let [bs (byte-array len)]
    (.nextBytes (SecureRandom.) bs)
    (seq bs)))

(defn random-pad-buffer
  [buf]
  (letfn [(pad [] (random-buffer (+ 5 (int (rand 6)))))]
    (concat (pad) buf (pad))))

(defn random-encrypter
  [bytes]
  (let [key (random-buffer 16)
        padded-buf (random-pad-buffer bytes)
        cbc? (zero? (int (rand 2)))]
    (if cbc?
      {:method :cbc
       :encrypted (cbc-encrypt padded-buf
                               (random-buffer 16)
                               key)}
      {:method :ecb
       :encrypted (ecb-encrypt padded-buf
                               key)})))

(defn consistent-key-encrypter
  [key unknown-input]
  (fn [bytes]
    (ecb-encrypt (concat bytes unknown-input) key)))

(declare guess-encryption)
(defn determine-method
  [encrypter-fn]
  (guess-encryption (encrypter-fn (apply concat (repeat 4 (repeat 16 0))))))

(defn determine-blocksize
  [encrypter-fn]
  (let [increasing-lengths (iterate #(cons 0 %) (list 0))
        groups (partition-by count (map encrypter-fn increasing-lengths))]
    (- (count (first (second groups)))
       (count (ffirst groups)))))

(defn decrypt-unknown
  []
  (let [key (random-buffer 16)
        special-input (decode-base64-file "resources/12.txt")
        encrypter-fn (consistent-key-encrypter key special-input)
        _ (when-not (= :ecb (determine-method encrypter-fn))
            (throw (RuntimeException. "Only ECB encrypters will work")))
        blocksize (determine-blocksize encrypter-fn)
        enc-length (count (encrypter-fn nil))
        next-match (fn [pad-len offset]
                     (take blocksize (drop offset (encrypter-fn (repeat pad-len 0)))))
        decrypt-byte (fn [match known-bytes]
                       (loop [guess 0]
                         (let [guess-bytes (conj known-bytes guess)
                               res (encrypter-fn (subvec guess-bytes
                                                         (- (count guess-bytes) blocksize)))]
                           (if (= (take blocksize res) match)
                             guess-bytes
                             (if (= guess 255)
                               nil
                               (recur (inc guess)))))))
        decode-block (fn [offset known-bytes]
                       (let [last-block? (= (- enc-length offset) blocksize)]
                         (loop [pad-len (dec blocksize)
                                known-bytes known-bytes]
                           (if (< pad-len 0)
                             known-bytes
                             (let [match (next-match pad-len offset)
                                   decrypted (decrypt-byte match known-bytes)]
                               (when-not decrypted
                                 (throw (ex-info "Unable to decode byte"
                                                 {:bytes known-bytes})))
                               (if (and last-block?
                                        (> pad-len 0)
                                        (= (last decrypted) 1))
                                 (let [pad-check (decrypt-byte (next-match (dec pad-len) offset)
                                                               (conj known-bytes 2))]
                                   (if (= (last pad-check) 2)
                                     (into known-bytes (repeat pad-len pad-len))
                                     (recur (dec pad-len)
                                            decrypted)))
                                 (recur (dec pad-len)
                                        decrypted)))))))
        decode-buffer (fn []
                        (loop [offset 0
                               known (vec (repeat (dec blocksize) 0))]
                          (if (= offset enc-length)
                            (drop (dec blocksize) known)
                            (recur (+ offset blocksize)
                                   (decode-block offset known)))))]
    (decode-buffer)))

(defn xor-prev*
  [init]
  (let [prev (volatile! init)]
    (fn [xf]
      (fn
        ([] (xf))
        ([res] (xf res))
        ([res input]
           (let [p @prev
                 nxt (xor-buffers input p)]
             (vreset! prev input)
             (xf res nxt)))))))

(defn guess-encryption
  [encrypted]
  (let [chunks (partition-all 16 encrypted)]
    (if (not= (count chunks) (count (set chunks)))
      :ecb
      :cbc)))

(defn score-guesses
  [n]
  (loop [n n
         res {:right 0
              :wrong 0}]
    (let [{:keys [method encrypted]} (random-encrypter (apply concat (repeat 4 (repeat 16 0))))
          guess (guess-encryption encrypted)]
      (if (> n 0)
        (if (= guess method)
          (recur (dec n)
                 (update-in res [:right] inc))
          (recur (dec n)
                 (update-in res [:wrong] inc)))
        res))))

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

(defn challenge11
  []
  (let [{:keys [right wrong]} (score-guesses 10000)]
    (assert (or (zero? wrong) (< (/ right wrong) 0.95)))
    (println "Success!")))


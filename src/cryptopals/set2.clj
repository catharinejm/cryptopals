(ns cryptopals.set2
  (:require [cryptopals.utils :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as s])
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
  ([] (consistent-key-encrypter nil))
  ([unknown-input]
     (let [key (random-buffer 16)]
       (fn encrypter
         ([] (encrypter nil))
         ([bytes]
            (ecb-encrypt (concat bytes unknown-input) key))))))

(declare guess-encryption)
(defn determine-method
  [encrypter-fn]
  (guess-encryption (encrypter-fn (apply concat (repeat 4 (repeat 16 0))))))

(defn determine-blocksize
  [encrypter-fn]
  (let [increasing-lengths (iterate #(cons 0 %) (list 0))
        groups (partition-by count (map encrypter-fn increasing-lengths))]
    (Math/abs (apply - (take 2 (map (comp count first) groups))))))

(defn block-getter
  [blocksize input-gen]
  (fn [offset input]
    (take blocksize (drop offset (input-gen input)))))

(defn encrypted-block-attributes
  [encrypter-fn]
  (let [blocksize (determine-blocksize encrypter-fn)
        get-block (block-getter blocksize encrypter-fn)
        init-offset (loop [offset 0]
                      (let [match (get-block offset "")
                            check (get-block offset (list 0))]
                        (if (seq check)
                          (if (= check match)
                            (recur (+ offset blocksize))
                            offset)
                          (throw (RuntimeException. "Failed to find alterable block")))))
        min-pad (loop [input-len 0
                       match (get-block init-offset nil)]
                  (when (> input-len blocksize)
                    (throw (RuntimeException. "Failed to determine unknown prefix length")))
                  (let [check (get-block init-offset (repeat (inc input-len) 0))]
                    (if (= match check)
                      (mod input-len blocksize)
                      (recur (inc input-len) check))))
        init-offset (if (> min-pad 0)
                      (+ init-offset blocksize)
                      init-offset)]
    {:blocksize blocksize
     :base-offset init-offset
     :min-padding min-pad
     :get-block get-block}))

(defn decrypt-unknown
  [encrypter-fn]
  (when-not (= :ecb (determine-method encrypter-fn))
    (throw (RuntimeException. "Only ECB encrypters will work")))
  (let [{:keys [blocksize base-offset min-padding get-block]} (encrypted-block-attributes encrypter-fn)
        enc-length (count (encrypter-fn))
        _ (println "min-padding:" min-padding "base-offset:" base-offset)
        next-match (fn [pad-len offset]
                     (take blocksize (drop offset (encrypter-fn (repeat (+ pad-len min-padding) 0)))))
        decrypt-byte (fn [match known-bytes]
                       (loop [guess 0]
                         (let [guess-bytes (conj known-bytes guess)
                               res (encrypter-fn (subvec guess-bytes
                                                         (- (count guess-bytes) blocksize min-padding)))]
                           (if (= (take blocksize (drop base-offset res)) match)
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
                        (loop [offset base-offset
                               known (vec (repeat (+ min-padding (dec blocksize)) 0))]
                          (if (= offset enc-length)
                            (drop (dec blocksize) known)
                            (recur (+ offset blocksize)
                                   (decode-block offset known)))))]
    (decode-buffer)))

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

(defn decode-query-string
  [query]
  (apply array-map (into [] (map #(s/split % #"=")) (s/split query #"&"))))

(defn encode-query-string
  [obj]
  (s/join "&" (map #(s/join "=" %) (partition 2 obj))))

(defn profile-for
  [email]
  (let [sani (s/replace email #"[&=]" #(case %
                                         "&" "%26"
                                         "=" "%3D"))]
    (encode-query-string ["email" sani
                          "uid" 10
                          "role" "user"])))

(defn admin-role
  [email key]
  (let [encrypter-fn (let [enc #(ecb-encrypt % key)]
                       (fn ef
                         ([] (ef nil))
                         ([bs] (enc (sequence chars->bytes* (profile-for (apply str (map byte->char bs))))))))
        {:keys [blocksize base-offset min-padding get-block]} (encrypted-block-attributes encrypter-fn)
        ^String prof (profile-for email)
        needed-len (- blocksize (mod (.. prof
                                         (substring 0 (inc (.lastIndexOf prof "=")))
                                         length)
                                     blocksize))
        pad-email (if (> needed-len 0)
                    (str "jon.distad+"
                         (apply str (repeat (dec needed-len) \a))
                         "@gmail.com")
                    email)
        encrypted-admin (get-block base-offset (pkcs7 (into (vec (repeat min-padding 0))
                                                            (map char->byte "admin"))
                                                      (+ blocksize min-padding)))]
    (concat (drop-last blocksize (encrypter-fn pad-email)) encrypted-admin)))

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
    (assert (or (zero? wrong) (> (/ right wrong) 95/100)))
    (println "Success!")))

(defn challenge12
  []
  (let [special-input (decode-base64-file "resources/12.txt")
        encrypter-fn (consistent-key-encrypter special-input)]
    (apply str (map char (decrypt-unknown encrypter-fn)))))

(defn challenge13
  []
  (let [key (random-buffer 16)
        email "jon.distad@gmail.com"
        enc (admin-role email key)
        ^String dec (apply str (map byte->char (ecb-decrypt enc key)))]
    (assert (.contains dec "&role=admin"))
    dec))

(ns cryptopals.set1
  (:require [clojure.java.io :as io])
  (:import javax.crypto.Cipher
           javax.crypto.spec.SecretKeySpec))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

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

(defmacro dbg [x]
  `((fn [y#]
      (println "Form: " '~(second &form))
      (println "Result: " y#)
      y#) ~x))

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

(defn sanitize-hex
  ^String [^String hex]
  (.toLowerCase (if (odd? (count hex))
                  (str "0" hex)
                  hex)))

(defn hex->bytes
  [^String hex]
  (sequence hex->bytes* (sanitize-hex hex)))

(defn hex->base64
  [^String hex]
  (apply str (sequence (comp hex->bytes* bytes->base64*)
                       (sanitize-hex hex))))

(defn xor-hex
  [hex1 hex2]
  (when-not (= (count hex1) (count hex2))
    (throw (IllegalArgumentException. "Unequal input lengths")))
  (apply str (sequence (comp (map xor)
                             bytes->hex*)
                       (sequence hex->bytes* (sanitize-hex hex1))
                       (sequence hex->bytes* (sanitize-hex hex2)))))

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

(defn byte-xor*
  [b]
  (map #(xor b %)))

(defn byte-xor-hex
  [^String hex b]
  (apply str (sequence (comp hex->bytes*
                             (byte-xor* b)
                             (map char))
                       (sanitize-hex hex))))

(defn best-score
  [{:keys [score] :as res} {new-scr :score :as next}]
  (if (< new-scr score)
    next
    res))

(defn score-map-hex
  [hex b]
  (let [s (byte-xor-hex hex b)]
    {:score (score-string s)
     :hex hex
     :result s
     :cipher (char b)}))

(defn score-map
  [string cipher]
  {:score (score-string string)
   :cipher cipher
   :result string})

(defn pick-string
  [^String hex]
  (let [hex (sanitize-hex hex)]
    (reduce best-score
            (for [n (range 256)]
              (score-map-hex hex n)))))

(defn find-encoded-hex
  [file]
  (let [lines (line-seq (io/reader file))
        scores (map pick-string lines)]
    (reduce best-score scores)))

(defn repeating-key-xor
  [input key]
  (map #(xor (int %1) (int %2))
       input (cycle key)))

(defn hamming-distance
  [buf1 buf2]
  (reduce #(+ %1 (Long/bitCount %2))
          0 (map #(xor %1 %2) buf1 buf2)))

(defn get-averages
  [sample-size bytes ksize]
  (letfn [(avg-dist [[key & keys :as sample]]
            (/ (reduce (fn [s k]
                         (+ s (/ (hamming-distance key k) ksize)))
                       0 keys)
               (count sample)))]
    (loop [keys (take sample-size (partition ksize bytes))
           sum 0]
      (if (next keys)
        (recur (next keys)
               (+ sum (avg-dist keys)))
        (/ sum sample-size)))))

(defn best-distance
  [[best-guess min-dist :as cur] [ks avg-dist :as next]]
  (if (< avg-dist min-dist)
    next
    cur))

(defn guess-keysize
  [sample-size bytes]
  (first (reduce best-distance
                 (map #(vector % (get-averages sample-size bytes %)) (range 2 41)))))

(defn guess-key
  [bytes]
  (let [ksize (guess-keysize 4 bytes)
        num-chunks (int (Math/ceil (/ (count bytes) ksize)))
        chunks (partition num-chunks
                          (apply interleave
                                 (partition ksize ksize (repeat nil) bytes)))
        decode (fn [bytes key]
                 (apply str (map #(char (xor key %)) bytes)))]
    (for [c chunks
          :let [c (keep identity c)]]
      (:cipher (reduce best-score {:score Double/POSITIVE_INFINITY}
                       (map #(score-map (decode c %) (char %)) (range 256)))))))

(defn decode-base64-file
  [file]
  (sequence (comp cat base64->bytes*) (line-seq (io/reader file))))

(defn break-repeating-xor
  [file]
  (let [bytes (decode-base64-file file)
        key (guess-key bytes)]
    (apply str (map char (repeating-key-xor bytes key)))))

(defn aes-ecb-decrypt
  [bytes ^String key]
  (when (not= (.length key) 16)
    (throw (IllegalArgumentException. (str "invalid key length: " (.length key) " (key: " key ")"))))
  (let [secret-key (SecretKeySpec. (.getBytes key) "AES")
        decrypt (doto (javax.crypto.Cipher/getInstance "AES/ECB/NoPadding")
                  (.init javax.crypto.Cipher/DECRYPT_MODE secret-key))]
    (apply str (sequence (comp (partition-all 16)
                               (mapcat #(.doFinal decrypt (byte-array %)))
                               (map #(char (& 0xFF %))))
                         bytes))))

(defn guess-ecb-line
  [file]
  (transduce (comp (map hex->bytes*)
                   (map #(vector % (get-averages (/ (count %) 16) % 16))))
             (completing best-distance)
             [:NO-LINE Double/POSITIVE_INFINITY]
             (line-seq (io/reader file))))

(defn challenge1
  []
  (let [in "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        expected "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
        result (hex->base64 in)]
    (assert (= expected result))
    result))

(defn challenge2
  []
  (let [in1 "1c0111001f010100061a024b53535009181c"
        in2 "686974207468652062756c6c277320657965"
        expected "746865206b696420646f6e277420706c6179"
        result (xor-hex in1 in2)]
    (assert (= expected result))
    result))

(defn challenge3
  []
  (let [in "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
        expected "Cooking MC's like a pound of bacon"
        result (:result (pick-string in))]
    (assert (= expected result))
    result))

(defn challenge4
  []
  (let [expected {:hex "7b5a4215415d544115415d5015455447414c155c46155f4058455c5b523f"
                  :result "Now that the party is jumping\n"
                  :cipher \5}
        result (find-encoded-hex "resources/4.txt")]
    (assert (= expected (select-keys result (keys expected))))
    result))

(defn challenge5
  []
  (let [in "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
        key "ICE"
        expected (str "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272"
                      "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")
        result (apply str (sequence bytes->hex* (repeating-key-xor in key)))]
    (assert (= expected result))
    result))

(defn challenge6
  []
  (println (break-repeating-xor "resources/6.txt")))

(defn challenge7
  []
  (println (aes-ecb-decrypt (decode-base64-file "resources/7.txt") "YELLOW SUBMARINE")))

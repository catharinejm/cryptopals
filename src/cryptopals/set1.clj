(ns cryptopals.set1
  (:require [clojure.java.io :as io]
            [cryptopals.utils :refer :all])
  (:import javax.crypto.Cipher
           javax.crypto.spec.SecretKeySpec))

(defn hex->base64
  [^String hex]
  (apply str (sequence (comp hex->bytes* bytes->base64*)
                       (sanitize-hex hex))))

(defn xor-hex
  [hex1 hex2]
  (apply str (sequence (comp xor-buffers* bytes->hex*)
                       (sequence hex->bytes* (sanitize-hex hex1))
                       (sequence hex->bytes* (sanitize-hex hex2)))))

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

(defn best-distance
  [[best-guess min-dist :as cur] [ks avg-dist :as next]]
  (if (< avg-dist min-dist)
    next
    cur))

(defn guess-keysize
  [sample-size bytes]
  (first (reduce best-distance
                 (map #(vector % (average-hamming-distance sample-size bytes %)) (range 2 41)))))

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

(defn break-repeating-xor
  [file]
  (let [bytes (decode-base64-file file)
        key (guess-key bytes)]
    (apply str (map char (repeating-key-xor bytes key)))))

(defn guess-ecb-line
  [file]
  (let [lines (line-seq (io/reader file))
        guess (transduce (comp (map #(sequence hex->bytes* %))
                               (map #(vector % (average-hamming-distance (/ (count %) 16) % 16))))
                         (completing best-distance)
                         [:NO-LINE Double/POSITIVE_INFINITY]
                         lines)]
    (inc (.indexOf ^java.util.List (vec lines) (apply str (sequence bytes->hex* (first guess)))))))

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
        before (System/nanoTime)
        result (find-encoded-hex "resources/4.txt")
        after (System/nanoTime)]
    (assert (= expected (select-keys result (keys expected))))
    (assoc result :time (double (/ (- after before) 1000000)))))

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
  (println (apply str
                  (ecb-decrypt (decode-base64-file "resources/7.txt") "YELLOW SUBMARINE"))))

(defn challenge8
  []
  (println "ECB line:" (guess-ecb-line "resources/8.txt"))
  (println "Probably, anyway..."))

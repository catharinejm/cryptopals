(ns cryptopals.set1
  (:require [clojure.java.io :as io]))

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

(def bytes->base64*
    (let [b64* (fn [b]
               (when (or (neg? b) (> b 63))
                 (throw (IllegalArgumentException. (str "invalid base64 number: " b))))
               (cond
                (< b 26)
                (char (+ b (int \A)))

                (< b 52)
                (char (+ (- b 26) (int \a)))

                (< b 62)
                (char (+ (- b 52) (int \0)))

                (= b 62)
                \+

                (= b 63)
                \/))
        b64 (fn [[b1 b2 b3]]
              (cond
               b3
               [(b64* (>>> b1 2))
                (b64* (| (<< (& b1 3) 4)
                         (>>> b2 4)))
                (b64* (| (<< (& b2 0xF) 2)
                         (>>> b3 6)))
                (b64* (& b3 0x3F))]

               b2
               [(b64* (>>> b1 2))
                (b64* (| (<< (& b1 3) 4)
                         (>>> b2 4)))
                (b64* (<< (& b2 0xF) 2))
                \=]

               :else
               [(b64* (>>> b1 2))
                (b64* (<< (& b1 3) 4))
                \=
                \=]))]
      (comp (partition-all 3)
            (mapcat b64))))

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

;; From http://www.data-compression.com/english.html
#_(def letter-freq
  {\e 0.0651738
   \t 0.0124248
   \a 0.0217339
   \o 0.0349835
   \i 0.1041442
   \n 0.0197881
   \s 0.0158610
   \h 0.0492888
   \r 0.0558094
   \d 0.0009033
   \l 0.0050529
   \c 0.0331490
   \u 0.0202124
   \m 0.0564513
   \w 0.0596302
   \f 0.0137645
   \g 0.0008606
   \y 0.0497563
   \p 0.0515760
   \b 0.0729357
   \v 0.0225134
   \k 0.0082903
   \j 0.0171272
   \x 0.0013692
   \q 0.0145984
   \z 0.0007836
   \space 0.1918182})

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

(defn byte-xor
  [^String hex b]
  (apply str (sequence (comp hex->bytes*
                             (map #(char (xor b %))))
                       (sanitize-hex hex))))

(defn best-score
  [{:keys [score] :as res} {new-scr :score :as next}]
  (if (< new-scr score)
    next
    res))

(defn score-map
  [hex b]
  (let [s (byte-xor hex b)]
    {:score (score-string s)
     :hex hex
     :result s
     :cipher (char b)}))

(defn pick-string
  [^String hex]
  (let [hex (sanitize-hex hex)]
    (reduce best-score
            (for [n (range 256)]
              (score-map hex n)))))

(defn find-encoded-hex
  [file]
  (let [lines (line-seq (io/reader file))
        scores (map pick-string lines)]
    (reduce best-score scores)))

(defn repeating-key-xor
  [^String input ^String key]
  (apply str (sequence (comp (map #(xor (int %1) (int %2)))
                             bytes->hex*)
                       input (cycle key))))

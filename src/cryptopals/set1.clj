(ns cryptopals.set1)

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

;; (def hex->bytes*
;;   (let [nibl (fn [chr]
;;                (case chr
;;                  (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) (- (int chr) (int \0))
;;                  (\a \b \c \d \e \f) (+ 10 (- (int chr) (int \a)))
;;                  (throw (IllegalArgumentException. (str "invalid hexidecimal character: " chr)))))
;;         to-byte (fn [h l]
;;                   (+ (* 16 (nibl h)) (nibl l)))]
;;     (fn [rf]
;;       (let [high (volatile! nil)]
;;         (fn
;;           ([] (rf))
;;           ([res] (rf res))
;;           ([res c]
;;              (if-let [h @high]
;;                (do
;;                  (vreset! high nil)
;;                  (rf res (to-byte h c)))
;;                (do
;;                  (vreset! high c)
;;                  res)))
;;           ([res c & cs]
;;              (if-let [hs (seq @high)]
;;                (do
;;                  (vreset! high nil)
;;                  (rf res (map to-byte hs (cons c cs))))
;;                (do
;;                  (vreset! high (cons c cs))
;;                  res))))))))

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
  [^String hex]
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
  (apply str (sequence (comp (map #(xor %1 %2))
                             bytes->hex*)
                       (sequence hex->bytes* (sanitize-hex hex1))
                       (sequence hex->bytes* (sanitize-hex hex2)))))

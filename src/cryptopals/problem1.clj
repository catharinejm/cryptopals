(ns cryptopals.problem1)

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

(defn hex->base64
  [^String hex]
  (let [hex (.toLowerCase (if (odd? (count hex))
                            (str "0" hex)
                            hex))
        nibl (fn [chr]
               (case chr
                 (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) (- (int chr) (int \0))
                 (\a \b \c \d \e \f) (+ 10 (- (int chr) (int \a)))
                 (throw (IllegalArgumentException. (str "invalid hexidecimal character: " chr)))))
        to-byte (fn ([[h l]]
                       (+ (* 16 (nibl h)) (nibl l))))
        b64* (fn [b]
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
    (apply str (sequence (comp (partition-all 2)
                               (map to-byte)
                               (partition-all 3)
                               (mapcat b64))
                         hex))))

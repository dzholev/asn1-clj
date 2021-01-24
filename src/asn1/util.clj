(ns asn1.util
  (:import java.nio.ByteBuffer))

(defn byte-arr-to-val
  "Converts a byte array into positive value."
  [ba]
  (.longValue (BigInteger. 1 ba)))

(defn byte-to-unsigned-int
  "Converts a byte to its unsigned integer value."
  [b]
  (bit-and 0xFF (int b)))

(defn short-value?
  "Checks if the left-most bit of a byte is 1."
  [b]
  (zero? (bit-and b 0x80)))

(defn normalized-value [b]
  "Sets the left-most bit of a byte to 0."
  (bit-and b 0x7F))

(defn get-bb-chunk-array
  "Reads a chunk of bytes from ByteBuffer and returns them into byte array."
  [bb length]
  (let [temp-array (make-array Byte/TYPE length)
        _ (.get bb temp-array)]
    temp-array))

(defn slice-bb
  "Reads a chunk of bytes from ByteBuffer into a new ByteBuffer."
  [bb length]
  (let [temp-array (get-bb-chunk-array bb length)]
    (ByteBuffer/wrap temp-array)))

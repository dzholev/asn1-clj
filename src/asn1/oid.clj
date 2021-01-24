(ns asn1.oid
  (:require [asn1.util :as u]))

(defn- acc-value
  "Converts the processed chunk into numerical value."
  [acc]
  (reduce #(bit-or (bit-shift-left %1 8) %2) 0 acc))

(defn- process-acc
  "This is the main method that performs the actual processing of a chunk,
   when it is being encoded in several bytes."
  [[result trans] acc-element]
  (let [new-trans (if (odd? acc-element) 0x80 0x00)]
   (if (u/short-value? acc-element)
    [(conj result (-> acc-element
                      (bit-or trans))) new-trans]
    [(conj result (-> (u/normalized-value acc-element)
                      (bit-shift-right 1))) new-trans])))

(defn- evaluate-acc
  "Evaluates the accumulated bytes per chunk."
  [acc]
  (acc-value (first (reduce process-acc [[] 0x00] acc))))

(defn- process-rest-bytes
  "Processes each of the OBJECT IDENTIFIER 'chunks'.
  These chunks are being accumulated and processed, depending on the incoming content bytes."
  [[acc result] b]
  (cond
    (and (u/short-value? b) (empty? acc))  [[] (conj result b)]
    (and (u/short-value? b) (seq acc)) [[] (conj result (evaluate-acc (conj acc b)))]
    :else [(conj acc b) result]))

(defn decode-rest-bytes
  "Decodes the rest of the bytes of an OBJECT IDENTIFIER."
  [ba]
  (let [bas (seq ba)
        [acc result] (reduce process-rest-bytes [[] []] bas)]
    (if (empty? acc)
      result
      (conj result (acc-value acc)))))

(defn decode-first-byte-oid
  "Decodes the first byte of an OBJECT IDENTIFIER."
  [first-byte]
  (let [first-byte-val (u/byte-to-unsigned-int first-byte)
        a (quot first-byte-val 40)
        b (rem first-byte-val 40)]
    [a b]))

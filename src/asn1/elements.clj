(ns asn1.elements
  (:require [asn1.oid :as oid]
            [asn1.util :as u])
  (:import  javax.xml.bind.DatatypeConverter))

;; Represents single ASN.1 element
(defrecord Element [position depth header-length length clss type])

;; Represents the state of the parsing engine
(defrecord State [position depth result])

(defn- read-element-type
  "Identify the ASN.1 element type.
  Throws an exception if the provided element type is not recognized."
  [b]
  (case b
    0x01 {:clss :prim :type :boolean}
    0x02 {:clss :prim :type :integer}
    0x03 {:clss :prim :type :bit-string}
    0x04 {:clss :prim :type :string}
    0x05 {:clss :prim :type :null}
    0x06 {:clss :prim :type :object-id}
    0x0C {:clss :prim :type :utf8-string}
    0x16 {:clss :prim :type :ia5-string}
    0x1E {:clss :prim :type :bpm-string}
    0x30 {:clss :cons :type :sequence}
    0x31 {:clss :cons :type :set}
    -96 {:clss :cons :type :context0}                        ; this is 0xA0
    -95 {:clss :cons :type :context1}                        ; this is 0xA1
    (throw (Exception. (str "Unknown ASN.1 element type: " b)))))

(defn- read-extended-length
  "Reads the extended content length."
  [bb length]
  (let [temp-array (u/get-bb-chunk-array bb length)]
    {:header-length (+ length 2) :length (u/byte-arr-to-val temp-array)}))

(defn- read-length
  "Reads the length of the current element."
  [bb]
  (let [b (.get bb)
        short-length (u/normalized-value b)]
    (if (u/short-value? b)
      {:header-length 2 :length short-length}
      (read-extended-length bb short-length))))

(defn- read-header
  "Reads the header (element type + lenght) of the current element."
  [bb]
  (let [element-type (read-element-type (.get bb))
        length-info (read-length bb)]
    (into element-type length-info)))

(defn- read-string-content
  "Reads the content of the current element as string.
  The provided `encoding` is used during the conversion."
  [bb length encoding]
  (let [temp-array (u/get-bb-chunk-array bb length)]
    (String. temp-array encoding)))

(defn- read-oid-content
  "Parse the content of the current element as ASN.1 OBJECT IDENTIFIER."
  [bb length]
  (let [first-chunks (oid/decode-first-byte-oid (.get bb))
        rest-chunks (oid/decode-rest-bytes (u/get-bb-chunk-array bb (dec length)))
        oid-segments (into first-chunks rest-chunks)]
    (apply str (interpose "." oid-segments))))

(defn- read-binary-content
  "Parse the content of the current element as binary.
  The result is HEX representation of the binary."
  [bb length]
  (let [temp-array (u/get-bb-chunk-array bb length)]
    (DatatypeConverter/printHexBinary temp-array)))

(defn- read-content
  "Reads the content of the current element."
  [type bb length]
  (cond
     (#{:boolean :integer :bit-string :string} type) (read-binary-content bb length)
     (= type :object-id) (read-oid-content bb length)
     (= type :ia5-string) (read-string-content bb length "US-ASCII")
     (= type :utf8-string) (read-string-content bb length "UTF-8")
     (= type :bpm-string) (read-string-content bb length "UTF-16")
     :else (throw (Exception. (str "Don't know how to handle content for type: " type)))))

(defn- read-element
  "Reads the current element."
  [{:keys [position
           depth]}
   bb]
  (if (.hasRemaining bb)
    (let [{:keys [header-length
                  length
                  clss
                  type]} (read-header bb)
          element (->Element position depth header-length length clss type)]
      (cond
        (= type :null) element
        (= clss :cons) element
        :else (assoc element :content (read-content type bb length))))))

(defn- parse-elements
  "Parses all ASN.1 elements.
  Uses recursion for traversing the collection elements and thus manages the depth information."
  [{:keys [position
           depth
           result] :as state}
   bb]
  (if-let [{:keys [header-length
                   length
                   clss] :as element-data}
           (read-element state bb)]
    (case clss
      :cons (let [cons-bb (u/slice-bb bb length)
                  temp-state (->State
                              (+ position header-length)
                              (inc depth)
                              (conj result element-data))
                  cons-state (parse-elements temp-state cons-bb)]
              (parse-elements (assoc cons-state :depth depth) bb))

      :prim (let [new-state (->State
                               (+ position header-length length)
                               depth
                               (conj result element-data))]
              (parse-elements new-state bb)))
    state))

(defn asn1-elements
  "Entry point for the ASN.1 parser."
  [bb]
  (:result (parse-elements (->State 0 0 []) bb)))
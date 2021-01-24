(ns asn1.util-test
  (:require [clojure.test :refer :all]
            [asn1.util :refer :all])
  (:import (java.nio ByteBuffer)))

(deftest test-zero-byte-arr-to-val
  (testing "Conversion of zero byte array to value."
    (let [byte-arr (make-array Byte/TYPE 0)
          computed-val (byte-arr-to-val byte-arr)]
      (is (= computed-val 0)))))

(deftest test-single-byte-arr-to-val
  (testing "Conversion of single byte array to value."
    (let [byte-arr (into-array Byte/TYPE [0x12])
          computed-val (byte-arr-to-val byte-arr)]
      (is (= computed-val 18)))))

(deftest test-byte-arr-to-val
  (testing "Conversion of byte array to value."
    (let [byte-arr (into-array Byte/TYPE [0x13 0x06])
          computed-val (byte-arr-to-val byte-arr)]
      (is (= computed-val 4870)))))

(deftest test-byte-to-unsigned-int-no-change
  (testing "Conversion of byte to its unsigned int value - no modification."
      (is (= (byte-to-unsigned-int 0x10) 16))))

(deftest test-byte-to-unsigned-int-with-change
  (testing "Conversion of byte to its unsigned int value - with modification."
    (is (= (byte-to-unsigned-int 0xFE) 254))))

(deftest test-is-short-value-false
  (testing "Check if a byte is with left-most bit 1 -> No."
    (is (complement (short-value? 0xFE)))))

(deftest test-is-short-value-true
  (testing "Check if a byte is with left-most bit 1 -> Yes."
    (is (short-value? 0x7E))))

(deftest test-normalized-value-no-change
  (testing "Set the left-most bit to 0 - no change"
    (is (= (normalized-value 0x6E) 0x6E))))

(deftest test-normalized-value-with-change
  (testing "Set the left-most bit to 0 - with change"
    (is (= (normalized-value 0xFE) 0x7E))))

(deftest test-get-bb-chunk-array
  (testing "Read a chunk of bytes from ByteBuffer."
    (let [byte-arr (into-array Byte/TYPE [0x13 0x06 0x3A 0x42 0x22])
          bb (ByteBuffer/wrap byte-arr)
          _ (.get bb)
          chunk (get-bb-chunk-array bb 3)]
      (is (= (seq chunk) [0x06 0x3A 0x42]))
      (is (.get bb) 0x22))))

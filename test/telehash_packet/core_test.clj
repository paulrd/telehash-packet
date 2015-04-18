(ns telehash-packet.core-test
  (:require [clojure.test :refer :all]
            [telehash-packet.core :refer :all]))

(deftest should-encode
  (testing "encode, check return type, check length"
    (let [json {:type "test" :foo ["bar"]}
          body (to-byte-buffer "any binary!")
          bin (encode json body)]
      (is (isa? (type bin) java.nio.ByteBuffer))
      (is (= 42 (.limit bin))))))

(deftest should-decode
  (testing "decode, check return type, content and length"
    (let [hex-str "001d7b2274797065223a2274657374222c22666f6f223a5b22626172225d7d616e792062696e61727921"
          bin (-> hex-str char-array 
                 org.apache.commons.codec.binary.Hex/decodeHex
                 java.nio.ByteBuffer/wrap)
          packet (decode bin)]
      (is (isa? (type packet) clojure.lang.IPersistentMap))
      (is (= (:type (:head packet)) "test"))
      (is (= (count (:body packet)) 11)))))

(deftest should-transpose
  (testing "decode, check return type, content and length"
    (let [hex-str "001d7b2274797065223a2274657374222c22666f6f223a5b22626172225d7d616e792062696e61727921"
          bin (-> hex-str char-array 
                 org.apache.commons.codec.binary.Hex/decodeHex
                 java.nio.ByteBuffer/wrap)
          packet (decode bin)
          body (-> packet :body byte-array java.nio.ByteBuffer/wrap)
          bin (encode (:head packet) body)]
      (is (isa? (type bin) java.nio.ByteBuffer))
      (is (= hex-str (org.apache.commons.codec.binary.Hex/encodeHexString 
                      (.array bin)))))))

(deftest should-handle-no-head
  (testing "encode/decode, return type and length; check body content"
    (let [body (to-byte-buffer "any binary!")
          bin (encode nil body)
          packet (decode bin)]
      (is (isa? (type bin) java.nio.ByteBuffer))
      (is (= 13 (.limit bin)))
      (is (empty? (:head packet)))
      (is (= (apply str (map char (:body packet))) "any binary!")))))

(deftest should-empty-head-and-body
  (testing "encode, check length"
    (let [bin (encode nil nil)]
      (is (= 2 (.limit bin))))))

(deftest should-handle-binary-head
  (testing "encode/decode: check type, length, content"
    (let [head (to-byte-buffer 0x42)
          body (to-byte-buffer "any binary!")
          bin (encode head body)
          packet (decode bin)]
      (is (isa? (type bin) java.nio.ByteBuffer))
      (is (= 14 (.limit bin)))
      (is (= 1 (count (:head packet)))))))

(deftest should-verify
  (testing "encode and verify packet"
    (let [hex-str "001d7b2274797065223a2274657374222c22666f6f223a5b22626172225d7d616e792062696e61727921"
          bin (-> hex-str char-array 
                 org.apache.commons.codec.binary.Hex/decodeHex
                 java.nio.ByteBuffer/wrap)
          packet (decode bin)]
      (is (packet? packet))
      (is (not (packet? {})))
      (is (not (packet? [])))
      (is (packet? {:head {} :body []}))
      (is (not (packet {:head nil :body []}))))))










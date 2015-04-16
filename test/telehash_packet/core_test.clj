(ns telehash-packet.core-test
  (:require [clojure.test :refer :all]
            [telehash-packet.core :refer :all]))

(deftest should-encode
  (testing "encode, check return type, check length"
    (let [json {:type "test" :foo ["bar"]}
          body (put-string "any binary!")
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
      (is (= (:type (:json packet)) "test"))
      (is (= (count (:body packet)) 11)))))

(deftest should-transpose
  (testing "decode, check return type, content and length"
    (let [hex-str "001d7b2274797065223a2274657374222c22666f6f223a5b22626172225d7d616e792062696e61727921"
          bin (-> hex-str char-array 
                 org.apache.commons.codec.binary.Hex/decodeHex
                 java.nio.ByteBuffer/wrap)
          packet (decode bin)
          body (-> packet :body byte-array java.nio.ByteBuffer/wrap)
          bin (encode (:json packet) body)]
      (is (isa? (type bin) java.nio.ByteBuffer))
      (is (= hex-str (org.apache.commons.codec.binary.Hex/encodeHexString 
                      (.array bin)))))))











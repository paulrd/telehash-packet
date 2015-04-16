(ns telehash-packet.core
  (:require [clojure.data.json :as json]
            [octet.core :as oc]
            [gloss.core :as gc]
            [gloss.io :as gio]))

(defn put-string [s]
  (gio/to-byte-buffer s))

(defn encode 
  "head and body can be nil or ByteBuffer; head can be a map (equivalent to json)"
  [head body]
  (let [fr (gc/finite-frame :uint16 (gc/string :utf-8))
        len-head (if head 
                   (if (isa? (type head) clojure.lang.IPersistentMap)
                     (gio/encode fr (json/write-str head))
                     ;; head should already be a ByteBuffer
                     (concat (gio/encode :uint16 (.limit head)) [head]))
                   (gio/encode :uint16 0))]  ; head is empty if nil / false
    (gio/contiguous (if body 
                      (concat len-head [body])
                      len-head))))

(defn decode
  "takes a ByteBuffer; see telehash.org protocol for packet format"
  [buf]
  (let [fr [(gc/finite-frame :uint16 (gc/string :utf-8)) 
            (gc/repeated :byte :prefix :none)]
        res (gio/decode fr buf)]
    {:json (-> res first (#(json/read-str % :key-fn keyword))) :body (last res)}))

(comment
  (type (byte-array (map byte "hoolo")))
  (= (Class/forName "[B") (type (byte-array (map byte "hoolo"))))
  
  (isa? (type {:hi "tru"}) PersistentArrayMap)
  (type {:hi "tru"})
  (json/write-str {:type "test" :foo ["bar"]})
  (short (char 35))

  (def fr (gc/compile-frame {:a :int16, :b :float32}))
  (def fr (gc/compile-frame :int32))

  (def ec (gio/encode :int32 3))
  (def dc (gio/decode :int32 ec))
  (-> ec first)
  dc

  (def fr (gc/compile-frame {:len :uint16}))
  (def ec (gio/encode fr {:len 60000}))
  (def dc (gio/decode fr ec))
  dc
  (.get (first ec))
  (.flip (first ec))

  (def fr (gc/compile-frame [:uint16-be (string :utf-8 :length )]))
  (def ec (gio/encode fr [60000]))
  (def dc (gio/decode fr ec))
  dc
  (.get (first ec))
  (.flip (first ec))
  
  (def fr [(gc/finite-frame :uint16 (gc/string :utf-8)) 
           (gc/repeated :byte :prefix :none)])
  (def ec (gio/encode fr ["hello" "any binary!"]))
  (def dc (gio/decode fr ec))
  dc
  ec
  (.get (first ec))
  (.get (last ec))
  (.flip (first ec))

  (let [ec (encode {:type "test" :foo ["bar"]} (put-string "any binary!"))
        fr [(gc/finite-frame :uint16 (gc/string :utf-8)) 
            (gc/repeated :byte :prefix :none)]]
    (gio/decode fr ec)
    )
  
  (def res (decode (encode {:type "test" :foo ["bar"]} (put-string "any binary!"))))
  (:body res)
  (def enc (encode {:type "test" :foo ["bar"]} (put-string "any binary!")))
  (.get enc)

  (let [hex-str "001d7b2274797065223a2274657374222c22666f6f223a5b22626172225d7d616e792062696e61727921"
        bin (-> hex-str char-array 
                org.apache.commons.codec.binary.Hex/decodeHex
                java.nio.ByteBuffer/wrap)
        packet (decode bin)]
    packet
    )
  
)



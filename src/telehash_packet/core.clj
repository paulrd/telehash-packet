(ns telehash-packet.core
  (:require [clojure.data.json :as json]
            [octet.core :as oc]
            [gloss.core :as gc]
            [gloss.io :as gio]))

(defn to-byte-buffer [s]
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
  (let [fr [(gc/header :uint16 #(if (< % 7) 
                                 (gc/compile-frame (into [] (repeat % :byte)))
                                 (gc/string :utf-8 :length %))
                       identity)
           (gc/repeated :byte :prefix :none)]
        res (gio/decode fr buf)]
    {:head (-> res first (#(when % 
                             (if (vector? %)
                               %
                               (json/read-str % :key-fn keyword)))))
     :body (last res)}))

(defn packet? [p]
  (and (map? p) (> (count p) 1) (or (vector? (:head p)) (map? (:head p)))
       (vector? (:body p))))














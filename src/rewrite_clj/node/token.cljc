(ns ^:no-doc rewrite-clj.node.token
  (:require [rewrite-clj.node.protocols :as node]))

#?(:clj (set! *warn-on-reflection* true))

;; ## Node

(defrecord TokenNode [value string-value]
  node/Node
  (tag [_] :token)
  (node-type [_n] :token)
  (printable-only? [_] false)
  (sexpr [_] value)
  (length [_] (count string-value))
  (string [_] string-value)

  Object
  (toString [this]
    (node/string this)))

(node/make-printable! TokenNode)

;; ## Constructor

(defn token-node
  "Create node for an unspecified EDN token."
  [value & [string-value]]
  (->TokenNode
    value
    (or string-value (pr-str value))))

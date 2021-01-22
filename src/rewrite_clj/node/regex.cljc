(ns ^:no-doc rewrite-clj.node.regex
  (:require [rewrite-clj.node.protocols :as node]))

#?(:clj (set! *warn-on-reflection* true))

;; ## Node

(defrecord RegexNode [pattern]
  rewrite-clj.node.protocols/Node
  (tag [_] :regex)
  (node-type [_node] :regex)
  (printable-only? [_] false)
  (sexpr [_] (list 're-pattern pattern))
  (length [_] 1)
  (string [_] (str "#\"" pattern "\"")))

(node/make-printable! RegexNode)

;; ## Constructor

(defn regex-node
  "Create node representing a regex"
  [pattern-string]
  (->RegexNode pattern-string))

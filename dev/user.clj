(ns user
  (:require [clojure.tools.reader.reader-types :as rt]
            ;The indexing reader defined below only requires the rt namespace.
            ;The following requires make it easy to use rewrite-cljc in the
            ;rest of the file
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [rewrite-clj.parser.core :as pc]
            [rewrite-clj.parser.token :as pt]
            [rewrite-clj.parser.whitespace :as ws]
            [rewrite-clj.reader :as r]
            [rewrite-clj.zip :as z] :reload-all))

(defprotocol EolNormalizingReader
  "Marker type for text readers that normalize line endings")

; An EolNormalizingReader that normalizes line endings to Linux style.

(deftype LinuxNormalizingReader
  [rdr]
  EolNormalizingReader
  rt/Reader
  (read-char [reader]
    (let [ch (rt/read-char rdr)]
      (cond
        (not (identical? ch \return)) ch
        (identical? (rt/peek-char rdr) \newline) (rt/read-char rdr)
        :else \newline)))
  (peek-char [reader]
    (let [ch (rt/peek-char rdr)]
      (if-not (identical? ch \return)
        ch
        \newline)))
  rt/IPushbackReader
  (unread [reader ch]
    (if-not (identical? ch \return)
      (rt/unread rdr ch)
      (throw (ex-info "Can't unread a carriage return"
                      {:msg "Illegal character in call to unread"
                       :ch \return}))))
  java.io.Closeable
  (close [reader]
    (when (instance? java.io.Closeable rdr)
      (.close ^java.io.Closeable rdr))))

(deftype MyIndexingPushbackReader
  [rdr ^java.util.ArrayDeque positions capacity file-name]
  rt/Reader
  (read-char [reader]
    #_(println "dmk: read-char 2")
    (let [row (rt/get-line-number reader)
          col (rt/get-column-number reader)
          ch (rt/read-char rdr)]
      (when ch
        (when (>= (.size positions) capacity)
          #_(println "popping last")
          (.removeLast positions))
        (.push positions [ch row col])
        ch)))

  (peek-char [reader]
    (rt/peek-char rdr))

  rt/IPushbackReader
  (unread [reader ch]
    (rt/unread rdr ch)
    (when ch
      (.pop positions)))

  rt/IndexingReader
  (get-line-number [reader]
    #_(println "dmk get-line-number")
    (let [[ch row _] (.peek positions)
          #_#_ _ (println "after peek")]
      (case ch
        nil 1
        \newline (inc row)
        \return (if (identical? \newline (rt/peek-char rdr)) row (inc row))
        row)))
  (get-column-number [reader]
    (let [[ch _ col] (.peek positions)]
      (case ch
        (nil \newline) 1
        \return (if (identical? \newline (rt/peek-char rdr)) (inc col) 1)
        (inc col))))
  (get-file-name [reader] file-name)

  java.io.Closeable
  (close [reader]
    (when (instance? java.io.Closeable rdr)
      (.close ^java.io.Closeable rdr))))

(defn ^java.io.Closeable indexing-push-back-reader
  "Creates an MyIndexingPushbackReader from a given string or PushbackReader"
  ([s-or-rdr]
   (indexing-push-back-reader s-or-rdr 1))
  ([s-or-rdr buf-len]
   (indexing-push-back-reader s-or-rdr buf-len nil))
  ([s-or-rdr buf-len file-name]
   (println "making index push-back rdr")
   (MyIndexingPushbackReader.
    (rt/to-pbr s-or-rdr buf-len) (java.util.ArrayDeque.) buf-len file-name)))

;if comment node includes the trailing EOL, it should allow \r\n as well for windows
;the regex below works and only allows a single EOL as before
(re-matches #"[^\r\n]*(?:\r|\n|\r\n)?" ";\r")

(comment
  (require '[clojure.tools.reader.reader-types :as rt]
           '[rewrite-clj.node :as n]
           '[rewrite-clj.parser :as p]
           '[rewrite-clj.parser.core :as pc]
           '[rewrite-clj.parser.token :as pt]
           '[rewrite-clj.parser.whitespace :as ws]
           '[rewrite-clj.reader :as r]
           '[rewrite-clj.zip :as z])
  (require '[clojure.spec.alpha :as s])
  (require '[clojure.spec.gen.alpha :as gen])
  (require '[clojure.string :as str])
  (require 'rewrite-clj.node.comment :reload)
  (gen/sample (s/gen string?))
  (let [s ";comment\r"
        c (n/comment-node s)]
    (.string c)
    ))

(def data
"(defn my-function [a]
  ;; a comment
  (* a 3))")

(def zloc (z/of-string data))

(def line-endings ["\n" "\r" "\r\n"])

(int \return)
(int \newline)
(= 0x0D0A (int \u0D0A))
\u0D0A
(s/def ::eol #{"\r" "\n" "\r\n"})
(gen/sample (s/gen ::eol))
(gen/generate (s/gen ::eol))
(gen/generate (s/gen set?))

(re-find #"\r" "h\ri")
(str/replace "\n\r\r" #"(?:\r\n|\r)" "\n")

(defn read-ch-with-pos [r]
  (let [col (rt/get-column-number r)
        line (rt/get-line-number r)
        ch (rt/read-char r)]
    [ch [line col]]))

(defn read-n
  "Read n characters from the given reader"
  ([rdr n] (read-n rdr n rt/read-char))
  ([rdr n read-ch-fn]
   (into [] (repeatedly n #(read-ch-fn rdr)))))

(defn read-all
  "Consumes all available characters of rdr"
  ([rdr] (read-all rdr rt/read-char))
  ([rdr read-ch-fn]
   (take-while (comp not nil?) (repeatedly #(read-ch-fn rdr)))))

(defn unread-vec
  "Unreads vector v of chars by repeatedly popping v until n chars have
  been unread or v is empty."
  ([rdr v] (unread-vec rdr v (count v)))
  ([rdr v n] (unread-vec rdr v n identity))
  ([rdr v n get-ch-fn]
   (loop [v v, n n]
     (if (and (seq v) (pos? n))
       (do
         (rt/unread rdr (get-ch-fn (peek v)))
         (recur (pop v) (dec n)))
       v))))

(defn read-unread
  "Consumes an input stream from a PushbackReader by repeatedly reading 1
  to n chars followed by unreading 0 to all of the chars just read until the
  stream is consumed. Returns the resulting string."
  ([rdr n] (read-unread rdr n rt/read-char identity))
  ([rdr n read-ch-fn get-ch-fn]
   (loop [v []
          steps []]
     ; (prn {:v v})
     (if (and (seq v) (nil? (get-ch-fn (peek v))))
       {:str (str/join "" (map get-ch-fn v)), :val v, :steps steps}
       (let [n (+ 1 (rand-int n))
             ;_ (println "read" n)
             v (into v (read-n rdr n read-ch-fn))
             m (rand-int (inc n))
             ;_ (println "unread" m)
             v (unread-vec rdr v m get-ch-fn)]
         (recur v (conj steps [n m])))))))

(gen/generate (gen/tuple (gen/boolean) (s/gen string?)))

(require '[clojure.test.check.properties :as prop])
(require '[clojure.test.check.generators :as tcg])
(require '[clojure.repl :refer :all])

(defn int-pair-generator [n]
  (tcg/let [a (tcg/choose 1 n)
            b (tcg/choose 0 a)]
    [a b]))

(gen/sample (s/gen #{"\r" "\n" "\r\n"}))
(gen/sample (tcg/choose 1 5))
(gen/generate (int-pair-generator 8))
(gen/sample (int-pair-generator 8))

;;; Want to generate a random sequence of line endings

;; These first two samples are fine. The two flavors of ending can happen in
;; any order.
(gen/sample (s/gen #{"\r" "\r\n"}))
(gen/sample (s/gen #{"\n" "\r\n"}))

;; For a sequence of CR and LF, all the LF chars have to be first or they
;; will get combined with a CR to make a CRLF Windows ending.
;; Below are two ways to generate a properly ordered sequence.

; Allow sequences with just one type of EOL
(gen/sample
  (gen/fmap sort (gen/vector (s/gen #{"\r" "\n"}) 1 5)))

; Guarantee at least one of each EOL type
(gen/sample
  (gen/fmap (fn [[lf cr]] (concat (repeat lf "\n") (repeat cr "\r")))
            (gen/tuple (tcg/choose 1 3) (tcg/choose 1 3))))

;; Where it gets tricky is to generate a sequence of, CR, LF, and CRLF such
;; that CR never directly precedes LF, thus making a single CRLF ending.
;; One possibility is to generate a vector of the three EOL types in any
;; order. Then use gen/fmap to sort the EOLs into a valid order.

(defn sort-eols
  "Sorts a sequence of line endings into an order such that CR and LF endings
  would never combine to make a single CRLF ending.  In a sorted result, the
  CRLF endings retain their position. Between them, all LF endings occur before
  CR endings."
  [eols-in]
  (loop [lf [], cr [], [eol & eols] eols-in]
    (if eol
      (case eol
        "\r" (recur lf (conj cr eol) eols)
        "\n" (recur (conj lf eol) cr eols)
        "\r\n" (recur (conj (into lf cr) eol) [] eols)) 
      (into lf cr))))

(gen/sample
  (gen/fmap sort-eols (gen/not-empty (gen/vector (s/gen #{"\r" "\n" "\r\n"})))))


(prop/for-all)
(unread-vec rdr [\h \e \l \l] 2)
(rt/read-char rdr)

(let [v [2 4 5 nil]]
  (and (seq v) (nil? (last v))))

;reader properties
;for any seq of next chars (including none), (= (read-char r) (peek-char r))
;for any rdr,

;for most readers, the count of characters should equal the length of the input string

(defn rand-eol-seq)

  (let [(random-sample)])
(let [n (+ 3 (rand-int 10))
      end (rand-nth line-endings)
      endings repe 
      ])
(rand-nth line-endings)
(clojure.string/join "" (repeatedly (rand-int 15) #(rand-nth line-endings)))

(def s "hello\nthere\r\ngood\rsir\r")
(def nr (->LinuxNormalizingReader (rt/to-pbr s 10)))
(def rdr (rt/indexing-push-back-reader nr 10))
(def r (indexing-push-back-reader "1\r2\n3\r\n4\n\r\r\r\n8" 10))
(def r2 (rt/indexing-push-back-reader "hi\r\n5" 10))
(rt/read-char r2)
(rt/peek-char r2)
(rt/unread r2 5)

(read-n r 7 read-ch-with-pos)
(unread-vec r (read-n r 10 read-ch-with-pos) 6 first)
(read-unread r 8 read-ch-with-pos first)

(let [q (java.util.ArrayDeque.)]
  (doto q
    (.push "hi")
    (.push "bye")
    (.push "rye")
    (.pop)
    (.pop)))

(let [v (transient [])]
  #_(peek v)
  (conj! v 1)
  #_(peek v)
  #_(peek (persistent! v))
  (pop! v)
  (peek (persistent! v)))

(rt/read-char rdr)
(rt/unread rdr \newline)
(let [ch (rt/read-char rdr)
      row (rt/get-line-number rdr)
      col (rt/get-column-number rdr)]s
  [ch row col])
(rt/peek-char rdr)


(let [s ";comment\r\n5"
      nr (->LinuxNormalizingReader (rt/to-pbr s 10))
      r (rt/indexing-push-back-reader nr 10)
      xs (p/parse-all r)
      ;r (r/string-reader ";comment\r\n5")
      ; xs (p/parse-string-all ";comment\r\n5")
      ; p1 (pc/parse-next r)
      ; p2 (pc/parse-next r)
      ; l (r/read-include-linebreak r)
      ; c1 (rt/read-char r)
      ; p2 (rt/peek-char r)
      ; c2 (rt/read-char r)
      ; t (pt/parse-token r)
      ; _ (rt/unread r c1)
      ; c1' (rt/read-char r)
      ; c2' (rt/read-char r)
      ]
  xs
  #_[[c1 p2 c2] [c1' c2']]
  #_(ws/parse-whitespace r))

(= \formfeed (first "\f"))
(-> zloc z/root-string)

(def dat ";comment\r\r\r\n")

(r/read-include-linebreak (r/string-reader dat))

(pst)
(def zloc2
  (some-> dat p/parse-string-all z/edn*)
  #_(z/of-string clj-areas))


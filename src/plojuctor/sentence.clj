;; ## Sentence Definitions

(ns plojuctor.sentence
  (:use [plojuctor common])
  (:use clojure.test)
  (:require [clojure.contrib.string :as string]
            [clojure.contrib.seq :as seq]
            [clojure.contrib.math :as math]
            ))

(def *underline-char* "~")
(def *item-char* "*")

(defn force-vector [x]
  (cond
    (vector? x) x
    (seq? x) (vec x)
    :else (vector x)))

; =defsentence
(defmacro defsentence
  "Define sentence macro.
  Generate a function which gets some arguments(last argument is string vector), and returns a string vector.

  ex. `(defsentence add-string-to-last [s v] (map #(str % v) v))`"
  [name & args]
  (let [doc-string? (-> args first string?)
        [doc-string bind & body] (if doc-string? args (cons "" args))
        flexible? (not (nil? (seq/find-first #(= % '&) bind)))
        fixed-arg (last bind)
        rest-arg (drop-last (if flexible? 2 1) bind) ]
    `(defn ~name [~@bind]
       (if ~flexible?
         (if (some (comp not vector?) ~fixed-arg)
           (recur ~@rest-arg (vec (map force-vector ~fixed-arg)))
           (force-vector (do ~@body)))
         (if (vector? ~fixed-arg)
           (force-vector (do ~@body))
           (recur ~@rest-arg (force-vector ~fixed-arg)))))))

; =string-split-at
(defn- string-split-at
  "Utility to split string"
  [n s]
  (map #(apply str %) (split-at n s)))

; =calc-center
(defn- calc-center
  "Utility to calculate space count to centerize"
  [n len]
  (if (> n len) 0
    (math/floor (/ (- len n) 2))))

; =mb-count
(defn- mb-count
  "Count string multi-byte support version (multi-byte char is counted as 2 ascii char)

  ex. `(mb-count \"abしー\")` returns 6"
  [s]
  (reduce #(+ %1 (if (= (count (.getBytes %2)) 3) 2 1)) 0 (drop 1 (string/split #"" s))))

(def blank [""])

; =lines
(defn lines
  "Concat lists, vectors or strings to a vector"
  [& x]
  (vec (flatten (map #(if (string? %) [%] (if (vector? %) % (vec %))) x))))

; =inline
(defn inline
  "Concat lists, vectors or strings to a vector wrapped string"
  [& x]
  [(apply str (flatten x))])

; =strong
(defsentence strong
  "font-weight: \"bold\""
  [v]
  (map #(str "\"" % "\"") v))

; =header
(defsentence header
  "h1 tag in html"
  [v]
  (map #(str ":" (string/replace-str " " "_" %)) v))

; =underline
(defsentence underline
  "text-decoration: underline"
  [v]
  (conj v (string/repeat (apply max (map mb-count v)) *underline-char*)))

; =padding-left
(defsentence padding-left
  "padding-left: [N]em"
  [n v]
  (let [sp (string/repeat n " ")]
    (map #(if (string/blank? %) "" (str sp %)) v)))

; =letter-space
(defsentence letter-space
  "letter-spacing: [N]em"
  [n v]
  (map #(string/join (string/repeat n " ")  (map str %)) v))
(def letter-1space (partial letter-space 1))

; =wrap
(defsentence wrap [width v]
  (flatten (map #(if (> (mb-count %) width) (string-split-at width %) %) v)))
(def wrap-page (partial wrap *width*))

; =center
(defsentence center
  "text-align: center"
  [width v]
  (map #(if (> (mb-count %) width) %
          (str (string/repeat (calc-center (mb-count %) width) " ") %)) v))
;; text-align: center (with default page width)
(def center-page (partial center *width*))

; =right
(defsentence right
  "text-align: right"
  [width v]
  (map #(if (> (mb-count %) width) %
          (str (string/repeat (- width (mb-count %)) " ") %)) v))
;; text-align: right (with default page width)
(def right-page (partial right *width*))

; =vertical-center
(defsentence vertical-center
  "vertical-align: middle"
  [height v]
  (if (> (count v) height) v
    (lines (repeat (calc-center (count v) height) blank) v)))
;; vertical-align: middle (with default page height)
(def vertical-center-page (partial vertical-center *height*))
; =middle
;; alias of vertical-center
(def middle vertical-center)
;; alias of vertical-center-page
(def middle-page vertical-center-page)

; =buttom
(defsentence bottom
  "vertical-align: bottom"
  [height base v]
  (let [len (+ (count base) (count v))]
    (if (> len height) base
      (lines base (repeat (- height len) blank) v))))
;; vertical-align: bottom (with default page height)
(def bottom-page (partial bottom *height*))

; =item
(def item-find-regexp
  (java.util.regex.Pattern/compile (str "^\\s+\\" *item-char* "\\s")))
(defn item
  "Itemize lists, vectors, or strings"
  [& x]
  (vec (map
    ;#(if (string/blank? %) % (str (if (re-find #"^\s+\*\s" %) "  " " * ") %))
    #(if (string/blank? %) % (str (if (re-find item-find-regexp %) "  " " * ") %))
    (apply lines x))))

; =code
(defmacro code
  "Convert code to vector wrapped string"
  [& exp]
  `(vector (str '~@exp)))

; =box
(defsentence box
  "Box vectors, lists, or strings"
  [v]
  (let [wmax (apply max (map count v))
        border (str "+" (string/repeat (+ 2 wmax) "-") "+") ]
    (lines
      border
      (map #(str "| " % (string/repeat (- wmax (mb-count %)) " ") " |") v)
      border)))

; =project-title
(defsentence project-title
  "Project title"
  [s]
  (lines blank (-> s letter-1space wrap-page underline center-page) blank))

; =title
(defsentence title
  "Slide title"
  [s]
  (lines (->> s letter-1space wrap-page underline (padding-left 1)) blank))


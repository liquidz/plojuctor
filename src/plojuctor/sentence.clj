;; ## Sentence Definitions

(ns plojuctor.sentence
  (:use [plojuctor common])
  (:require [clojure.contrib.string :as string]
            [clojure.contrib.math :as math]
            ))

; =defsentence
(defmacro defsentence
  "Define sentence macro.
  Generate a function which gets some arguments(last argument is string vector), and returns a string vector.

  ex. `(defsentence add-string-to-last [s v] (map #(str % v) v))`"
  [name & args]
  (let [first-arg (first args)
        [doc-or-meta bind & body] (if (or (string? first-arg) (map? first-arg)) args (cons "" args))
        fa (last bind)
        rest-arg (drop-last bind)]
    `(defn ~name [~@bind]
       (if (string? ~fa) (recur ~@rest-arg [~fa])
         (let [res# (do ~@body)]
           (if (vector? res#) res# (vec res#)))))))

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
;: font-weight: "bold"
(defsentence strong [v]
  (map #(str "\"" % "\"") v))

; =header
;; h1 tag in html
(defsentence header [v]
  (map #(str ":" (string/replace-str " " "_" %)) v))

; =underline
;;  text-decoration: underline
(defsentence underline [v]
  (conj v (string/repeat (apply max (map mb-count v)) "~")))

; =padding-left
;; padding-left: [N]em;
(defsentence padding-left [n v]
  (let [sp (string/repeat n " ")]
    (map #(if (string/blank? %) "" (str sp %)) v)))

; =letter-space
;; letter-spacing: [N]em
(defsentence letter-space [n v]
  (map #(string/join (string/repeat n " ")  (map str %)) v))
(def letter-1space (partial letter-space 1))

; =wrap
(defsentence wrap [width v]
  (flatten (map #(if (> (mb-count %) width) (string-split-at width %) %) v)))
(def wrap-page (partial wrap *width*))

; =center
;; text-align: center
(defsentence center [width v]
  (map #(if (> (mb-count %) width) %
          (str (string/repeat (calc-center (mb-count %) width) " ") %)) v))
;; text-align: center (with default page width)
(def center-page (partial center *width*))

; =right
;; text-align: right
(defsentence right [width v]
  (map #(if (> (mb-count %) width) %
          (str (string/repeat (- width (mb-count %)) " ") %)) v))
;; text-align: right (with default page width)
(def right-page (partial right *width*))

; =vertical-center
;; vertical-align: middle
(defsentence vertical-center [height v]
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
;; vertical-align: bottom
(defsentence bottom [height base v]
  (let [len (+ (count base) (count v))]
    (if (> len height) v
      (lines base (repeat (- height len) blank) v))))
;; vertical-align: bottom (with default page height)
(def bottom-page (partial bottom *height*))

; =item
(defn item
  "Itemize lists, vectors, or strings"
  [& x]
  (vec (map
    #(if (string/blank? %) % (str (if (re-find #"^\s+\*\s" %) "  " " * ") %))
    (apply lines x))))

; =code
(defmacro code
  "Convert code to vector wrapped string"
  [& exp]
  `(vector (str '~@exp)))

; =box
;; Box vectors, lists, or strings
(defsentence box [v]
  (let [wmax (apply max (map count v))
        border (str "+" (string/repeat (+ 2 wmax) "-") "+") ]
    (lines
      border
      (map #(str "| " % (string/repeat (- wmax (mb-count %)) " ") " |") v)
      border)))

; =project-title
;; Project title
(defsentence project-title [s]
  (lines blank (-> s letter-1space wrap-page underline center-page) blank))

; =title
;; Slide title
(defsentence title [s]
  (lines (->> s letter-1space wrap-page underline (padding-left 1)) blank))


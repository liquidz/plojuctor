(ns plojuctor.sentence
  (:use [plojuctor common])
  (:require [clojure.contrib.string :as string]
            [clojure.contrib.math :as math]
            ))

(defmacro defsentence [name arg & body]
  (let [fa (last arg)
        rest-arg (drop-last arg)]
    `(defn ~name [~@arg]
       (if (string? ~fa) (recur ~@rest-arg [~fa])
         (let [res# (do ~@body)]
           (if (vector? res#) res# (vec res#)))))))

(defn- string-split-at [n s]
  (map #(apply str %) (split-at n s)))

(defn- calc-center [n len]
  (if (> n len) 0
    (math/floor (/ (- len n) 2))))

(defn- mb-count [s]
  (reduce #(+ %1 (if (= (count (.getBytes %2)) 3) 2 1)) 0 (drop 1 (string/split #"" s))))



(def blank [""])

(defn lines [& x]
  (vec (flatten (map #(if (string? %) [%] (if (vector? %) % (vec %))) x))))

(defn inline [& x]
  [(apply str (flatten x))])

(defsentence strong [v]
  (map #(str "\"" % "\"") v))

(defsentence header [v]
  (map #(str ":" (string/replace-str " " "_" %)) v))

(defsentence underline [v]
  (conj v (string/repeat (apply max (map mb-count v)) "~")))

(defsentence padding-left [n v]
  (let [sp (string/repeat n " ")]
    (map #(if (string/blank? %) "" (str sp %)) v)))

(defsentence letter-space [n v]
  (map #(string/join (string/repeat n " ")  (map str %)) v))
(def letter-1space (partial letter-space 1))

(defsentence wrap [width v]
  (flatten (map #(if (> (mb-count %) width) (string-split-at width %) %) v)))
(def wrap-page (partial wrap *width*))

(defsentence center [width v]
  (map #(if (> (mb-count %) width) %
          (str (string/repeat (calc-center (mb-count %) width) " ") %)) v))
(def center-page (partial center *width*))

(defsentence right [width v]
  (map #(if (> (mb-count %) width) %
          (str (string/repeat (- width (mb-count %)) " ") %)) v))
(def right-page (partial right *width*))

(defsentence vertical-center [height v]
  (if (> (count v) height) v
    (lines (repeat (calc-center (count v) height) blank) v)))
(def vertical-center-page (partial vertical-center *height*))
(def middle vertical-center)
(def middle-page vertical-center-page)

(defsentence bottom [height base v]
  (let [len (+ (count base) (count v))]
    (if (> len height) v
      (lines base (repeat (- height len) blank) v))))
(def bottom-page (partial bottom *height*))

(defn item [& x]
  (vec (map
    #(if (string/blank? %) % (str (if (re-find #"^\s+\*\s" %) "  " " * ") %))
    (apply lines x))))

(defmacro code [& exp]
  `(vector (str '~@exp)))

(defsentence box [v]
  (let [wmax (apply max (map count v))
        border (str "+" (string/repeat (+ 2 wmax) "-") "+") ]
    (lines
      border
      (map #(str "| " % (string/repeat (- wmax (mb-count %)) " ") " |") v)
      border)))

(defsentence project-title [s]
  (lines blank (-> s letter-1space wrap-page underline center-page) blank))

(defsentence title [s]
  (lines (->> s letter-1space wrap-page underline (padding-left 1)) blank))


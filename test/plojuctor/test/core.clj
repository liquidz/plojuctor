(ns plojuctor.test.core
  (:use [plojuctor core sentence] :reload-all)
  (:use [clojure.test]))

(defmacro deftest= [test-name & test-pairs]
  `(deftest ~test-name
     (are [x# y#] (= x# y#)
       ~@test-pairs)))

(defsentence id [v] v)
(defsentence multi [n v] (map #(* % n) v))
(defsentence upper [& v]
  (map #(.toUpperCase %) (lines v)))

(deftest test-defsentence
  (are [x y] (= x y)
    [1] (id [1])
    [1] (id 1)

    [2] (multi 2 [1])
    [2] (multi 2 1)
    [2 4 6] (multi 2 [1 2 3])
    [2 4 6] (multi 2 '(1 2 3))

    ["A"] (upper ["a"])
    ["A" "B"] (upper ["a"] ["b"])
    ["A" "B"] (upper [["a"] ["b"]])
    ["A"] (upper "a")
    ["A" "B"] (upper "a" "b")))

(deftest test-lines
  (are [x y] (= x y)
    ["a" "b"] (lines "a" "b")
    ["a" "b"] (lines "a" (lines "b"))))

(deftest test-inline
  (are [x y] (= x y)
    ["a"] (inline "a")
    ["ab"] (inline "a" "b")
    ["ab"] (inline "a" (inline "b"))))

(deftest= test-strong
  ["\"a\""] (strong "a"))

(deftest= test-header
  [":a"] (header "a")
  [":a_b"] (header "a b"))

(deftest= test-underline
  ["a" *underline-char*] (underline "a"))

(deftest= test-padding-left
  ["  a"] (padding-left 2 "a"))

(deftest= test-lettr-space
  ["a  b  c"] (letter-space 2 "abc")
  ["a b c"] (letter-1space "abc"))

(deftest= test-wrap
  ["a"] (wrap 2 "a")
  ["ab" "cd"] (wrap 2 "abcd"))

(deftest= test-center
  ["abcd"] (center 4 "abcd")
  [" ab"] (center 4 "ab"))

(deftest= test-right
  ["ab"] (right 2 "ab")
  ["  ab"] (right 4 "ab"))

(deftest= test-vertical-center
  ["ab"] (vertical-center 1 "ab")
  ["" "ab"] (vertical-center 3 "ab"))

(deftest= test-bottom
  ["ab"] (bottom 1 [] "ab")
  ["ab"] (bottom 1 ["ab"] "cd")
  ["" "" "ab"] (bottom 3 [] "ab")
  ["ab" "" "cd"] (bottom 3 ["ab"] "cd"))

(deftest test-item
  (are [space x y] (= (vector (str space *item-char* " " x)) y)
    " " "a" (item "a")
    "   " "a" (item (item "a"))))

(deftest= test-code
  ["(+ 1 2)"] (code (+ 1 2)))

(deftest= test-box
  ["+---+" "| a |" "+---+"] (box "a"))


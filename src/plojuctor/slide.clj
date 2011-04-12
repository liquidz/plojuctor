;; ## Slide Utilities

(ns plojuctor.slide
  (:use plojuctor.common
        [plojuctor.sentence :only [lines]])
  (:require [clojure.contrib.string :as string]
            [clojure.contrib.math :as math]
            ))

(def slides (ref []))
(def page (atom -1))

; =init-slides!
(defn init-slides!
  "Make defined slides empty.
  This is automatically called by plojuctor.core, so you do not need to call this function."
  []
  (dosync (ref-set slides [])))

; =slide
(defmacro slide
  "Define slide macro

    (slide
      (title \"slide title\")
      \"slide contents\")"
  [& x]
  `(dosync
    (ref-set slides (conj @slides (lines ~@x)) )))

; =page-border
(def page-border (string/repeat (math/floor (/ *width* 3)) " .."))

; =print-page
(defn- print-page
  "Print specified slide

   * `v` is a vector which contains slide data
   * `border?` is a boolean for showing page border"
  [v & {:keys [border?] :or {border? true}}]
  (if border? (println page-border))
  (doseq [s v] (println s))
  (when border?
    (dotimes [_ (- *height* (count v))] (println ""))
    (println page-border)))

; =show-page
(defn show-page
  "Print current page of slides and page number"
  []
  (print-page (nth @slides @page))
  (str (inc @page) " / " (count @slides)))



(ns plojuctor.slide
  (:use plojuctor.common
        [plojuctor.sentence :only [lines]])
  (:require [clojure.contrib.string :as string]
            [clojure.contrib.math :as math]
            ))

(def slides (ref []))
(def page (atom -1))

(defn init-slides! []
  (dosync (ref-set slides [])))

(defmacro defslide [& x]
  `(dosync
    (ref-set slides (conj @slides (lines ~@x)) )))

(def page-border (string/repeat (math/floor (/ *width* 3)) " .."))

(defn print-page [v & {:keys [border?] :or {border? true}}]
  (if border? (println page-border))
  (doseq [s v] (println s))
  (when border?
    (dotimes [_ (- *height* (count v))] (println ""))
    (println page-border)))

(defn show-page []
  (print-page (nth @slides @page))
  (str (inc @page) " / " (count @slides))
  )



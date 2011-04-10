(ns plojuctor.core
  (:use [plojuctor slide sentence])
  (:require [clojure.contrib.string :as string]))

; controller {{{
(defn move-page! [n]
  (reset! page n)
  (show-page))

(defn next-page! []
  (when (< @page (-> @slides count dec))
    (swap! page inc)
    (show-page)))

(defn prev-page! []
  (when (> @page 0)
    (swap! page dec)
    (show-page)))

(init-slides!)
; }}}

(defmacro container [& sentences]
  `(padding-left 2 (lines ~@sentences)))

(defslide
  (middle-page
    (lines
      (project-title "Plojuctor")
      (center-page (strong "presentation tool in clojure repl"))
      (repeat 2 blank)
      (center-page
        (inline (code (prev-page!)) " / " (code (next-page!)) " でスライド移動")))))

(defslide
  (title "Plojuctorとは？")
  (container
    (item "Clojure REPL上で動作するプレゼンテーションツール" blank

          (inline (strong "here comes clojure") " をベースとしています")
          (item "http://bit.ly/hU4qDx") blank

          "VimClojure上で動作させるとカラースキームによって色付け可能" blank

          "センテンスを自由に定義することでカスタマイズ可能")))

(defslide
  (title "センテンス")
  (container
    (header "underline")
    (code (underline "下線付きテキスト"))
    (underline "下線付きテキスト")
    blank

    (header "letter spacing")
    (code (letter-space 1 "字間1スペーステキスト"))
    (letter-space 1 "字間1スペーステキスト")
    blank

    (header "strong")
    (code (strong "強調表示 (VimClojure上のみ)"))
    (strong "強調表示 (VimClojure上のみ)")
    blank

    "などなど"))

(defslide
  (middle-page
    (center-page
      (letter-space 2 (lines "Clojureで" "プレゼンしよう!")))))




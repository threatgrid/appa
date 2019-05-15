(ns appa.core
  (:require [instaparse.core :as insta :refer [defparser] :include-macros true]))

(def grammar (slurp "resources/jqt.grammar"))

(defparser jqt grammar)

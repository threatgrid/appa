(ns appa.core
  (:require [instaparse.core :as insta :refer [defparser] :include-macros true]))

(def grammar (slurp "resources/jqt.grammar"))

(defparser jqt grammar)

(defn ->path
  [[tag & args]]
  (if (= :identity tag)
    :identity
    (into {:type tag} args)))

(defn ->expression
  [[[t & path] [vt [lt var-label]]]]
  (assert (= t :path))
  (let [exp {:path (mapv ->path path)}]
    (if var-label
      (do
        (assert (= [:variable :label] [vt lt]))
        (assoc exp :variable var-label))
      exp)))


(defn query->structure
  [[tag & args]]
  (case tag
    :pipe nil
    :expression (->expression args)))

(defn where-clause
  [query-text]
  (let [[tag & query] (jqt query-text) ]
    (assert (= tag :query))
    (keep query->structure query)
  ))

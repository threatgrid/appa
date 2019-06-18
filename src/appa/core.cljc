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
  (println "making an expression: " path)
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

(defn jq-text->structure
  [query-text]
  (let [[tag & query] (jqt query-text) ]
    (assert (= tag :query))
    (keep query->structure query)))

(defn term->datalog
  "Return:  [Var [triples]]"
  [latest-var term]
  (if-not latest-var  ;; no var means this is a top level query
    (cond
      ;; . or [] both mean every top level entity
      (or (= :identity term) (= {:type :index-expression} term))
      (let [v (gensym "?v")]
        [v [[v :naga/entity true]]])

      ;; the only other option is an index term
      (= :index-expression (:type term))
      (if-let [n (:number term)]
        ;; index by n is invalid at the top level
        (throw (ex-info "Top level items are unordered" {:term term}))
        (if-let [f (:quoted-string term)]
          ;; index by term
          (let [v (gensym "?v")
                f' (gensym "?v")]
            [v [[v :naga/entity true] [v (keyword f) f']]])
          (throw (ex-info "Unknown index type" {:term term}))))
      
      :default
      (throw (ex-info "Unknown root term" {:term term})))
    (cond
      ;; . or [] both mean every top level entity
      (or (= :identity term) (= {:type :index-expression} term))
      [latest-var []]

      ;; the only other option is an index term
      (= :index-expression (:type term))
      (if-let [n (:number term)]
        (loop [i 0, tail latest-var, exp []]
          (if (= i n)
            (let [entry (gensym "?e")]
              [entry (conj exp [tail :naga/value entry])])
            (let [ntail (gensym "?l")]
              (recur (inc i) ntail (conj exp [tail :naga/rest ntail])))))
        (if-let [f (:quoted-string term)]
          ;; index by term
          (let [v (gensym "?v")
                f' (gensym "?v")]
            [v [[v (keyword f) f']]])
          (throw (ex-info "Unknown index type" {:term term}))))
      
      :default
      (throw (ex-info "Unknown term" {:term term}))
      )))

(defn query-structure->datalog
  [query]
  (let [[_ dl] (reduce (fn [[latest-var datalog] term]
                         (let [[v d] (term->datalog latest-var term)]
                           [v (concat datalog d)]))
                       [nil []] query)]
    dl))

(defn jq->graph-query
  [jq-text]
  (let [jq (jq-text->structure jq-text)]
    (query-structure->datalog jq)))

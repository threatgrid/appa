(ns appa.core
  (:require [instaparse.core :as insta :refer [defparser] :include-macros true]
            [clojure.pprint :refer [pprint]]))

(def grammar (slurp "resources/jqt.grammar"))

(defparser jqt grammar)

(defn to-long
  [n]
  (if (number? n)
    n
    #?(:clj (Long/parseLong n)
       :cljs (js/parseInt n))))

(defn ->path
  [[tag & args]]
  (case tag
    :identity :identity
    :label {:type :label :value (first args)}
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

(defn jq-text->structure
  [query-text]
  (let [[tag & query] (jqt query-text) ]
    (assert (= tag :query))
    (keep query->structure query)))

(defn term->datalog
  "Return:  [Var [triples]]"
  [latest-var term]
  (if-not latest-var ;; no var means this is a top level query
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
        (let [n (to-long n)]
          (loop [i 0, tail latest-var, exp []]
            (if (= i n)
              (let [entry (gensym "?e")]
                [entry (conj exp [tail :naga/value entry])])
              (let [ntail (gensym "?l")]
                (recur (inc i) ntail (conj exp [tail :naga/rest ntail]))))))
        (if-let [f (:quoted-string term)]
          ;; index by term
          (let [v (gensym "?v")
                f' (gensym "?v")]
            [v [[v (keyword f) f']]])
          (throw (ex-info "Unknown index type" {:term term}))))

      (= :label (:type term))
      (let [v (gensym "?v")
            property (keyword (:value term))]
        [v [[latest-var property v]]])
      
      :default
      (throw (ex-info "Unknown term" {:term term}))
      )))

(defn rewrite-var
  [nvar var statement]
  (mapv #(if (= nvar %) var %) statement))

(defn process-term
  [latest-var {:keys [path variable] :as term}]
  (let [[next-var datalog] (reduce (fn [[lv acc] e]
                                     (let [[nv d] (term->datalog lv e)]
                                       [nv (concat acc d)]))
                                   [latest-var []]
                                   path)]
    (if variable
      (let [v (symbol (str \? variable))]
        [v (map (partial rewrite-var next-var v) datalog)])
      [next-var datalog])))

(defn query-structure->datalog
  [query]
  (let [[selected dl] (reduce (fn [[latest-var datalog] term]
                         (let [[v d] (process-term latest-var term)]
                           [v (concat datalog d)]))
                       [nil []] query)]
    [selected dl]))

(defn jq->graph-query
  [jq-text]
  (try
    (let [jq (jq-text->structure jq-text)]
      (query-structure->datalog jq))
    (catch Exception e
      (println "ERROR: " (ex-message e))
      (pprint (:term (ex-data e)))
      (.printStackTrace e))))

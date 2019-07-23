(ns appa.core
  (:require [instaparse.core :as insta :refer [defparser] :include-macros true]
            [asami.core :as asami]
            [naga.data :as data]
            [clojure.pprint :refer [pprint]]))

;(def grammar (slurp "resources/jqt.grammar"))

(def grammar
"query = expression ( <ws> pipe <ws> expression)*
expression = path [<ws> <'as'> <ws> variable]
path = root? obj-identifier*
<root> = identity | index-expression
pipe = <'|'>
identity = <'.'>
<obj-identifier> = (<'.'> label) | index-expression
index-expression = <'['> index? <']'>
<index> = number | quote-string | variable
variable = <'$'> label
label = #'[a-zA-Z_][a-zA-Z0-9_]*'

number = #'[0-9]+'
<quote-string> = <'\"'> quoted-string <'\"'>
quoted-string = #'([^\\\\]+\\\\\\\")*[^\\\"]*'
ws = #'\\s*'")

(defparser jqt grammar)

(defn to-long
  "Converts a parameter that is either a string containing a number or a number into number"
  [n]
  (if (number? n)
    n
    #?(:clj (Long/parseLong n)
       :cljs (js/parseInt n))))

;; Definitions:
;; CST: Concrete Syntax Tree. Raw output from the parser.
;; AST: Abstract Syntax Tree. Semantic representation of what was parsed.

(defn ->path
  "Convert a CST path element into an AST path element"
  [[tag & args]]
  (case tag
    :identity :identity
    :label {:type :label :value (first args)}
    (into {:type tag} args)))

(defn ->expression
  "Convert a CST path element with possible var binding into AST form"
  [[[t & path] [vt [lt var-label]]]]
  (assert (= t :path))
  (let [exp {:path (mapv ->path path)}]
    (if var-label
      (do
        (assert (= [:variable :label] [vt lt]))
        (assoc exp :variable var-label))
      exp)))


(defn query->structure
  "Convert a single CST term into AST form"
  [[tag & args]]
  (case tag
    :pipe nil
    :expression (->expression args)))

(defn jq-text->structure
  "Parse a jq string into a query AST"
  [query-text]
  (let [[tag & query] (jqt query-text) ]
    (assert (= tag :query) (str "bad: tag=" tag "; query=" query))
    (keep query->structure query)))

(defn term->datalog
  "Converts a single term into a seq of constraints and a single var that the constraints will bind.
  Return:  [Var [triples]]"
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
            [f' [[v :naga/entity true] [v (keyword f) f']]])
          (throw (ex-info "Unknown index type" {:term term}))))
      
      :default
      (let [v (gensym "?v")
            v2 (gensym "?v2")
            property (keyword (:value term))]
        [v2 [[v :naga/entity true] [v property v2]]]))
    (cond
      ;; . or [] both mean every top level entity
      (or (= :identity term) (= {:type :index-expression} term))
      [latest-var []]

      ;; the only other option is an index term
      (= :index-expression (:type term))
      (if-let [n (:number term)]
        (let [n (to-long n)]
          (loop [i 0, tail latest-var, exp []]
            (if (>= i n)
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
      (throw (ex-info "Unknown term" {:term term})))))

(defn rewrite-var
  "Changes all instances of nvar in a statement into var"
  [nvar var statement]
  (mapv #(if (= nvar %) var %) statement))

(defn process-term
  "Converts a single term from a jq query into a set of constraint operations.
   Individual terms are separated by | characters.
   Return a pair of the variable bound by this term, and the constraints that bind it."
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
  "Convert a CST for a query into equivalent datalog"
  [query]
  (let [[selected dl] (reduce (fn [[latest-var datalog] term]
                         (let [[v d] (process-term latest-var term)]
                           [v (concat datalog d)]))
                       [nil []] query)]
    [selected dl]))

(defn jq->graph-query
  "Convert a jq query in text form into a graph query structural form.
   TODO: bind the current store, and use this to determine array lengths when doing negative indexing"
  [jq-text]
  (try
    (let [jq (jq-text->structure jq-text)
          [var where] (query-structure->datalog jq)
          vars (keep :variable jq)
          find-vars (cons var (remove #{var} vars))]
      {:find find-vars
       :where where})
    (catch Exception e
      (println "ERROR: " (ex-message e))
      (pprint (:term (ex-data e)))
      (.printStackTrace e))))

(defn load-objs
  "Retrieves a sequence of objects from a store"
  [store refs]
  (map (partial data/id->json store) refs))

(defn query-objects
  "Executes a jq query against a store, and returns a sequence of the resulting objects.
   If the query requests a single variable, then a sequence of objects is returned.
   If multiple vars are defined, then sequences of the associated objects in order will be returned."
  [store jquery]
  (let [{:keys [find where]} (jq->graph-query jquery)
        query (-> [:find] (into find) (conj :where) (into where))
        refs-list (asami/q query store)
        objs (map (partial load-objs store) refs-list)]
    (if (= 1 (count find))
      {:results (map first objs)}
      {:results (map first objs)
       :vars (rest find)
       :bindings (map rest objs)})))

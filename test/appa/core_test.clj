(ns appa.core-test
  (:require [clojure.test :refer :all]
            [appa.core :refer :all]
            [qtest.core :refer [with-fresh-gen] :include-macros true]
            [asami.core :as asami]))

(deftest test-load
  (testing "Testing that basic loading/querying works"
    (let [store (asami/create-store {})]
      )))

(deftest parse-test
  (let [cst (jqt ".")]
    (is (= cst [:query [:expression [:path [:identity]]]])))
  (let [cst (jqt "[]")]
    (is (= cst [:query [:expression [:path [:index-expression]]]])))

  (let [cst (jqt ".foo")]
    (is (= cst [:query [:expression [:path [:label "foo"]]]])))

  (let [cst (jqt "[] as $x | [0]")]
    (is (= cst [:query
                [:expression [:path [:index-expression]] [:variable [:label "x"]]]
                [:pipe]
                [:expression [:path [:index-expression [:number "0"]]]]])))

  (let [cst (jqt "[] as $x | [1]")]
    (is (= cst [:query
                [:expression [:path [:index-expression]] [:variable [:label "x"]]]
                [:pipe]
                [:expression [:path [:index-expression [:number "1"]]]]])))

  (let [cst (jqt "[\"a\"] as $x | [0]")]
    (is (= cst [:query
                [:expression [:path [:index-expression [:quoted-string "a"]]]
                             [:variable [:label "x"]]]
                [:pipe]
                [:expression [:path [:index-expression [:number "0"]]]]])))

  (let [cst (jqt "[].foo[2] as $x | [0]")]
    (is (= cst [:query
                [:expression [:path [:index-expression]
                              [:label "foo"]
                              [:index-expression [:number "2"]]]
                             [:variable [:label "x"]]]
                [:pipe]
                [:expression [:path [:index-expression [:number "0"]]]]]))))


(deftest compile-test
  (let [gquery (with-fresh-gen (jq->graph-query "."))]
    (is (= gquery '{:find (?v1), :where ([?v1 :naga/entity true])})))
  (let [gquery (with-fresh-gen (jq->graph-query "[]"))]
    (is (= gquery '{:find (?v1), :where ([?v1 :naga/entity true])})))

  (let [gquery (with-fresh-gen (jq->graph-query ".foo"))]
    (is (= gquery '{:find (?v22), :where ([?v1 :naga/entity true] [?v1 :foo ?v22])})))

  (let [gquery (with-fresh-gen (jq->graph-query "[] as $x | [0]"))]
    (is (= gquery '{:find (?e2 "x"), :where ([?x :naga/entity true] [?x :naga/first ?e2])})))

  (let [gquery (with-fresh-gen (jq->graph-query "[] as $x | [1]"))]
    (is (= gquery '{:find (?e3 "x"), :where ([?x :naga/entity true] [?x :naga/rest ?l2] [?l2 :naga/first ?e3])})))

  (let [gquery (with-fresh-gen (jq->graph-query "[\"a\"] as $x | [0]"))]
    (is (= gquery '{:find (?e3 "x"), :where ([?v1 :naga/entity true] [?v1 :a ?x] [?x :naga/first ?e3])})))

  (let [gquery (with-fresh-gen (jq->graph-query "[].foo[2] as $x | [0]"))]
    (is (= gquery '{:find (?e6 "x"), :where ([?v1 :naga/entity true] [?v1 :foo ?v2] [?v2 :naga/rest ?l3] [?l3 :naga/rest ?l4] [?l4 :naga/first ?x] [?x :naga/first ?e6])})))

  (let [gquery (with-fresh-gen (jq->graph-query "[].foo[].bar as $x | [0]"))]
    (is (= gquery '{:find (?e5 "x"), :where ([?v1 :naga/entity true] [?v1 :foo ?v2] [?v2 :naga/contains ?l3] [?l3 :bar ?x] [?x :naga/first ?e5])})))

  (let [gquery (with-fresh-gen (jq->graph-query "[][\"foo\"][].bar as $x | [0]"))]
    (is (= gquery '{:find (?e5 "x"), :where ([?v1 :naga/entity true] [?v1 :foo ?v2] [?v2 :naga/contains ?l3] [?l3 :bar ?x] [?x :naga/first ?e5])}))))

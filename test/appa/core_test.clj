(ns appa.core-test
  (:require [clojure.test :refer :all]
            [appa.core :refer :all]
            [asami.core :as asami]))

(deftest test-load
  (testing "Testing that basic loading/querying works"
    (let [store (asami/create-store {})]
      )))

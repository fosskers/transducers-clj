(ns transducers.core-test
  (:require [transducers.core :as t]
            [clojure.test :refer [deftest is]]))

(deftest count-test
  (is (= 4 (transduce t/pass t/count [1 2 3 4]))))

(deftest average-test
  (is (= 3 (transduce t/pass (t/average -1) [1 2 3 4 5]))))

(deftest any-test
  (is (not (transduce t/pass (t/any #(zero? (mod % 2))) [1 3 5 7])))
  (is (transduce t/pass (t/any #(zero? (mod % 2))) [1 3 5 7 2])))

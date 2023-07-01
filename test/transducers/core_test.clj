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

(deftest all-test
  (is (transduce t/pass (t/all #(= 3 (count %))) ["abc" "def" "ghi"]))
  (is (not (transduce t/pass (t/all #(= 3 (count %))) ["abc" "de" "ghi"]))))

(deftest first-test
  (is (= 6 (transduce (filter #(zero? (mod % 2))) (t/first 0) [1 3 5 6 9]))))

(deftest last-test
  (is (= 10 (transduce t/pass (t/last 0) [2 4 6 7 10]))))

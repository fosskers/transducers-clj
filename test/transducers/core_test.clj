(ns transducers.core-test
  (:require [transducers.core :as t]
            [clojure.test :refer [deftest is]]))

(deftest count-test
  (is (= 4 (transduce (map identity) t/count [1 2 3 4]))))

(deftest average-test
  (is (= 3 (transduce (map identity) (t/average -1) [1 2 3 4 5]))))

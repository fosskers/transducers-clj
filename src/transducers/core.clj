(ns transducers.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

;; --- Transducers --- ;;

(defn window
  "Yield `n`-length windows of overlapping values. This is different from
  `partition-all` which yields non-overlapping windows. If there were fewer
  items in the input than `n`, then this yields nothing."
  [n]
  (fn [reducer]
    (let [queue (atom [])]
      (fn
        ([result] (reducer result))
        ([result input]
         (swap! queue conj input)
         (let [len (clojure.core/count @queue)]
           (cond (< len n) result
                 (= len n) (reducer result @queue)
                 :else (do (swap! queue subvec 1)
                           (reducer result @queue)))))))))

(comment
  (transduce (window 4) conj [1 2 3 4 5 6]))

(defn scan
  "Build up successive values from the results of previous applications of a
  given function `f`. A `seed` is also given, and appears as the first element
  passed through the transduction."
  [f seed]
  (fn [reducer]
    (let [prev (atom seed)]
      (fn
        ([result]
         (let [result (reducer result @prev)]
           (if (reduced? result)
             (reducer @result)
             (reducer result))))
        ([result input]
         (let [old @prev
               result (reducer result old)]
           (if (reduced? result)
             result
             (let [new (f @prev input)]
               (swap! prev (constantly new))
               result))))))))

(defn pass
  "Just pass along each value of the transduction without transforming."
  [reducer]
  (fn
    ([result] (reducer result))
    ([result input] (reducer result input))))

(defn csv
  "Interprets the data stream as CSV data. The first item found is assumed to be
  the header list, and it will be used to construct useable maps for all
  subsequent items."
  [reducer]
  (let [headers (atom nil)]
    (fn
      ([result] (reducer result))
      ([result input]
       (if @headers
         (reducer result (zipmap @headers input))
         (do (swap! headers (constantly input))
             result))))))

(comment
  (with-open [reader (clojure.java.io/reader "foo.csv")]
    (transduce (comp csv
                     (map #(select-keys % ["Name" "Age"])))
               conj (clojure.data.csv/read-csv reader))))

;; --- Reducers --- ;;

(defn count
  "Count the number of elements that made it through the transduction."
  ([] 0)
  ([acc] acc)
  ([acc _] (inc acc)))

(defn average
  "Calculate the average value of all numeric elements in a transduction. A
  `fallback` must be provided in case no elements made it through the
  transduction (thus protecting from division-by-zero)."
  [fallback]
  (let [items (atom 0)]
    (fn
      ([] 0)
      ([acc] (if (zero? @items)
               fallback
               (/ acc @items)))
      ([acc input] (swap! items inc)
                   (+ acc input)))))

(defn any
  "Yield `true` if any element in the transduction satisfies `pred`.
  Short-circuits the transduction as soon as the condition is met."
  [pred]
  (fn
    ([] false)
    ([acc] acc)
    ([_ input] (if (pred input)
                 (reduced true)
                 false))))

(defn all
  "Yield `true` if all elements of the transduction satisfy `pred`. Short-circuit
  with `false` if any element fails the test."
  [pred]
  (fn
    ([] true)
    ([acc] acc)
    ([acc input] (if (and acc (pred input))
                   true
                   (reduced false)))))

(defn first
  "Yield the first value of the transduction, or the `fallback` if there were none."
  [fallback]
  (fn
    ([] fallback)
    ([acc] acc)
    ([_ input] (reduced input))))

(defn last
  "Yield the final value of the transduction, or the `fallback` if there were none."
  [fallback]
  (fn
    ([] fallback)
    ([acc] acc)
    ([_ input] input)))

(defn find
  "Find the first element in the transduction that satisfies a given `pred`.
  Yields `nil` if no such element were found."
  [pred]
  (fn
    ([] nil)
    ([acc] acc)
    ([_ input] (if (pred input)
                 (reduced input)
                 nil))))

(defn for-each
  "Run through every item in a transduction for their side effects but throw
  away all results. Yields `nil`."
  ([] nil)
  ([_] nil)
  ([_ _] nil))

(comment
  (transduce (map println) for-each [1 2 3]))

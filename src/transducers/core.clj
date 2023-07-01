(ns transducers.core)

;; --- Transducers --- ;;

(defn pass
  "Just pass along each value of the transduction without transforming."
  [reducer]
  (fn
    ([result] (reducer result))
    ([result input] (reducer result input))))

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

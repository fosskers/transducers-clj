(ns transducers.core)

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


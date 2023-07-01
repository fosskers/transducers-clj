(ns transducers.core)

;; --- Reducers --- ;;

(defn count
  "Count the number of elements that made it through the transduction."
  ([] 0)
  ([acc] acc)
  ([acc _] (inc acc)))

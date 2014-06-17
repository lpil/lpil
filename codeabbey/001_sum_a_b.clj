(def input "4299613 7512993")

(apply + (map #(Integer/parseInt %) (clojure.string/split input #" ")))

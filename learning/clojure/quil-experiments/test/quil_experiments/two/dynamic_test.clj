(ns quil-experiments.two.dynamic-test
  (:use clojure.test
        quil-experiments.two.dynamic))

(defn contains-correct-state-keys? [state]
  (is (contains? state :slow-osc))
  (is (contains? state :fast-osc))
  (is (contains? state :colour))
  (is (contains? state :size)))

(deftest starting-state-contains-correct-keys
  (contains-correct-state-keys? starting-state))

(deftest update-returns-state-with-correct-keys
  (contains-correct-state-keys? (update starting-state)))

(run-tests)

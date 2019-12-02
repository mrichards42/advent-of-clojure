(ns advent.2019-test
  (:require [clojure.test :refer :all]
            [advent.util :as util]
            [advent-2019.day01 :as day01]
            ))

(deftest day01-test
  (is (= 3421505 (util/run day01/part1)))
  (is (= 5129386 (util/run day01/part2))))

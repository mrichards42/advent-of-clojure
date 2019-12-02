(ns advent.2019-test
  (:require [clojure.test :refer :all]
            [advent.util :as util]
            [advent-2019.day01 :as day01]
            [advent-2019.day02 :as day02]
            ))

(deftest day01-test
  (is (= 3421505 (util/run day01/part1)))
  (is (= 5129386 (util/run day01/part2))))

(deftest day02-test
  (is (= 6568671 (util/run day02/part1)))
  (is (= 3951 (util/run day02/part2))))
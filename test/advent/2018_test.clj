(ns advent.2018-test
  (:require [clojure.test :refer :all]
            [advent.util :as util]
            [advent-2018.day01 :as day01]
            [advent-2018.day02 :as day02]
            [advent-2018.day03 :as day03]
            [advent-2018.day04 :as day04]
            [advent-2018.day05 :as day05]
            [advent-2018.day06 :as day06]
            ))

(deftest day01-test
  (is (= 406 (util/run day01/part1)))
  (is (= 312 (util/run day01/part2))))

(deftest day02-test
  (is (= 6972 (util/run day02/part1)))
  (is (= "aixwcbzrmdvpsjfgllthdyoqe" (util/run day02/part2))))

(deftest day03-test
  (is (= 109143 (util/run day03/part1)))
  (is (= 506 (util/run day03/part2))))

(deftest day04-test
  (is (= 85296 (util/run day04/part1)))
  (is (= 58559 (util/run day04/part2))))

(deftest day05-test
  (is (= 9562 (util/run day05/part1)))
  (is (= 4934 (util/run day05/part2))))

(deftest day06-test
  (is (= 3449 (util/run day06/part1)))
  (is (= 44868 (util/run day06/part2))))

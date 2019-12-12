(ns advent.2019-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [advent.util :as util]
            [advent-2019.day01 :as day01]
            [advent-2019.day02 :as day02]
            [advent-2019.day03 :as day03]
            [advent-2019.day04 :as day04]
            [advent-2019.day05 :as day05]
            [advent-2019.day06 :as day06]
            [advent-2019.day07 :as day07]
            [advent-2019.day08 :as day08]
            [advent-2019.day09 :as day09]
            ))

(deftest day01-test
  (is (= 3421505 (util/run day01/part1)))
  (is (= 5129386 (util/run day01/part2))))

(deftest day02-test
  (is (= 6568671 (util/run day02/part1)))
  (is (= 3951 (util/run day02/part2))))

(deftest day03-test
  (is (= 375 (util/run day03/part1)))
  (is (= 14746 (util/run day03/part2))))

(deftest day04-test
  (is (= 1929 (day04/part1)))
  (is (= 1306 (day04/part2))))

(deftest day05-test
  (is (= 10987514 (util/run day05/part1)))
  (is (= 14195011 (util/run day05/part2))))

(deftest day06-test
  (is (= 186597 (util/run day06/part1)))
  (is (= 412 (util/run day06/part2))))

(deftest day07-test
  (is (= 21860 (util/run day07/part1)))
  (is (= 2645740 (util/run day07/part2))))

(deftest day08-test
  (is (= 2975 (util/run day08/part1)))
  (is (= (-> "████.█..█.███..█..█.████.
              █....█..█.█..█.█..█.█....
              ███..████.█..█.█..█.███..
              █....█..█.███..█..█.█....
              █....█..█.█.█..█..█.█....
              ████.█..█.█..█..██..████."
          (str/replace #"\s+" "\n"))
         (util/run day08/part2))))

(deftest day09-test
  (is (= 4234906522 (util/run day09/part1)))
  (is (= 60962 (util/run day09/part2))))

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
            [advent-2019.day10 :as day10]
            [advent-2019.day11 :as day11]
            [advent-2019.day12 :as day12]
            [advent-2019.day13 :as day13]
            [advent-2019.day14 :as day14]
            [advent-2019.day15 :as day15]
            [advent-2019.day16 :as day16]
            [advent-2019.day17 :as day17]
            [advent-2019.day18 :as day18]
            [advent-2019.day19 :as day19]
            [advent-2019.day20 :as day20]
            [advent-2019.day21 :as day21]
            [advent-2019.day22 :as day22]
            [advent-2019.day23 :as day23]
            [advent-2019.day24 :as day24]
            [advent-2019.day25 :as day25]
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

(deftest day10-test
  (is (= 263 (util/run day10/part1)))
  (is (= 1110 (util/run day10/part2))))

(deftest day11-test
  (is (= 1894 (util/run day11/part1)))
  (is (= (-> "...██.█..█.████.█....████...██.███..█..█...
              ....█.█.█.....█.█.......█....█.█..█.█..█...
              ....█.██.....█..█......█.....█.███..████...
              ....█.█.█...█...█.....█......█.█..█.█..█...
              .█..█.█.█..█....█....█....█..█.█..█.█..█...
              ..██..█..█.████.████.████..██..███..█..█..."
             (str/replace #"\s+" "\n"))
         (util/run day11/part2))))

(deftest day12-test
  (is (= 6678 (util/run day12/part1)))
  (is (= 496734501382552 (util/run day12/part2))))

(deftest day13-test
  (is (= 270 (util/run day13/part1)))
  (is (= 12535 (util/run day13/part2))))

(deftest day14-test
  (is (= 273638 (util/run day14/part1)))
  (is (= 4200533 (util/run day14/part2))))

(deftest day15-test
  (is (= 262 (util/run day15/part1)))
  (is (= 314 (util/run day15/part2))))

(deftest day16-test
  (is (= "73127523" (util/run day16/part1)))
  (is (= "80284420" (util/run day16/part2))))

(deftest day17-test
  (is (= 7280 (util/run day17/part1)))
  (is (= 1045393 (util/run day17/part2))))

(deftest day18-test
  (is (= 3546 (util/run day18/part1)))
  (is (= 1988 (util/run day18/part2))))

(deftest day19-test
  (is (= 215 (util/run day19/part1)))
  (is (= 7720975 (util/run day19/part2))))

(deftest day20-test
  (is (= 510 (util/run day20/part1)))
  (is (= 5652 (util/run day20/part2))))

(deftest day21-test
  (is (= 19351230 (util/run day21/part1)))
  (is (= 1141262756 (util/run day21/part2))))

(deftest day22-test
  (is (= 6526 (util/run day22/part1)))
  (is (= 79855812422607 (util/run day22/part2))))

(deftest day23-test
  (is (= 24954 (util/run day23/part1)))
  (is (= 17091 (util/run day23/part2))))

(deftest day24-test
  (is (= 32511025 (util/run day24/part1)))
  (is (= 1932 (util/run day24/part2))))

(deftest day25-test
  (is (= "285213704" (util/run day25/part1))))

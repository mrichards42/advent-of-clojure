(ns advent.2024-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [advent.util :as util]
            [advent-2024.day01 :as day01]
            [advent-2024.day02 :as day02]
            [advent-2024.day03 :as day03]
            [advent-2024.day04 :as day04]
            [advent-2024.day05 :as day05]
            [advent-2024.day06 :as day06]
            [advent-2024.day07 :as day07]
            [advent-2024.day08 :as day08]
            [advent-2024.day09 :as day09]
            [advent-2024.day10 :as day10]
            [advent-2024.day11 :as day11]
            [advent-2024.day12 :as day12]
            [advent-2024.day13 :as day13]
            #_[advent-2024.day14 :as day14]
            #_[advent-2024.day15 :as day15]
            #_[advent-2024.day16 :as day16]
            [advent-2024.day17 :as day17]
            #_[advent-2024.day18 :as day18]
            #_[advent-2024.day19 :as day19]
            #_[advent-2024.day20 :as day20]
            #_[advent-2024.day21 :as day21]
            #_[advent-2024.day22 :as day22]
            #_[advent-2024.day23 :as day23]
            #_[advent-2024.day24 :as day24]
            #_[advent-2024.day25 :as day25]
            ))

(deftest day01-test
  (is (= 1603498 (util/run day01/part1)))
  (is (= 25574739 (util/run day01/part2))))

(deftest day02-test
  (is (= 202 (util/run day02/part1)))
  (is (= 271 (util/run day02/part2))))

(deftest day03-test
  (is (= 178886550 (util/run day03/part1)))
  (is (= 87163705 (util/run day03/part2))))

(deftest day04-test
  (is (= 2554 (util/run day04/part1)))
  (is (= 1916 (util/run day04/part2))))

(deftest day05-test
  (is (= 4569 (util/run day05/part1)))
  (is (= 6456 (util/run day05/part2))))

(deftest day06-test
  (is (= 5162 (util/run day06/part1)))
  (is (= 1909 (util/run day06/part2))))

(deftest day07-test
  (is (= 28730327770375 (util/run day07/part1)))
  (is (= 424977609625985 (util/run day07/part2))))

(deftest day08-test
  (is (= 247 (util/run day08/part1)))
  (is (= 861 (util/run day08/part2))))

(deftest day09-test
  (is (= 6331212425418 (util/run day09/part1)))
  (is (= 6363268339304 (util/run day09/part2))))

(deftest day10-test
  (is (= 744 (util/run day10/part1)))
  (is (= 1651 (util/run day10/part2))))

(deftest day11-test
  (is (= 175006 (util/run day11/part1)))
  (is (= 207961583799296 (util/run day11/part2))))

(deftest day12-test
  (is (= 1363682 (util/run day12/part1)))
  (is (= 787680 (util/run day12/part2))))

(deftest day13-test
  (is (= 40369 (util/run day13/part1)))
  (is (= 72587986598368 (util/run day13/part2))))

(deftest day17-test
  (is (= "4,0,4,7,1,2,7,1,6" (util/run day17/part1)))
  (is (= 202322348616234 (util/run day17/part2))))

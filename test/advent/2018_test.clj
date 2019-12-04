(ns advent.2018-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [advent.util :as util]
            [advent-2018.day01 :as day01]
            [advent-2018.day02 :as day02]
            [advent-2018.day03 :as day03]
            [advent-2018.day04 :as day04]
            [advent-2018.day05 :as day05]
            [advent-2018.day06 :as day06]
            [advent-2018.day07 :as day07]
            [advent-2018.day08 :as day08]
            [advent-2018.day09 :as day09]
            [advent-2018.day10 :as day10]
            [advent-2018.day11 :as day11]
            [advent-2018.day12 :as day12]
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

(deftest day07-test
  (is (= "CQSWKZFJONPBEUMXADLYIGVRHT" (util/run day07/part1)))
  (is (= 914 (util/run day07/part2))))

(deftest day08-test
  (is (= 42768 (util/run day08/part1)))
  (is (= 34348 (util/run day08/part2))))

(deftest day09-test
  (is (= 398371 (util/run day09/part1)))
  (is (= 3212830280 (util/run day09/part2))))

(deftest day10-test
  (is (= (-> "#####...#....#..#####......###...####...#.......#####...######
              #....#..#....#..#....#......#...#....#..#.......#....#..#.....
              #....#..#....#..#....#......#...#.......#.......#....#..#.....
              #....#..#....#..#....#......#...#.......#.......#....#..#.....
              #####...######..#####.......#...#.......#.......#####...#####.
              #....#..#....#..#...........#...#..###..#.......#.......#.....
              #....#..#....#..#...........#...#....#..#.......#.......#.....
              #....#..#....#..#.......#...#...#....#..#.......#.......#.....
              #....#..#....#..#.......#...#...#...##..#.......#.......#.....
              #####...#....#..#........###.....###.#..######..#.......######"
             (str/replace #"\s+" "\n"))
         (util/run day10/part1)))
  (is (= 10831 (util/run day10/part2))))

(deftest day11-test
  (is (= [33 54] (util/run day11/part1)))
  (is (= [232 289 8] (util/run day11/part2))))

(deftest day12-test
  (is (= 1787 (util/run day12/part1)))
  (is (= 1100000000475 (util/run day12/part2))))

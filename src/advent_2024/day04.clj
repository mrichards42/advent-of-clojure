(ns advent-2024.day04
  "Ceres Search"
  (:require [advent.util :as util]
            [clojure.string :as str]))

(defn str->grid [s]
  (vec (str/split-lines s)))

(defn grid-at [grid x y]
  (nth (nth grid y nil) x nil))

(defn grid-pattern-compile [pattern]
  (for [[y line] (map-indexed vector pattern)
        [x ch] (map-indexed vector line)
        :when (not= \. ch)]
    [x y ch]))

(defn grid-match-seq
  "Like re-seq, but matches a sting `grid` against any number of `patterns`,
  where a pattern is a 2d collection (e.g. a vector of strings).

  '.' can be used to match any character"
  [grid & patterns]
  (let [patterns (map grid-pattern-compile patterns)]
    (for [dx (range (count grid))
          dy (range (count (first grid)))
          pat patterns
          :when (every? (fn [[x y ch]]
                          (= ch (grid-at grid (+ x dx) (+ y dy))))
                        pat)]
      [dx dy])))

;;; Part 1
;; XMAS word search

(def xmas-patterns
  [["XMAS"]
   ["SAMX"]
   ["X"
    "M"
    "A"
    "S"]
   ["S"
    "A"
    "M"
    "X"]
   ["X..."
    ".M.."
    "..A."
    "...S"]
   ["S..."
    ".A.."
    "..M."
    "...X"]
   ["...X"
    "..M."
    ".A.."
    "S..."]
   ["...S"
    "..A."
    ".M.."
    "X..."]])

(defn part1 [input]
  (let [grid (str->grid input)]
    (count (apply grid-match-seq grid xmas-patterns))))

(comment
  (->> "MMMSXXMASM
       MSAMXMSMSA
       AMXSXMAAMM
       MSAMASMSMX
       XMASAMXAMM
       XXAMMXXAMA
       SMSMSASXSS
       SAXAMASAAA
       MAMMMXMMMM
       MXMXAXMASX"
       (util/example-input)
       (part1))
  ;; => 18

  (time (util/run part1)))

;;; Part 2
;; X-MAS (2 MAS forming a cross) word search

(def x-mas-patterns
  [["M.M"
    ".A."
    "S.S"]
   ["S.M"
    ".A."
    "S.M"]
   ["S.S"
    ".A."
    "M.M"]
   ["M.S"
    ".A."
    "M.S"]])

(defn part2 [input]
  (let [grid (str->grid input)]
    (count (apply grid-match-seq grid x-mas-patterns))))

(comment
  (->> "MMMSXXMASM
       MSAMXMSMSA
       AMXSXMAAMM
       MSAMASMSMX
       XMASAMXAMM
       XXAMMXXAMA
       SMSMSASXSS
       SAXAMASAAA
       MAMMMXMMMM
       MXMXAXMASX"
       (util/example-input)
       (part2))
  ;; => 9

  (time (util/run part2)))

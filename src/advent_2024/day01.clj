(ns advent-2024.day01
  "Historian Hysteria"
  (:require [advent.util :as util]
            [clojure.string :as str]))

;;; Part 1
;; Pairwise difference between two lists

(defn parse-input [input]
  (let [numbers (for [line (util/lines input)]
                  (map parse-long (str/split line #"\s+")))]
    [(map first numbers) (map second numbers)]))

(defn part1 [input]
  (let [[left right] (parse-input input)]
    (reduce + (map (comp abs -) (sort left) (sort right)))))

(comment
  (->> "3   4
       4   3
       2   5
       1   3
       3   9
       3   3"
       (util/example-input)
       (part1))

  (time (util/run part1)))

;;; Part 2
;; Determine how often each number from the left list appears in the right list

(defn part2 [input]
  (let [[left right] (parse-input input)
        counts (frequencies right)]
    (reduce + (map #(* (counts % 0) %) left))))

(comment
  (time (util/run part2)))

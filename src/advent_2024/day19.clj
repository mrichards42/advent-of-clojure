(ns advent-2024.day19
  "Puzzle Title"
  (:require [advent.util :as util]
            [clojure.string :as str]))

;;; Part 1
;; Find strip patterns that are possible to make with the given towels

(defn parse-input [input]
  (let [[towels _ & patterns] (str/split-lines input)
        towels (str/split towels #", ")]
    {:towels towels
     :patterns patterns}))

(defn part1 [input]
  (let [{:keys [towels patterns]} (parse-input input)
        re-pat (re-pattern (str "^(?:" (str/join "|" towels) ")*$"))]
    (count (filter #(re-matches re-pat %) patterns))))

(comment
  (time (util/run part1)))

;;; Part 2
;; Count all possible ways to arrange towels to make the stripe patterns

(def arrangements
  (memoize
   (fn [towels pattern]
     (apply
      +
      (for [t towels
            :when (str/starts-with? pattern t)]
        (if (= t pattern)
          1
          (arrangements towels (subs pattern (count t)))))))))

(defn part2 [input]
  (let [{:keys [towels patterns]} (parse-input input)]
    (apply + (map #(arrangements towels %) patterns))))

(comment
  (time (util/run part2)))

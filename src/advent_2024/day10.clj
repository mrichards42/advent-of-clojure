(ns advent-2024.day10
  "Hoof It"
  (:require [advent.grid :as grid]
            [advent.util :as util]
            [clojure.set :as set]))

;;; Part 1
;; For each trailhead (0) count how many peaks (9) it can reach

(defn parse-input [input]
  (->> input
       (grid/parse (fn [pos ch] [pos (parse-long (str ch))]))
       (reduce (fn [m [pos digit]]
                 (update m digit (fnil conj #{}) pos))
               {})))

(defn walk-map [m height paths]
  (if (= 9 height)
    paths
    (let [possible-neighbors (m (inc height))
          paths' (set (for [path paths
                            [dx dy] [[1 0] [-1 0] [0 1] [0 -1]]
                            :let [[x y] (peek path)
                                  head [(+ x dx) (+ y dy)]]
                            :when (possible-neighbors head)]
                        (conj path head)))]
      (recur m (inc height) paths'))))

(defn part1 [input]
  (let [m (parse-input input)
        starts (set (map vector (m 0)))
        paths (walk-map m 0 starts)]
    (count (set (map (juxt first last) paths)))))

(comment
  (parse-input (util/data))
  (time (util/run part1)))

;;; Part 2
;; Number of distinct 0-9 paths

(defn part2 [input]
  (let [m (parse-input input)
        starts (set (map vector (m 0)))
        paths (walk-map m 0 starts)]
    (count paths)))

(comment
  (time (util/run part2)))

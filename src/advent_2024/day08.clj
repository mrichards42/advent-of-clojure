(ns advent-2024.day08
  "Resonant Collinearity"
  (:require [advent.grid :as grid]
            [advent.util :as util]))

(def example
  (util/example-input
   "............
   ........0...
   .....0......
   .......0....
   ....0.......
   ......A.....
   ............
   ............
   ........A...
   .........A..
   ............
   ............
   "))

;;; Part 1
;; Antinodes are exactly 1 step away from each pair

(defn parse-input [input]
  (let [cells (grid/parse input)]
    {:xmax (apply max (map (comp first first) cells))
     :ymax (apply max (map (comp second first) cells))
     :antenna-groups (->> (remove (comp #{\.} second) cells)
                          (reduce
                           (fn [m [pos ch]]
                             (update m ch (fnil conj []) pos))
                           {}))}))

(defn calc-antinode [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    [(+ x1 (* 2 dx)) (+ y1 (* 2 dy))]))

(defn part1 [input]
  (let [{:keys [antenna-groups xmax ymax]} (parse-input input)
        antinodes (for [antennas (vals antenna-groups)
                        subj antennas
                        other antennas
                        :when (not= subj other)
                        :let [[x y] (calc-antinode subj other)]
                        :when (and (<= 0 x xmax) (<= 0 y ymax))]
                    [x y])]
    (count (set antinodes))))

(comment
  (part1 example)
  ;; => 14
  (time (util/run part1)))

;;; Part 2
;; Antinodes are anywhere a line between two antennas hits a grid space exactly
;; (i.e. any number of steps away from a pair, including the antenna itself)

(defn antinode-seq [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (for [dist (drop 1 (range))]
      [(+ x1 (* dist dx)) (+ y1 (* dist dy))])))

(defn part2 [input]
  (let [{:keys [antenna-groups xmax ymax]} (parse-input input)
        antinodes (for [antennas (vals antenna-groups)
                        subj antennas
                        other antennas
                        :when (not= subj other)
                        [x y] (antinode-seq subj other)
                        :while (and (<= 0 x xmax) (<= 0 y ymax))]
                    [x y])]
    (count (set antinodes))))

(comment
  (part2 example)
  ;; => 34
  (time (util/run part2)))

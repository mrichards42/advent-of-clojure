;; Template for day namespaces

(ns advent-2024.day12
  "Garden Groups"
  (:require [advent.grid :as grid]
            [advent.util :as util]
            [clojure.set :as set]))

(def example
  (util/example-input
   "RRRRIICCFF
   RRRRIICCCF
   VVRRRCCFFF
   VVRCCCJFFF
   VVVVCJJCFE
   VVIVCCJJEE
   VVIIICJJEE
   MIIIIIJJEE
   MIIISIJEEE
   MMMISSJEEE"))

;;; Part 1
;; Score contiguous regions

(defn parse-input [input]
  (->> (grid/parse (fn [pos ch] [ch pos]) input)
       (reduce (fn [m [ch pos]] (update m ch (fnil conj #{}) pos)) {})))

(def moves
  [[1 0]
   [-1 0]
   [0 1]
   [0 -1]])

(defn neighbors [pos]
  (for [move moves] (mapv + pos move)))

(defn contiguous-regions [positions]
  (loop [current-region #{(first positions)}
         frontier #{(first positions)}
         unseen (disj positions (first positions))
         regions []]
    (if (empty? unseen)
      (conj regions current-region)
      (let [frontier' (set/intersection (set (mapcat neighbors frontier))
                                        unseen)]
        (if (seq frontier')
          (recur (into current-region frontier')
                 frontier'
                 (set/difference unseen frontier')
                 regions)
          (recur #{(first unseen)}
                 #{(first unseen)}
                 (disj unseen (first unseen))
                 (conj regions current-region)))))))

(defn perimeter [region]
  (let [positions (set region)]
    (apply +
           (for [pos positions]
             (let [neighbor-count (count (filter positions (neighbors pos)))]
               (- 4 neighbor-count))))))

(defn part1 [input]
  (let [grid (parse-input input)]
    (->> (mapcat contiguous-regions (vals grid))
         (map (fn [region] (* (count region) (perimeter region))))
         (apply +))))

(comment
  (part1 example)
  ;; => 1930
  (time (util/run part1)))

;;; Part 2
;; Score regions, but count only contiguous lines of fence instead of the whole
;; perimeter

(defn side-count-horizontal [region {:keys [x-min x-max]} y look-dir]
  (let [edge-points (set
                     (for [x (range x-min (inc x-max))
                           :when (and (region [x y])
                                      (not (region [x (+ look-dir y)])))]
                       x))]
    ;; within these points that are edges, count the ones that *start* a line
    ;; (i.e. there isn't a point before it)
    (count (remove #(edge-points (dec %)) edge-points))))

(defn side-count-vertical [region {:keys [y-min y-max]} x look-dir]
  (let [edge-points (set
                     (for [y (range y-min (inc y-max))
                           :when (and (region [x y])
                                      (not (region [(+ x look-dir) y])))]
                       y))]
    ;; within these points that are edges, count the ones that *start* a line
    ;; (i.e. there isn't a point before it)
    (count (remove #(edge-points (dec %)) edge-points))))

(defn side-count [region]
  (let [{:keys [x-min x-max y-min y-max] :as bounds} (grid/bounds region)]
    ;; Sweep lines in all directions and count the number of contiguous edges.
    ;; It feels like there should be a cleaner way to do this (especially
    ;; without both -horizontal and -vertical functions), but I can't come up
    ;; with anything simple right now.
    (apply + (concat (for [y (range y-min (inc y-max))
                           look-dir [-1 1]]
                       (side-count-horizontal region bounds y look-dir))
                     (for [x (range x-min (inc x-max))
                           look-dir [-1 1]]
                       (side-count-vertical region bounds x look-dir))))))

(defn part2 [input]
  (let [grid (parse-input input)]
    (->> (mapcat contiguous-regions (vals grid))
         (map (fn [region] (* (count region) (side-count region))))
         (apply +))))

(comment
  (part2 example)
  ;; => 1206
  (time (util/run part2)))

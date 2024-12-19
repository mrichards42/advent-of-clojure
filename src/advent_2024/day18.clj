(ns advent-2024.day18
  "RAM Run"
  (:require [advent.algo :as algo]
            [advent.grid :as grid]
            [advent.util :as util]
            [clojure.string :as str]))

;;; Part 1
;; Find a path after 1024 bytes have fallen

(defn parse-input [input w h]
  {:grid
   (into {}
         (for [x (range w)
               y (range h)]
           [[x y] \.]))
   :corruption (vec
                (for [line (str/split-lines input)]
                  (map parse-long (re-seq #"\d+" line))))})

(defn find-path [grid corruption start end]
  (let [grid (into grid (map (fn [pos] [pos \#])) corruption)
        neighbors (fn [pos]
                    (for [n (grid/neighbors-4 pos)
                          :when (= \. (grid n))]
                      [1 n]))]
    (algo/dijkstra-path neighbors start end)))

(defn part1
  ([input] (part1 input 71 71))
  ([input w h]
   (let [{:keys [grid corruption]} (parse-input input w h)
         start [0 0]
         end [(dec w) (dec h)]]
     (count (find-path grid (take 1024 corruption) start end)))))

(comment
  (time (util/run part1)))

;;; Part 2
;; Find the first byte that prevents any path

(defn part2
  ([input] (part2 input 71 71))
  ([input w h]
   (let [{:keys [grid corruption]} (parse-input input w h)
         start [0 0]
         end [(dec w) (dec h)]
         path-exists? (fn [n]
                        (boolean
                         (find-path grid (subvec corruption 0 n) start end)))]
     (->> (algo/binary-search
           #(case [(path-exists? %) (path-exists? (inc %))]
              [true true] -1  ; path exists here and at next step = too low
              [true false] 0  ; path exists here but not at next step = goal
              [false false] 1 ; no path exists = too high
              )
           0 1024 (count corruption))
          (nth corruption)
          (str/join ",")))))

(comment
  (time (util/run part2)))

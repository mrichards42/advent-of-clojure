(ns advent-2024.day06
  "Guard Gallivant"
  (:require [advent.util :as util]
            [clojure.string :as str]))

(def example
  (util/example-input
   "....#.....
   .........#
   ..........
   ..#.......
   .......#..
   ..........
   .#..^.....
   ........#.
   #.........
   ......#..."))

;;; Part 1
;; Run the guard and count how many cells they visit

(def guard? #{\> \< \^ \v})
(def turn {\> \v
           \< \^
           \^ \>
           \v \<})

(defn parse-grid [input]
  (let [grid (into {}
                   (for [[y line] (map-indexed vector (str/split-lines input))
                         [x cell] (map-indexed vector line)]
                     {[x y] cell}))
        guard (first (filter (comp guard? val) grid))]
    {:grid (assoc grid (key guard) \.)
     :guard guard}))

(defn move [grid guard]
  (let [[[x y] dir] guard
        ahead (case dir
                \> [(inc x) y]
                \< [(dec x) y]
                \^ [x (dec y)]
                \v [x (inc y)])]
    (if (= \# (grid ahead))
      [[x y] (turn dir)]
      [ahead dir])))

(defn part1 [input]
  (let [{:keys [grid guard]} (parse-grid input)]
    (->> guard
         (iterate (partial move grid))
         (take-while (fn [[pos]] (grid pos)))
         (into #{} (map first))
         (count))))

(comment
  (time (part1 example))
  (take 10 (range))
  (analyze-grid (:grid (parse-grid example)))
  ;; => 41
  (time (util/run part1)))

;;; Part 2
;; Try every possible location to add a new block and count how many locations
;; result in the guard entering a loop.

;; Might be able to optimize further by making it a graph (then adding a block
;; is just splitting two paths in half)

(defn jump [grid guard]
  (let [[[x y] dir] guard
        ahead (case dir
                \> [(inc x) y]
                \< [(dec x) y]
                \^ [x (dec y)]
                \v [x (inc y)])]
    (case (grid ahead)
      \# [[x y] (turn dir)]
      nil [ahead dir]
      ;; for part 2 we don't care about visiting every square, we just need to
      ;; know that we can walk between blocks
      \. (recur grid [ahead dir]))))

(defn run-guard [grid guard move-fn]
  (loop [seen #{}
         guard guard]
    (cond
      (seen guard) {:result :loop :seen seen}
      (nil? (grid (first guard))) {:result :exited :seen seen}
      :else (recur (conj seen guard) (move-fn grid guard)))))

(defn part2 [input]
  (let [{:keys [grid guard]} (parse-grid input)
        {:keys [seen]} (run-guard grid guard move)
        seen-positions (set (map first seen))
        init-pos (first guard)
        locations-to-put-blocks (disj seen-positions init-pos)]
    (->> locations-to-put-blocks
         (pmap (fn [pos] (run-guard (assoc grid pos \#) guard jump)))
         (filter (comp #{:loop} :result))
         (count))))

(comment
  (part2 example)
  ;; => 6
  (time (util/run part2)))

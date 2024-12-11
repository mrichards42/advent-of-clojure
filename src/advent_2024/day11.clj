(ns advent-2024.day11
  "Plutonian Pebbles"
  (:require [advent.util :as util]))

;;; Part 1
;; Number of stones after 25 iterations

(defn parse-input [line]
  (->> line
       (re-seq #"\d+")
       (map parse-long)
       (frequencies)))

(defn split-number [n]
  (let [s (str n)
        size (count s)]
    [(parse-long (subs s 0 (/ size 2)))
     (parse-long (subs s (/ size 2)))]))

(defn step [m]
  (->> (for [[k n] m
             :let [ks (cond
                        (zero? k) [1]
                        (even? (count (str k))) (split-number k)
                        :else [(* k 2024)])]
             k' ks]
         [k' n])
       (reduce (fn [m [k v]]
                 (update m k (fnil + 0) v))
               {})))

(defn part1 [input]
  (->> (parse-input input)
       (iterate step)
       (drop 25)
       (first)
       (vals)
       (apply +)))

(comment
  (part1 "125 17")
  ;; => 55312
  (time (util/run part1)))

;;; Part 2
;; Number of stones after 75 iterations

(defn part2 [input]
  (->> (parse-input input)
       (iterate step)
       (drop 75)
       (first)
       (vals)
       (apply +)))

(comment
  (part2 "125 17")
  ;; => 65601038650482
  (time (util/run part2)))

(ns advent-2024.day02
  "Red-Nosed Reports"
  (:require [advent.util :as util]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def example
  (util/example-input
   "7 6 4 2 1
   1 2 7 8 9
   9 7 6 2 1
   1 3 2 4 5
   8 6 4 4 1
   1 3 6 7 9"))

;;; Part 1
;; Count the number of "safe" reports

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map #(edn/read-string (str "[" % "]")))))

(defn safe? [report]
  (let [sorted (sort report)]
    (and (or (= sorted report)
             (= sorted (reverse report)))
         (every? #(<= 1 % 3) (map (comp abs -) report (rest report))))))

(defn part1 [input]
  (->> (parse-input input)
       (filter safe?)
       (count)))

(comment
  (part1 example)
  ;; => 2
  (time (util/run part1)))

;;; Part 2
;; Reports are allowed to have 1 "unsafe" value

(defn vector-dissoc
  [v idx]
  (into (subvec v 0 idx) (subvec v (inc idx))))

(defn safe-with-replacement? [report]
  (or (safe? report)
      (some safe? (map-indexed (fn [idx _] (vector-dissoc report idx)) report))))

(defn part2 [input]
  (->> (parse-input input)
       (filter safe-with-replacement?)
       (count)))

(comment
  (part2 example)
  ;; => 4
  (time (util/run part2)))

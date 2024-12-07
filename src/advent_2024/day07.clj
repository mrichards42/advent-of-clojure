(ns advent-2024.day07
  "Bridge Repair"
  (:require [advent.util :as util]
            [clojure.math :as math]
            [clojure.string :as str]))

(def example
  (util/example-input
   "190: 10 19
   3267: 81 40 27
   83: 17 5
   156: 15 6
   7290: 6 8 6 15
   161011: 16 10 13
   192: 17 8 14
   21037: 9 7 18 13
   292: 11 6 16 20
   "))

;;; Part 1
;; Try to make an equation using only + and *

(defn parse-input [input]
  (for [line (str/split-lines input)]
    (let [[target & values] (map parse-long (re-seq #"\d+" line))]
      {:target target
       :values (vec values)})))

(defn eval-equation [ops target values [next-num & more-nums]]
  (if next-num
    (let [values' (for [value values
                        op ops
                        :let [new-value (op value next-num)]
                        :when (<= new-value target)]
                    new-value)]
      (recur ops target values' more-nums))
    values))

(defn can-make-equation? [ops {:keys [target values]}]
  (->> (eval-equation ops target (take 1 values) (rest values))
       (some #(= target %))))

(defn part1 [input]
  (->> (parse-input input)
       (filter (partial can-make-equation? (seq [+ *])))
       (map :target)
       (apply +)))

(comment
  (part1 example)
  ;; => 3749
  (time (util/run part1)))

;;; Part 2
;; Try to make an equation also using || which concats numbers

(defn int-concat [a b]
  (parse-long (str a b)))

(defn part2 [input]
  (->> (parse-input input)
       (util/pfilter (partial can-make-equation? (seq [+ * int-concat])))
       (map :target)
       (apply +)))

(comment
  (part2 example)
  ;; => 11387
  (time (util/run part2)))

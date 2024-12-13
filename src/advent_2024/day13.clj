(ns advent-2024.day13
  "Claw Contraption"
  (:require [advent.algo :as algo]
            [advent.util :as util]
            [clojure.string :as str]))

(def example
  (util/example-input
   "Button A: X+94, Y+34
   Button B: X+22, Y+67
   Prize: X=8400, Y=5400

   Button A: X+26, Y+66
   Button B: X+67, Y+21
   Prize: X=12748, Y=12176

   Button A: X+17, Y+86
   Button B: X+84, Y+37
   Prize: X=7870, Y=6450

   Button A: X+69, Y+23
   Button B: X+27, Y+71
   Prize: X=18641, Y=10279
   "))

;;; Part 1
;; Press buttons until you hit the prize

(defn parse-input [input]
  (for [machine (str/split input #"\n\n")]
    (let [[a b prize] (str/split-lines machine)]
      {:a (mapv (comp parse-long second) (re-seq #"[XY]\+(\d+)" a))
       :b (mapv (comp parse-long second) (re-seq #"[XY]\+(\d+)" b))
       :prize (mapv (comp parse-long second) (re-seq #"[XY]=(\d+)" prize))})))


;; Did a graph traversal for part 1 :) Then naturally had to do the math
;; solution for part 2.
(defn solve-equation [{[ax ay] :a
                       [bx by] :b
                       [px py] :prize}]
  ;; equations are
  ;;     ax * A + bx * B = px
  ;;     ay * A + by * B = py
  ;; so to solve for A:
  ;;     B = (px - ax * A) / bx (from the first equation)
  ;;     b = (py - ay * A) / by (from the second equation)
  ;;     (px - ax * A) / bx = (py - ay * A) / by
  ;;     by * (px - ax * A) = bx * (py - ay * A)
  ;;     (by * px) - (by * ax) * A = (bx * py) - (bx * ay) * A
  ;;     ((bx * ay) - (by * ax)) * A = (bx * py) - (by * px)
  ;;     A = ((bx * py) - (by * px)) / ((bx * ay) - (by * ax))
  ;; and then for B:
  ;;     B = (px - (ax * A)) / bx
  ;; ^^ apparently this is equivalent to Cramer's rule
  (let [a (/ (- (* bx py) (* by px))
             (- (* bx ay) (* by ax)))
        b (/ (- px (* ax a))
             bx)]
    (when (and (int? a) (int? b))
      [a b])))

(defn part1 [input]
  (->> (keep solve-equation (parse-input input))
       (map (fn [[a b]] (+ (* 3 a) b)))
       (apply +)))

(comment
  (part1 example)
  ;; => 480
  (time (util/run part1)))

;;; Part 2
;; add 10000000000000 to each of the prize coordinates

(defn parse-input-2 [input]
  (for [{:keys [a b prize]} (parse-input input)]
    {:a a
     :b b
     :prize (mapv #(+ 10000000000000 %) prize)}))

(defn part2 [input]
  (->> (keep solve-equation (parse-input-2 input))
       (map (fn [[a b]] (+ (* 3 a) b)))
       (apply +)))

(comment
  (part2 example)
  ;; => 875318608908
  (time (util/run part2)))

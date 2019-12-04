(ns advent-2019.day04
  "Secure Container"
  (:require [advent.util :as util]))

;;; Part 1

; You arrive at the Venus fuel depot only to discover it's protected by a
; password. The Elves had written the password on a sticky note, but someone
; threw it out.

; However, they do remember a few key facts about the password:

;   - It is a six-digit number.
;   - The value is within the range given in your puzzle input.
;   - Two adjacent digits are the same (like 22 in 122345).
;   - Going from left to right, the digits never decrease; they only ever
;     increase or stay the same (like 111123 or 135679).

; Other than the range rule, the following are true:

;   - 111111 meets these criteria (double 11, never decreases).
;   - 223450 does not meet these criteria (decreasing pair of digits 50).
;   - 123789 does not meet these criteria (no double).

; How many different passwords within the range given in your puzzle input meet
; these criteria?

(def ^:dynamic *lower-bound* 134564)
(def ^:dynamic *upper-bound* 585159)

(defn digits [n]
  (->> (iterate #(int (/ % 10)) n)
       (take-while (complement zero?))
       (reduce #(conj %1 (mod %2 10)) ())))

(defn increasing? [digits]
  (apply <= digits))

(defn has-pair? [digits]
  (some #(apply = %) (partition 2 1 digits)))

(defn part1 []
  (->> (range *lower-bound* (inc *upper-bound*))
       (map digits)
       (filter increasing?)
       (filter has-pair?)
       (count)))

#_ (time (part1))

;;; Part 2

; An Elf just remembered one more important detail: the two adjacent matching
; digits are not part of a larger group of matching digits.

; Given this additional criterion, but still ignoring the range rule, the
; following are now true:

;   - 112233 meets these criteria because the digits never decrease and all
;     repeated digits are exactly two digits long.
;   - 123444 no longer meets the criteria (the repeated 44 is part of a larger
;     group of 444).
;   - 111122 meets the criteria (even though 1 is repeated more than twice, it
;     still contains a double 22).

; How many different passwords within the range given in your puzzle input meet
; all of the criteria?

(defn has-strict-pair? [digits]
  ;; a pair that is not also a triple or more
  (->> (re-seq #"(.)\1+" (apply str digits))
       (map first)
       (some #(= 2 (count %)))))

(defn part2 []
  (->> (range *lower-bound* (inc *upper-bound*))
       (map digits)
       (filter increasing?)
       (filter has-strict-pair?)
       (count)))

#_ (time (part2))

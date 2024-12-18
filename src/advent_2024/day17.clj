(ns advent-2024.day17
  "Chronospatial Computer"
  (:require [advent.util :as util]
            [clojure.math :as math]
            [clojure.string :as str]))

;;; Part 1
;; Run the computer and print output

(defn combo [c ^long operand]
  (case operand
    0 0
    1 1
    2 2
    3 3
    4 (:A c)
    5 (:B c)
    6 (:C c)
    7 (throw (ex-info "Invalid program" {:operand 7}))))

(defn run-instr [c ^long op ^long operand]
  (case op
    ;; adv
    0 (-> c
          (assoc :A (long (/ (:A c) (math/pow 2 (combo c operand)))))
          (update :ip + 2))
    ;; bxl
    1 (-> c
          (assoc :B (bit-xor (:B c) operand))
          (update :ip + 2))
    ;; bst
    2 (-> c
          (assoc :B (mod (combo c operand) 8))
          (update :ip + 2))
    ;; jnz
    3 (if (zero? (:A c))
        (update c :ip + 2)
        (assoc c :ip operand))
    ;; bxc
    4 (-> c
          (assoc :B (bit-xor (:B c) (:C c)))
          (update :ip + 2))
    ;; out
    5 (-> c
          (update :out conj (mod (combo c operand) 8))
          (update :ip + 2))
    ;; bdv
    6 (-> c
          (assoc :B (long (/ (:A c) (math/pow 2 (combo c operand)))))
          (update :ip + 2))
    ;; cdv
    7 (-> c
          (assoc :C (long (/ (:A c) (math/pow 2 (combo c operand)))))
          (update :ip + 2))
    ;; default = halt
    (assoc c :running? false)))

(defn build-computer [input]
  (let [[regs program-str] (str/split input #"\n\n")
        [A B C] (for [line (str/split-lines regs)]
                  (parse-long (re-find #"\d+" line)))
        program (map parse-long (re-seq #"\d+" program-str))]
    {:A A
     :B B
     :C C
     :mem program
     :ip 0
     :out []
     :running? true}))

(defn run-program [c]
  (->> c
       (iterate (fn [{:keys [ip mem] :as c}]
                  (run-instr c (nth mem ip -1) (nth mem (inc ip) -1))))
       (drop-while :running?)
       (first)))

(defn part1 [input]
  (let [c (build-computer input)]
    (str/join "," (:out (run-program c)))))

(comment
  (time (util/run part1)))

;;; Part 2
;; What initial value for A produces a quine?

;; I thought about trying to run the program backwards, but I don't think you
;; can actually do the inverse of most of these. After inspecting my input, I
;; noticed that each loop is only dependent on the value of A to start, and
;; that at the end of each loop A = A // 8.

;; So this works backwards, starting with A = 0 (which is the condition to end
;; the loop), and finds programs that produces successively longer tails of
;; correct output. At each step we then loop for A' = A * 8 + every value 0-7
;; (since any number 0-7 could be the remainder of integer division), and see
;; if it produces one more correct output value, until we've produced the whole
;; output.

(defn part2 [input]
  (let [c (build-computer input)]
    (loop [idx 0
           As [0]]
      (if (= idx (count (:mem c)))
        (apply min As)
        (recur (inc idx)
               (for [n (range 0 8)
                     ;; There can be multiple valid starting values for each
                     ;; step! Just picking the lowest A can run into impossible
                     ;; states as this progresses, so we need to track every
                     ;; possible A
                     A As
                     :let [A' (+ n (* 8 A))
                           {:keys [mem out]} (run-program (assoc c :A A'))]
                     :when (= out (take-last (inc idx) mem))]
                 A'))))))

(comment
  (time (util/run part2)))

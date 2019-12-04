(ns advent-2018.day12
  "Subterranean Sustainability"
  (:require [advent.util :as util]
            [clojure.string :as str]))

;;; Part 1

; The year 518 is significantly more underground than your history books
; implied. Either that, or you've arrived in a vast cavern network under the
; North Pole.

; After exploring a little, you discover a long tunnel that contains a row of
; small pots as far as you can see to your left and right. A few of them
; contain plants - someone is trying to grow things in these
; geothermally-heated caves.

; The pots are numbered, with 0 in front of you. To the left, the pots are
; numbered -1, -2, -3, and so on; to the right, 1, 2, 3.... Your puzzle input
; contains a list of pots from 0 to the right and whether they do (#) or do not
; (.) currently contain a plant, the initial state. (No other pots currently
; contain plants.) For example, an initial state of #..##.... indicates that
; pots 0, 3, and 4 currently contain plants.

; Your puzzle input also contains some notes you find on a nearby table:
; someone has been trying to figure out how these plants spread to nearby pots.
; Based on the notes, for each generation of plants, a given pot has or does
; not have a plant based on whether that pot (and the two pots on either side
; of it) had a plant in the last generation. These are written as LLCRR => N,
; where L are pots to the left, C is the current pot being considered, R are
; the pots to the right, and N is whether the current pot will have a plant in
; the next generation. For example:

;   - A note like ..#.. => . means that a pot that contains a plant but with no
;     plants within two pots of it will not have a plant in it during the next
;     generation.
;   - A note like ##.## => . means that an empty pot with two plants on each
;     side of it will remain empty in the next generation.
;   - A note like .##.# => # means that a pot has a plant in a given generation
;     if, in the previous generation, there were plants in that pot, the one
;     immediately to the left, and the one two pots to the right, but not in
;     the ones immediately to the right and two to the left.

; It's not clear what these plants are for, but you're sure it's important, so
; you'd like to make sure the current configuration of plants is sustainable by
; determining what will happen after 20 generations.

; For example, given the following input:

; initial state: #..#.#..##......###...###

; ...## => #
; ..#.. => #
; .#... => #
; .#.#. => #
; .#.## => #
; .##.. => #
; .#### => #
; #.#.# => #
; #.### => #
; ##.#. => #
; ##.## => #
; ###.. => #
; ###.# => #
; ####. => #

; For brevity, in this example, only the combinations which do produce a plant
; are listed. (Your input includes all possible combinations.) Then, the next
; 20 generations will look like this:

;                  1         2         3     
;        0         0         0         0     
;  0: ...#..#.#..##......###...###...........
;  1: ...#...#....#.....#..#..#..#...........
;  2: ...##..##...##....#..#..#..##..........
;  3: ..#.#...#..#.#....#..#..#...#..........
;  4: ...#.#..#...#.#...#..#..##..##.........
;  5: ....#...##...#.#..#..#...#...#.........
;  6: ....##.#.#....#...#..##..##..##........
;  7: ...#..###.#...##..#...#...#...#........
;  8: ...#....##.#.#.#..##..##..##..##.......
;  9: ...##..#..#####....#...#...#...#.......
; 10: ..#.#..#...#.##....##..##..##..##......
; 11: ...#...##...#.#...#.#...#...#...#......
; 12: ...##.#.#....#.#...#.#..##..##..##.....
; 13: ..#..###.#....#.#...#....#...#...#.....
; 14: ..#....##.#....#.#..##...##..##..##....
; 15: ..##..#..#.#....#....#..#.#...#...#....
; 16: .#.#..#...#.#...##...#...#.#..##..##...
; 17: ..#...##...#.#.#.#...##...#....#...#...
; 18: ..##.#.#....#####.#.#.#...##...##..##..
; 19: .#..###.#..#.#.#######.#.#.#..#.#...#..
; 20: .#....##....#####...#######....#.#..##.

; The generation is shown along the left, where 0 is the initial state. The pot
; numbers are shown along the top, where 0 labels the center pot,
; negative-numbered pots extend to the left, and positive pots extend toward
; the right. Remember, the initial state begins at pot 0, which is not the
; leftmost pot used in this example.

; After one generation, only seven plants remain. The one in pot 0 matched the
; rule looking for ..#.., the one in pot 4 matched the rule looking for .#.#.,
; pot 9 matched .##.., and so on.

; In this example, after 20 generations, the pots shown as # contain plants,
; the furthest left of which is pot -2, and the furthest right of which is pot
; 34. Adding up all the numbers of plant-containing pots after the 20th
; generation produces 325.

; After 20 generations, what is the sum of the numbers of all pots which
; contain a plant?

(defn parse-input [f]
  (let [lines (util/lines f)
        plants (second (re-find #"initial state: (.*)" (first lines)))
        rules (->> lines
                   (drop 2)
                   (map #(mapv str/trim (str/split % #"=>")))
                   (into {}))]
    {:plants plants
     :first-idx 0
     :rules rules}))

(defn plant-step [{:keys [plants rules]} idx]
  (let [surroundings (subs (str ".." plants "..")
                           idx
                           (+ 5 idx))]
    (get rules surroundings ".")))

(defn ensure-space [{:keys [plants] :as state}]
  (let [buffer "....."
        needs-first? (not (str/starts-with? plants buffer))
        needs-last? (not (str/ends-with? plants buffer))]
    (cond-> state
      needs-first? (update :plants #(str buffer %))
      needs-last? (update :plants #(str % buffer))
      needs-first? (update :first-idx - (count buffer)))))

(defn step [state]
  (let [state' (ensure-space state)]
    (-> state'
        (update :plants #(->> (range (count %))
                              (map (partial plant-step state'))
                              (apply str)))
        (update :generation (fnil inc 0)))))

(defn sum-of-plants [{:keys [plants first-idx]}]
  (->> (map (fn [plant idx] (when (= \# plant) idx))
            plants
            (iterate inc first-idx))
       (filter identity)
       (reduce +)))

(defn part1 [f]
  (let [final-state (->> (parse-input f)
                         (iterate step)
                         (take (inc 20))
                         (last))]
    (sum-of-plants final-state)))

#_ (time (util/run part1))

;; Test data

(defn print-simulation [state n]
  (let [states (take (inc n) (iterate step state))
        first-idx (:first-idx (last states))]
    (doseq [state states]
      (printf "%10s %3s" (sum-of-plants state) (:generation state 0))
      (print (apply str (repeat (- (:first-idx state) first-idx) " ")))
      (println (:plants state)))))

#_ (-> "initial state: #..#.#..##......###...###

       ...## => #
       ..#.. => #
       .#... => #
       .#.#. => #
       .#.## => #
       .##.. => #
       .#### => #
       #.#.# => #
       #.### => #
       ##.#. => #
       ##.## => #
       ###.. => #
       ###.# => #
       ####. => #"
       (str/replace #"^\s+" "")
       (.getBytes "utf-8")
       (parse-input)
       (print-simulation 100))

;;; Part 2

; You realize that 20 generations aren't enough. After all, these plants will
; need to last another 1500 years to even reach your timeline, not to mention
; your future.

; After fifty billion (50000000000) generations, what is the sum of the numbers
; of all pots which contain a plant?


;; need a good way to calculate this, but eventually we reach a steady-state
;; with 11 pairs of plants marching 1 space to the right each generation
;; The formula ends up being
; (generation - 1000) * 22 + 22475
; 22 * generation - 22000 + 22475
; 22 * generation + 475

(defn plant-area
  "Returns just the substr that contains plants."
  [state]
  (second (re-find #"^\.*(#.*#).*$" (:plants state))))

(defn part2 [f]
  (let [;; find the first state that repeats the plant layout multiple times
        window 5
        [state state'] (->> (parse-input f)
                            (iterate step)
                            (partition window 1)
                            (drop-while #(apply not= (map plant-area %)))
                            (first))
        ;; figure out the formula
        plant-score (sum-of-plants state)
        plant-score' (sum-of-plants state')
        score-diff (- plant-score' plant-score)]
    ;; assuming this pattern repeats, the score should increase by `score-diff`
    ;; every generation
    ;; NB: using math fns with ' since they auto-promote large numbers
    (+' plant-score (*' score-diff (-' 50000000000 (:generation state))))))

#_ (time (util/run part2))

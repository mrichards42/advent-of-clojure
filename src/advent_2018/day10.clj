(ns advent-2018.day10
  "The Stars Align"
  (:require [advent.util :as util]
            [advent.grid :as grid]
            [clojure.string :as str]))

;;; Part 1

; It's no use; your navigation system simply isn't capable of providing walking
; directions in the arctic circle, and certainly not in 1018.

; The Elves suggest an alternative. In times like these, North Pole rescue
; operations will arrange points of light in the sky to guide missing Elves
; back to base. Unfortunately, the message is easy to miss: the points move
; slowly enough that it takes hours to align them, but have so much momentum
; that they only stay aligned for a second. If you blink at the wrong time, it
; might be hours before another message appears.

; You can see these points of light floating in the distance, and record their
; position in the sky and their velocity, the relative change in position per
; second (your puzzle input). The coordinates are all given from your
; perspective; given enough time, those positions and velocities will move the
; points into a cohesive message!

; Rather than wait, you decide to fast-forward the process and calculate what
; the points will eventually spell.

; For example, suppose you note the following points:

; position=< 9,  1> velocity=< 0,  2>
; position=< 7,  0> velocity=<-1,  0>
; position=< 3, -2> velocity=<-1,  1>
; position=< 6, 10> velocity=<-2, -1>
; position=< 2, -4> velocity=< 2,  2>
; position=<-6, 10> velocity=< 2, -2>
; position=< 1,  8> velocity=< 1, -1>
; position=< 1,  7> velocity=< 1,  0>
; position=<-3, 11> velocity=< 1, -2>
; position=< 7,  6> velocity=<-1, -1>
; position=<-2,  3> velocity=< 1,  0>
; position=<-4,  3> velocity=< 2,  0>
; position=<10, -3> velocity=<-1,  1>
; position=< 5, 11> velocity=< 1, -2>
; position=< 4,  7> velocity=< 0, -1>
; position=< 8, -2> velocity=< 0,  1>
; position=<15,  0> velocity=<-2,  0>
; position=< 1,  6> velocity=< 1,  0>
; position=< 8,  9> velocity=< 0, -1>
; position=< 3,  3> velocity=<-1,  1>
; position=< 0,  5> velocity=< 0, -1>
; position=<-2,  2> velocity=< 2,  0>
; position=< 5, -2> velocity=< 1,  2>
; position=< 1,  4> velocity=< 2,  1>
; position=<-2,  7> velocity=< 2, -2>
; position=< 3,  6> velocity=<-1, -1>
; position=< 5,  0> velocity=< 1,  0>
; position=<-6,  0> velocity=< 2,  0>
; position=< 5,  9> velocity=< 1, -2>
; position=<14,  7> velocity=<-2,  0>
; position=<-3,  6> velocity=< 2, -1>

; Each line represents one point. Positions are given as <X, Y> pairs: X
; represents how far left (negative) or right (positive) the point appears,
; while Y represents how far up (negative) or down (positive) the point
; appears.

; At 0 seconds, each point has the position given. Each second, each point's
; velocity is added to its position. So, a point with velocity <1, -2> is
; moving to the right, but is moving upward twice as quickly. If this point's
; initial position were <3, 9>, after 3 seconds, its position would become <6,
; 3>.

; Over time, the points listed above would move like this:

; Initially:
; ........#.............
; ................#.....
; .........#.#..#.......
; ......................
; #..........#.#.......#
; ...............#......
; ....#.................
; ..#.#....#............
; .......#..............
; ......#...............
; ...#...#.#...#........
; ....#..#..#.........#.
; .......#..............
; ...........#..#.......
; #...........#.........
; ...#.......#..........

; After 1 second:
; ......................
; ......................
; ..........#....#......
; ........#.....#.......
; ..#.........#......#..
; ......................
; ......#...............
; ....##.........#......
; ......#.#.............
; .....##.##..#.........
; ........#.#...........
; ........#...#.....#...
; ..#...........#.......
; ....#.....#.#.........
; ......................
; ......................

; After 2 seconds:
; ......................
; ......................
; ......................
; ..............#.......
; ....#..#...####..#....
; ......................
; ........#....#........
; ......#.#.............
; .......#...#..........
; .......#..#..#.#......
; ....#....#.#..........
; .....#...#...##.#.....
; ........#.............
; ......................
; ......................
; ......................

; After 3 seconds:
; ......................
; ......................
; ......................
; ......................
; ......#...#..###......
; ......#...#...#.......
; ......#...#...#.......
; ......#####...#.......
; ......#...#...#.......
; ......#...#...#.......
; ......#...#...#.......
; ......#...#..###......
; ......................
; ......................
; ......................
; ......................

; After 4 seconds:
; ......................
; ......................
; ......................
; ............#.........
; ........##...#.#......
; ......#.....#..#......
; .....#..##.##.#.......
; .......##.#....#......
; ...........#....#.....
; ..............#.......
; ....#......#...#......
; .....#.....##.........
; ...............#......
; ...............#......
; ......................
; ......................

; After 3 seconds, the message appeared briefly: HI. Of course, your message
; will be much longer and will take many more seconds to appear.

; What message will eventually appear in the sky?

(defn parse-star [s]
  (let [[_ p v] (re-find #"position=<(.*)> velocity=<(.*)>" s)
        parse-point (fn [p]
                      (mapv #(Integer/parseInt (str/trim %))
                            (str/split p #",")))]
    {:position (parse-point p)
     :velocity (parse-point v)}))

(defn parse-stars [f]
  (map parse-star (util/lines f)))

(defn size [stars]
  (let [{:keys [x-min y-min  x-max y-max]} (grid/bounds (map :position stars))]
    (* (- x-max x-min) (- y-max y-min))))

(defn step
  ([stars] (step stars 1))
  ([stars n]
   (doall
     (for [{:keys [position velocity] :as star} stars]
       (assoc star :position (mapv + position (map #(* n %) velocity)))))))

(defn star-message [stars]
  (grid/draw-coords (map :position stars)
                    (fn [xy]
                      (if (some #(= xy (:position %)) stars)
                        "#"
                        "."))))

;; Step second by second, looking for the smallest bounding box

(defn find-message-naive [stars]
  (->> stars
       (iterate step)
       (map-indexed (fn [i stars]
                      {:i i
                       :stars stars
                       :size (size stars)}))
       ;; Find the smallest extent of the star field
       (reduce (fn [prev state]
                 (if (< (:size prev) (:size state))
                   (reduced prev)
                   state)))))

;; A better algorithm -- more or less a binary search

(defn slope-at [stars n]
  (- (size (step stars n))
     (size (step stars (inc n)))))

(defn find-message
  ([stars]
   ;; search between 0 and the first negative slope
   (find-message stars 0
                 (->> (iterate #(* 10 %) 10)
                      (drop-while #(pos? (slope-at stars %)))
                      (first))))
  ([stars min-n max-n]
   (let [n (int (/ (+ min-n max-n) 2))
         slope (slope-at stars n)
         prev-slope (slope-at stars (dec n))]
     #_(println min-n max-n n slope)
     (cond
       ;; we're done when the slopes on either side of this point are
       (or (and (neg? slope) (pos? prev-slope))
           (>= min-n max-n))
       {:steps n
        :stars (step stars n)}
       ;; otherwise narrow the bounds
       (pos? slope) (recur stars (inc n) max-n)
       (neg? slope) (recur stars min-n (dec n))))))

(defn part1 [f]
  (->> (parse-stars f)
       (find-message)
       (:stars)
       (star-message)))

#_ (time (println (util/run part1)))

;;; Part 2

; Good thing you didn't have to wait, because that would have taken a long time
; - much longer than the 3 seconds in the example above.

; Impressed by your sub-hour communication capabilities, the Elves are curious:
; exactly how many seconds would they have needed to wait for that message to
; appear?

(defn part2 [f]
  (->> (parse-stars f)
       (find-message)
       (:steps)))

#_ (time (util/run part2))

(ns advent-2019.day19
  "Tractor Beam"
  (:require [advent.grid :as grid]
            [advent.util :as util]
            [advent-2019.intcode :as ic]))

;;; Part 1

; Unsure of the state of Santa's ship, you borrowed the tractor beam technology
; from Triton. Time to test it out.

; When you're safely away from anything else, you activate the tractor beam,
; but nothing happens. It's hard to tell whether it's working if there's
; nothing to use it on. Fortunately, your ship's drone system can be configured
; to deploy a drone to specific coordinates and then check whether it's being
; pulled. There's even an Intcode program (your puzzle input) that gives you
; access to the drone system.

; The program uses two input instructions to request the X and Y position to
; which the drone should be deployed. Negative numbers are invalid and will
; confuse the drone; all numbers should be zero or positive.

; Then, the program will output whether the drone is stationary (0) or being
; pulled by something (1). For example, the coordinate X=0, Y=0 is directly in
; front of the tractor beam emitter, so the drone control program will always
; report 1 at that location.

; To better understand the tractor beam, it is important to get a good picture
; of the beam itself. For example, suppose you scan the 10x10 grid of points
; closest to the emitter:

;        X
;   0->      9
;  0#.........
;  |.#........
;  v..##......
;   ...###....
;   ....###...
; Y .....####.
;   ......####
;   ......####
;   .......###
;  9........##

; In this example, the number of points affected by the tractor beam in the
; 10x10 area closest to the emitter is 27.

; However, you'll need to scan a larger area to understand the shape of the
; beam. How many points are affected by the tractor beam in the 50x50 area
; closest to the emitter? (For each of X and Y, this will be 0 through 49.)

(def checked (atom {}))
(defn check-point [program xy]
  (swap! checked update xy (fnil inc 0))
  (-> program
      (ic/send-input xy)
      (ic/resume)
      (ic/read-output)
      (first)))

;; draw the full grid
#_ (let [program (-> (ic/parse-code (util/data)) (ic/program))]
     (println
      (grid/draw-bounds
       {:x-min 0 :x-max 75
        :y-min 0 :y-max 75}
       (fn [x y]
         (case (check-point program [x y])
           1 "#"
           0 ".")))))

;; The naive method: check all the points
#_ (time
    (let [program (ic/program (ic/parse-code (util/data)))]
      (->> (for [x (range 50)
                 y (range 50)]
             (check-point program [x y]))
           (reduce + ))))

;; A better method: keep track of just the edges

;; check-point is expensive, so we definitely don't want to use it as a filter
;; on a chunked sequence.
(defn unchunk [s]
  (when-first [head s]
    (lazy-seq (cons head
                    (unchunk (rest s))))))

(defn leading-x-edge [program y x-start x-end]
  (->> (range (max x-start 0) x-end)
       (unchunk)
       (filter #(= 1 (check-point program [% y])))
       (first)))

(defn trailing-x-edge [program y x-start x-end]
  (->> (range (max x-start 0) x-end)
       (unchunk)
       (take-while #(= 1 (check-point program [% y])))
       (last)))

(defn find-x-bounds [program y [x1 x2]]
  (if-let [x1' (leading-x-edge program y x1 50)]
    (let [x2' (trailing-x-edge program y (max (inc x1') x2) 50)]
      [x1' (or x2' x1')])
    [-1 -1]))

(defn part1 [f]
  (let [program (ic/program (ic/parse-code f))]
    (loop [y 0
           [x1 x2] [-1 -1]
           beam-points 0]
      (if (< y 50)
        (let [[x1 x2] (find-x-bounds program y [x1 x2])]
          (recur (inc y)
                 [x1 x2]
                 (if-not (neg? x1)
                   (+ beam-points (- x2 x1) 1)
                   beam-points)))
        beam-points))))

#_ (time (util/run part1))

;;; Part 2

; You aren't sure how large Santa's ship is. You aren't even sure if you'll
; need to use this thing on Santa's ship, but it doesn't hurt to be prepared.
; You figure Santa's ship might fit in a 100x100 square.

; The beam gets wider as it travels away from the emitter; you'll need to be a
; minimum distance away to fit a square of that size into the beam fully.
; (Don't rotate the square; it should be aligned to the same axes as the drone
; grid.)

; For example, suppose you have the following tractor beam readings:

; #.......................................
; .#......................................
; ..##....................................
; ...###..................................
; ....###.................................
; .....####...............................
; ......#####.............................
; ......######............................
; .......#######..........................
; ........########........................
; .........#########......................
; ..........#########.....................
; ...........##########...................
; ...........############.................
; ............############................
; .............#############..............
; ..............##############............
; ...............###############..........
; ................###############.........
; ................#################.......
; .................########OOOOOOOOOO.....
; ..................#######OOOOOOOOOO#....
; ...................######OOOOOOOOOO###..
; ....................#####OOOOOOOOOO#####
; .....................####OOOOOOOOOO#####
; .....................####OOOOOOOOOO#####
; ......................###OOOOOOOOOO#####
; .......................##OOOOOOOOOO#####
; ........................#OOOOOOOOOO#####
; .........................OOOOOOOOOO#####
; ..........................##############
; ..........................##############
; ...........................#############
; ............................############
; .............................###########

; In this example, the 10x10 square closest to the emitter that fits entirely
; within the tractor beam has been marked O. Within it, the point closest to
; the emitter (the only highlighted O) is at X=25, Y=20.

; Find the 100x100 square closest to the emitter that fits entirely within the
; tractor beam; within that square, find the point closest to the emitter. What
; value do you get if you take that point's X coordinate, multiply it by 10000,
; then add the point's Y coordinate? (In the example above, this would be
; 250020.)

(defn santa-fits? [program [x y :as bottom-left]]
  (and (< 100 y)
       ;; check bottom left
       (= 1 (check-point program bottom-left))
       ;; check top right
       (= 1 (check-point program [(+ x 99) (- y 99)]))))

(defn find-next-edge
  ([program xy] (find-next-edge program xy 10))
  ([program [x y] limit]
   (when-let [x' (leading-x-edge program y x (+ x limit))]
     [x' y])))

(defn find-tractor-beam-distance [program]
  (loop [[x y] [0 0]]
    (if (santa-fits? program [x y])
      ;; return top-left corner
      [x (- y 99)]
      ;; shift down a row and find the leading edge
      (let [edge [x (inc y)]]
        (recur (or (find-next-edge program edge)
                   edge))))))

(defn part2 [f]
  (let [program (ic/program (ic/parse-code f))
        [x y] (find-tractor-beam-distance program)]
  (+ (* x 10000) y)))

#_ (time (util/run part2))

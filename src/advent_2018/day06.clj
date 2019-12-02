(ns advent-2018.day06
  "Chronal Coordinates"
  (:require [advent.util :as util]
            [clojure.string :as str]))

;;; Part 1

; The device on your wrist beeps several times, and once again you feel like
; you're falling.

; "Situation critical," the device announces. "Destination indeterminate.
; Chronal interference detected. Please specify new target coordinates."

; The device then produces a list of coordinates (your puzzle input). Are they
; places it thinks are safe or dangerous? It recommends you check manual page
; 729. The Elves did not give you a manual.

; If they're dangerous, maybe you can minimize the danger by finding the
; coordinate that gives the largest distance from the other points.

; Using only the Manhattan distance, determine the area around each coordinate
; by counting the number of integer X,Y locations that are closest to that
; coordinate (and aren't tied in distance to any other coordinate).

; Your goal is to find the size of the largest area that isn't infinite. For
; example, consider the following list of coordinates:

; 1, 1
; 1, 6
; 8, 3
; 3, 4
; 5, 5
; 8, 9

; If we name these coordinates A through F, we can draw them on a grid, putting
; 0,0 at the top left:

; ..........
; .A........
; ..........
; ........C.
; ...D......
; .....E....
; .B........
; ..........
; ..........
; ........F.

; This view is partial - the actual grid extends infinitely in all directions.
; Using the Manhattan distance, each location's closest coordinate can be
; determined, shown here in lowercase:

; aaaaa.cccc
; aAaaa.cccc
; aaaddecccc
; aadddeccCc
; ..dDdeeccc
; bb.deEeecc
; bBb.eeee..
; bbb.eeefff
; bbb.eeffff
; bbb.ffffFf

; Locations shown as . are equally far from two or more coordinates, and so
; they don't count as being closest to any.

; In this example, the areas of coordinates A, B, C, and F are infinite - while
; not shown here, their areas extend forever outside the visible grid. However,
; the areas of coordinates D and E are finite: D is closest to 9 locations, and
; E is closest to 17 (both including the coordinate's location itself).
; Therefore, in this example, the size of the largest area is 17.

; What is the size of the largest area that isn't infinite?

(defn parse-locations [f]
  (->> (util/lines f)
       (map-indexed (fn [i line]
                      (let [[x y] (str/split line #"\s*,\s*")]
                        {:id i
                         :x (Integer/parseInt x)
                         :y (Integer/parseInt y)})))))

(defn bounds [coords]
  {:x-min (apply min (map :x coords))
   :x-max (apply max (map :x coords))
   :y-min (apply min (map :y coords))
   :y-max (apply max (map :y coords))})

(defn all-coords [bounds]
  (for [x (range (:x-min bounds) (inc (:x-max bounds)))
        y (range (:y-min bounds) (inc (:y-max bounds)))]
    {:x x :y y}))

(defn on-edge? [bounds {:keys [x y]}]
  (or (= (:x-min bounds) x)
      (= (:x-max bounds) x)
      (= (:y-min bounds) y)
      (= (:y-may bounds) y)))

(defn dist [{x1 :x y1 :y} {x2 :x y2 :y}]
  ;; This is very much the hot path in this ns, so marking types here speeds
  ;; things up _significantly_ (by a factor of about 10)
  (+ (Math/abs (- (int x1) (int x2)))
     (Math/abs (- (int y1) (int y2)))))

(defn closest-location
  "Returns the closest location to this coord.

  If multiple locations are the closest, returns nil."
  [coord locations]
  (let [[_ closest] (reduce
                     (fn [[closest locs] l]
                       (let [d (dist coord l)]
                         (cond
                           (< d closest) [d #{l}]
                           (= d closest) [d (conj locs l)]
                           :else [closest locs])))
                     [Integer/MAX_VALUE #{}]
                     locations)]
    (when (= 1 (count closest))
      (first closest))))

(defn part1 [f]
  (let [locations (parse-locations f)
        bounds (bounds locations)]
    (->> (all-coords bounds)
         (map #(assoc % :location (closest-location % locations)))
         ;; exclude points without a single closest location
         (filter :location)
         ;; exclude locations w/ edge squares (since these have infinite area)
         (group-by :location)
         (remove #(some (partial on-edge? bounds) (val %)))
         ;; find the location with the most squares
         (map (comp count val))
         (apply max))))

#_ (time (util/run part1))

;;; Part 2

; On the other hand, if the coordinates are safe, maybe the best you can do is
; try to find a region near as many coordinates as possible.

; For example, suppose you want the sum of the Manhattan distance to all of the
; coordinates to be less than 32. For each location, add up the distances to
; all of the given coordinates; if the total of those distances is less than
; 32, that location is within the desired region. Using the same coordinates as
; above, the resulting region looks like this:

; ..........
; .A........
; ..........
; ...###..C.
; ..#D###...
; ..###E#...
; .B.###....
; ..........
; ..........
; ........F.

; In particular, consider the highlighted location 4,3 located at the top
; middle of the region. Its calculation is as follows, where abs() is the
; absolute value function:

;     Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;     Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;     Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;     Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;     Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;     Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;     Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

; Because the total distance to all coordinates (30) is less than 32, the
; location is within the region.

; This region, which also includes coordinates D and E, has a total size of 16.

; Your actual region will need to be much larger than this example, though,
; instead including all locations with a total distance of less than 10000.

; What is the size of the region containing all locations which have a total
; distance to all given coordinates of less than 10000?

(defn sum-distances [s locations]
  (reduce + (map dist (repeat s) locations)))

(defn part2 [f]
  (let [target-distance 10000
        locations (parse-locations f)
        bounds (bounds locations)]
    (->> (all-coords bounds)
         (map #(sum-distances % locations))
         (filter #(and % (< % target-distance)))
         (count))))

#_ (time (util/run part2))
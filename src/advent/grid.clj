(ns advent.grid
  "Grid utils"
  (:require [clojure.string :as str]))

(defn draw-bounds
  "Returns a string representation of a grid, given a map of `bounds` and a
  function `f`.

  `bounds` contains keys :x-min, :x-max :y-min :y-max

  `f` is a function taking x and y and returning a string."
  [{:keys [x-min x-max y-min y-max] :as bounds} f]
  (str/join
   "\n"
   (for [y (range y-min (inc y-max))]
     (str/join
      (for [x (range x-min (inc x-max))]
        (f x y))))))

(defn bounds [coords]
  {:x-min (apply min (map first coords))
   :x-max (apply max (map first coords))
   :y-min (apply min (map second coords))
   :y-max (apply max (map second coords))})

(defn draw-coords
  "Returns a string representation of a grid, given a seq of [x y] `coords` and
  a function `f`.

  `f` is a function taking [x y] and returning a string."
  [coords f]
  (draw-bounds (bounds coords) #(f [%1 %2])))

(defn draw-map
  "Returns a string representation of a `grid` stored as a map, where the keys
  are [x y] pairs.

  `f` is a function taking a grid val that should return a string."
  [grid f]
  (draw-coords (keys grid) #(f (get grid %))))

(defn parse
  "Parses a grid represented by a string in `input`.

  Calls `(f [x y] character)` for each location.

  Returns a seq of non-nil returns from `f`."
  [input f]
  (letfn [(parse-line [y line]
            (map-indexed (fn [x ch] (f [x y] ch)) line))]
    (->> (str/split-lines input)
         (map-indexed parse-line)
         (apply concat)
         (filter identity))))

(ns advent-2019.day15
  "Oxygen System"
  (:require [advent.util :as util]
            [advent.grid :as grid]
            [advent-2019.intcode :as ic]))

;;; Part 1

; Out here in deep space, many things can go wrong. Fortunately, many of those
; things have indicator lights. Unfortunately, one of those lights is lit: the
; oxygen system for part of the ship has failed!

; According to the readouts, the oxygen system must have failed days ago after
; a rupture in oxygen tank two; that section of the ship was automatically
; sealed once oxygen levels went dangerously low. A single remotely-operated
; repair droid is your only option for fixing the oxygen system.

; The Elves' care package included an Intcode program (your puzzle input) that
; you can use to remotely control the repair droid. By running that program,
; you can direct the repair droid to the oxygen system and fix the problem.

; The remote control program executes the following steps in a loop forever:

;   - Accept a movement command via an input instruction.
;   - Send the movement command to the repair droid.
;   - Wait for the repair droid to finish the movement operation.
;   - Report on the status of the repair droid via an output instruction.

; Only four movement commands are understood: north (1), south (2), west (3),
; and east (4). Any other command is invalid. The movements differ in
; direction, but not in distance: in a long enough east-west hallway, a series
; of commands like 4,4,4,4,3,3,3,3 would leave the repair droid back where it
; started.

; The repair droid can reply with any of the following status codes:

;   - 0: The repair droid hit a wall. Its position has not changed.
;   - 1: The repair droid has moved one step in the requested direction.
;   - 2: The repair droid has moved one step in the requested direction; its
;     new position is the location of the oxygen system.

; You don't know anything about the area around the repair droid, but you can
; figure it out by watching the status codes.

; For example, we can draw the area using D for the droid, # for walls, . for
; locations the droid can traverse, and empty space for unexplored locations.
; Then, the initial state looks like this:

;       
;       
;    D  
;       
;       

; To make the droid go north, send it 1. If it replies with 0, you know that
; location is a wall and that the droid didn't move:

;       
;    #  
;    D  
;       
;       

; To move east, send 4; a reply of 1 means the movement was successful:

;       
;    #  
;    .D 
;       
;       

; Then, perhaps attempts to move north (1), south (2), and east (4) are all met
; with replies of 0:

;       
;    ## 
;    .D#
;     # 
;       

; Now, you know the repair droid is in a dead end. Backtrack with 3 (which you
; already know will get a reply of 1 because you already know that location is
; open):

;       
;    ## 
;    D.#
;     # 
;       

; Then, perhaps west (3) gets a reply of 0, south (2) gets a reply of 1, south
; again (2) gets a reply of 0, and then west (3) gets a reply of 2:

;       
;    ## 
;   #..#
;   D.# 
;    #  

; Now, because of the reply of 2, you know you've found the oxygen system! In
; this example, it was only 2 moves away from the repair droid's starting
; position.

; What is the fewest number of movement commands required to move the repair
; droid from its starting position to the location of the oxygen system?


;; set to > 0 to draw a graph at each step of the pathfinding algorithm
(def ^:dynamic *draw-sleep* 0)

(defn draw-graph [graph]
  (grid/draw-map graph
                 #(case (:type %)
                    :start "S"
                    :droid "@"
                    :empty "."
                    :wall "█"
                    :oxygen "O"
                    nil " ")))


(defn empty-graph [program]
  (cond-> {[0 0] {:program program,
                  :type :start
                  :pos [0 0]}}
    ;; dummy bounds makes the drawing nicer
    (pos? *draw-sleep*) (merge {[-22 -22] nil
                                [22 22] nil})))

(defn move-coord [[x y] dir]
  (case dir
    1 [x (dec y)] ; north
    2 [x (inc y)] ; south
    3 [(dec x) y] ; west
    4 [(inc x) y] ; east
    ))

(defn oxygen? [node]
  (= :oxygen (:type node)))

(defn wall? [node]
  (= :wall (:type node)))

(defn explore-neighbor
  "Moves the droid one step in `dir`.

  Returns a new node for the square in that direction."
  [program dir]
  (let [program' (-> (ic/send-input program [dir])
                     (ic/resume))
        output (first (ic/read-output program'))]
    {:program (ic/consume-output program')
     :type (case output
             0 :wall
             1 :empty
             2 :oxygen)}))

(defn explore-neighbors
  "Returns nodes for all neighbors of the input `pos`."
  [graph pos]
  (let [{:keys [program]} (get graph pos)]
    (map (fn [dir]
           (let [pos' (move-coord pos dir)]
             (or ;; already explored
                 (get graph pos')
                 ;; explore a new node
                 (-> (explore-neighbor program dir)
                     (assoc :pos pos')))))
         [1 2 3 4])))

(defn add-node [graph node]
  (assoc graph (:pos node) node))

(defn set-distance [graph pos dist]
  (assoc-in graph [pos :distance] dist))

(defn dijkstra-infinite
  "Dijkstra's algorithm on a graph of unknown size.

  Returns an explored graph with distances."
  [graph start-pos target?]
  (loop [graph graph
         frontier (conj (clojure.lang.PersistentQueue/EMPTY) [start-pos 0])]
    (let [[pos dist] (peek frontier)]
      (cond
        ;; Found the target
        (target? (get graph pos))
        (set-distance graph pos dist)
        ;; Explored the whole graph
        (not pos)
        graph
        ;; Next step
        :else
        (let [neighbors (explore-neighbors graph pos)
              graph (reduce add-node graph neighbors)]
          (when (pos? *draw-sleep*)
            (println (draw-graph graph))
            (Thread/sleep *draw-sleep*))
          (recur (set-distance graph pos dist)
                 (->> neighbors
                      (remove wall?) ;; can't explore walls
                      (remove :distance) ;; remove already seen
                      (map #(vector (:pos %) (inc dist)))
                      (into (pop frontier)))))))))

(defn part1 [f]
  (let [program (ic/program (ic/parse-code f))
        graph (dijkstra-infinite (empty-graph program) [0 0] oxygen?)]
    (->> (filter oxygen? (vals graph))
         (first)
         (:distance))))

#_ (time (util/run part1))

;;; Part 2

; You quickly repair the oxygen system; oxygen gradually fills the area.

; Oxygen starts in the location containing the repaired oxygen system. It takes
; one minute for oxygen to spread to all open locations that are adjacent to a
; location that already contains oxygen. Diagonal locations are not adjacent.

; In the example above, suppose you've used the droid to explore the area fully
; and have the following map (where locations that currently contain oxygen are
; marked O):

;  ##   
; #..## 
; #.#..#
; #.O.# 
;  ###  

; Initially, the only location which contains oxygen is the location of the
; repaired oxygen system. However, after one minute, the oxygen spreads to all
; open (.) locations that are adjacent to a location containing oxygen:

;  ##   
; #..## 
; #.#..#
; #OOO# 
;  ###  

; After a total of two minutes, the map looks like this:

;  ##   
; #..## 
; #O#O.#
; #OOO# 
;  ###  

; After a total of three minutes:

;  ##   
; #O.## 
; #O#OO#
; #OOO# 
;  ###  

; And finally, the whole region is full of oxygen after a total of four minutes:

;  ##   
; #OO## 
; #O#OO#
; #OOO# 
;  ###  

; So, in this example, all locations contain oxygen after 4 minutes.

; Use the repair droid to get a complete map of the area. How many minutes will
; it take to fill with oxygen?

(defn part2 [f]
  (let [;; start by finding the oxygen tank
        program (ic/program (ic/parse-code f))
        graph (dijkstra-infinite (empty-graph program) [0 0] oxygen?)
        ;; then start _at_ the oxygen tank and find the longest path
        oxygen (first (filter oxygen? (vals graph)))
        graph (if (pos? *draw-sleep*)
                ;; reset the whole graph for the visualization
                (add-node (empty-graph program) oxygen)
                ;; otherwise reset just the distances for efficiency
                (util/map-vals #(dissoc % :distance) graph))
        graph (dijkstra-infinite graph (:pos oxygen) (constantly false))]
    (apply max (keep :distance (vals graph)))))

#_ (time (util/run part2))

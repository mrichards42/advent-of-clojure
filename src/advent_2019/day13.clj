(ns advent-2019.day13
  "Care Package"
  (:require [advent.util :as util]
            [advent.grid :as grid]
            [advent-2019.intcode :as ic]))

;;; Part 1

; As you ponder the solitude of space and the ever-increasing three-hour
; roundtrip for messages between you and Earth, you notice that the Space Mail
; Indicator Light is blinking. To help keep you sane, the Elves have sent you a
; care package.

; It's a new game for the ship's arcade cabinet! Unfortunately, the arcade is
; all the way on the other end of the ship. Surely, it won't be hard to build
; your own - the care package even comes with schematics.

; The arcade cabinet runs Intcode software like the game the Elves sent (your
; puzzle input). It has a primitive screen capable of drawing square tiles on a
; grid. The software draws tiles to the screen with output instructions: every
; three output instructions specify the x position (distance from the left), y
; position (distance from the top), and tile id. The tile id is interpreted as
; follows:

;   - 0 is an empty tile. No game object appears in this tile.
;   - 1 is a wall tile. Walls are indestructible barriers.
;   - 2 is a block tile. Blocks can be broken by the ball.
;   - 3 is a horizontal paddle tile. The paddle is indestructible.
;   - 4 is a ball tile. The ball moves diagonally and bounces off objects.

; For example, a sequence of output values like 1,2,3,6,5,4 would draw a
; horizontal paddle tile (1 tile from the left and 2 tiles from the top) and a
; ball tile (6 tiles from the left and 5 tiles from the top).

; Start the game. How many block tiles are on the screen when the game exits?


(def tiles
  [{:id :empty,  :str " "}
   {:id :wall,   :str "█"}
   {:id :block,  :str "#"}
   {:id :paddle, :str "_"}
   {:id :ball,   :str "o"}])

(defn tile [idx]
  (get-in tiles [idx :str]))

(defn tile-id [idx]
  (get-in tiles [idx :id]))

(def blank-screen {:score 0 :board {}})

(defn update-screen [screen instructions]
  (reduce (fn [screen [x y v]]
            (if (= [-1 0] [x y])
              (assoc screen :score v)
              (cond-> (assoc-in screen [:board [x y]] v)
                (= :paddle (tile-id v)) (assoc :paddle-x x)
                (= :ball (tile-id v)) (assoc :ball-x x))))
          screen
          (partition-all 3 instructions)))

(defn draw-screen [{:keys [board score]}]
  (str
   ;; score
   (format "score: %d" (or score 0))
   "\n"
   ;; grid
   (grid/draw-map board #(tile (or % 0)))))

(defn part1 [f]
  (let [program (ic/program (ic/parse-code f))
        screen blank-screen
        output (-> program
                   ic/resume
                   ic/read-output)
        screen (update-screen screen output)]
    (->> (vals (:board screen))
         (map tile-id)
         (filter #{:block})
         (count))))

#_ (time (util/run part1))

;;; Part 2

; The game didn't run because you didn't put in any quarters. Unfortunately,
; you did not bring any quarters. Memory address 0 represents the number of
; quarters that have been inserted; set it to 2 to play for free.

; The arcade cabinet has a joystick that can move left and right. The software
; reads the position of the joystick with input instructions:

;   - If the joystick is in the neutral position, provide 0.
;   - If the joystick is tilted to the left, provide -1.
;   - If the joystick is tilted to the right, provide 1.

; The arcade cabinet also has a segment display capable of showing a single
; number that represents the player's current score. When three output
; instructions specify X=-1, Y=0, the third output instruction is not a tile;
; the value instead specifies the new score to show in the segment display. For
; example, a sequence of output values like -1,0,12345 would show 12345 as the
; player's current score.

; Beat the game by breaking all the blocks. What is your score after the last
; block is broken?

(defn find-tile [screen id]
  (->> (:board screen)
       (filter #(= id (tile-id (val %))))
       (first)))

(defn ai-joystick [{:keys [ball-x paddle-x] :as screen}]
  ;; Move the paddle left or right to track the ball
  ;; This isn't necessarily the most _efficient_ way to win, but it should
  ;; always work
  (cond
    (< ball-x paddle-x) -1 ;; move left
    (> ball-x paddle-x)  1 ;; move right
    :else 0))

(def sleep (atom 0))

(defn display [screen]
  (when (pos? @sleep)
    (Thread/sleep @sleep)
    (println (draw-screen screen))))

(defn part2 [f]
  (let [program (ic/program (ic/parse-code f))
        ;; add quarters
        program (ic/put-memory program 0 2)]
    (loop [program (ic/resume program)
           ;; play the game
           screen (update-screen blank-screen (ic/read-output program))]
      (if (ic/end? program)
        ;; get the score
        (:score screen)
        ;; move the joystick and run until the next pause
        (let [joystick (ai-joystick screen)
              program' (-> (ic/send-input program [joystick])
                           (ic/resume))]
          (recur (ic/consume-output program')
                 (-> screen
                     (update-screen (ic/read-output program'))
                     (doto display))))))))

#_ (time (util/run part2))

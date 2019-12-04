(ns advent-2018.day09
  "Marble Mania"
  (:require [advent.util :as util]))

;;; Part 1

; You talk to the Elves while you wait for your navigation system to
; initialize. To pass the time, they introduce you to their favorite marble
; game.

; The Elves play this game by taking turns arranging the marbles in a circle
; according to very particular rules. The marbles are numbered starting with 0
; and increasing by 1 until every marble has a number.

; First, the marble numbered 0 is placed in the circle. At this point, while it
; contains only a single marble, it is still a circle: the marble is both
; clockwise from itself and counter-clockwise from itself. This marble is
; designated the current marble.

; Then, each Elf takes a turn placing the lowest-numbered remaining marble into
; the circle between the marbles that are 1 and 2 marbles clockwise of the
; current marble. (When the circle is large enough, this means that there is
; one marble between the marble that was just placed and the current marble.)
; The marble that was just placed then becomes the current marble.

; However, if the marble that is about to be placed has a number which is a
; multiple of 23, something entirely different happens. First, the current
; player keeps the marble they would have placed, adding it to their score. In
; addition, the marble 7 marbles counter-clockwise from the current marble is
; removed from the circle and also added to the current player's score. The
; marble located immediately clockwise of the marble that was removed becomes
; the new current marble.

; For example, suppose there are 9 players. After the marble with value 0 is
; placed in the middle, each player (shown in square brackets) takes a turn.
; The result of each of those turns would produce circles of marbles like this,
; where clockwise is to the right and the resulting current marble is in
; parentheses:

; [-] (0)
; [1]  0 (1)
; [2]  0 (2) 1 
; [3]  0  2  1 (3)
; [4]  0 (4) 2  1  3 
; [5]  0  4  2 (5) 1  3 
; [6]  0  4  2  5  1 (6) 3 
; [7]  0  4  2  5  1  6  3 (7)
; [8]  0 (8) 4  2  5  1  6  3  7 
; [9]  0  8  4 (9) 2  5  1  6  3  7 
; [1]  0  8  4  9  2(10) 5  1  6  3  7 
; [2]  0  8  4  9  2 10  5(11) 1  6  3  7 
; [3]  0  8  4  9  2 10  5 11  1(12) 6  3  7 
; [4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7 
; [5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7 
; [6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
; [7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15 
; [8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15 
; [9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15 
; [1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15 
; [2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15 
; [3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15 
; [4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15 
; [5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15 
; [6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15 
; [7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15

; The goal is to be the player with the highest score after the last marble is
; used up. Assuming the example above ends after the marble numbered 25, the
; winning score is 23+9=32 (because player 5 kept marble 23 and removed marble
; 9, while no other player got any points in this very short example game).

; Here are a few more examples:

;   - 10 players; last marble is worth 1618 points: high score is 8317
;   - 13 players; last marble is worth 7999 points: high score is 146373
;   - 17 players; last marble is worth 1104 points: high score is 2764
;   - 21 players; last marble is worth 6111 points: high score is 54718
;   - 30 players; last marble is worth 5807 points: high score is 37305

; What is the winning Elf's score?


;; pure-clojure doubly-linked list

(defprotocol ListNode
  (set-next! [this other])
  (set-prev! [this other])
  (next-node [this])
  (prev-node [this]))

(deftype Node [val
               ^:unsynchronized-mutable next
               ^:unsynchronized-mutable prev]
  ListNode
  (next-node [this]
    (.-next this))
  (prev-node [this]
    (.-prev this))
  (set-next! [this other]
    (when-not (= (.-next this) other)
      (set! (.-next this) other)
      (set-prev! other this))
    this)
  (set-prev! [this other]
    (when-not (= (.-prev this) other)
      (set! (.-prev this) other)
      (set-next! other this))
    this))

(defn insert-after
  "Inserts `v` after the current `node`, returning the newly inserted node."
  [node v]
  (let [new-node (->Node v nil nil)]
    (set-next! new-node (next-node node))
    (set-next! node new-node)
    new-node))

(defn remove-node
  "Removes `node`, returning the next node in line."
  [node]
  (let [current (next-node node)]
    (set-next! (prev-node node) (next-node node))
    current))

(defn traverse [node f n]
  (if (zero? n)
    node
    (recur (f node) f (dec n))))

(defn node-circle [init]
  (let [n (->Node init nil nil)]
    (set-next! n n)
    n))


;; Game running functions

(defn active-player [{:keys [scores]} marble]
  (mod (dec marble) ;; the 0th marble is played automatically
       (count scores)))

(defn score-marble [{:keys [circle] :as state} marble]
  (let [to-remove (traverse circle prev-node 7)
        score (+ marble (.-val to-remove))
        circle' (remove-node to-remove)]
    (-> state
        (assoc :circle circle')
        (update-in [:scores (active-player state marble)] + score))))

(defn place-marble [{:keys [circle] :as state} marble]
  (assoc state :circle (insert-after (next-node circle) marble)))

(defn step [state marble]
  (if (zero? (mod marble 23))
    (score-marble state marble)
    (place-marble state marble)))

(defn play-game [player-count marble-count]
  (let [state {:circle (node-circle 0)
               :scores (vec (repeat player-count 0))}
        marbles (map inc (range marble-count))]
    (reduce step state marbles)))

(defn top-score [state]
  (apply max (:scores state)))

(defn read-game [f]
  (->> (util/slurp f)
       (re-find #"(\d+) players; last marble is worth (\d+) points")
       (drop 1)
       (map #(Integer/parseInt %))))

(defn part1 [f]
  (top-score (apply play-game (read-game f))))

#_ (time (util/run part1))

;;; Part 2

; Amused by the speed of your answer, the Elves are curious:

; What would the new winning Elf's score be if the number of the last marble
; were 100 times larger?

(defn part2 [f]
  (let [[players marbles] (read-game f)]
    (top-score (play-game players (* 100 marbles)))))

#_ (time (util/run part2))



(comment
  ;; java deque version (actually slower)
  (do
    (defn node-circle [init]
      (java.util.ArrayDeque. [init]))

    (defn rotate! [circle n]
      (if (pos? n)
        (dotimes [_ n] (.addFirst circle (.removeLast circle)))
        (dotimes [_ (Math/abs n)] (.addLast circle (.removeFirst circle))))
      circle)

    (defn score-marble [{:keys [circle] :as state} marble]
      (rotate! circle 8)
      (let [score (+ marble (.removeFirst circle))]
        (rotate! circle -1)
        (update-in state [:scores (active-player state marble)] + score)))

    (defn place-marble [{:keys [circle] :as state} marble]
      (rotate! circle -1)
      (.addLast circle marble)
      state)
    )


  ;; examples
  (time (top-score (play-game 9 25)))

  (time (top-score (play-game 10 1618)))
  (time (top-score (play-game 13 7999)))
  (time (top-score (play-game 17 1104)))
  (time (top-score (play-game 21 6111)))
  (time (top-score (play-game 30 5807)))

  ;; actual data
  (time (top-score (play-game 462 71938)))
  (time (top-score (play-game 462 7193800)))
  )

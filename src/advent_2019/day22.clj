(ns advent-2019.day22
  "Slam Shuffle"
  (:require [advent.util :as util]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as str]))

;;; Part 1

; There isn't much to do while you wait for the droids to repair your ship. At
; least you're drifting in the right direction. You decide to practice a new
; card shuffle you've been working on.

; Digging through the ship's storage, you find a deck of space cards! Just like
; any deck of space cards, there are 10007 cards in the deck numbered 0 through
; 10006. The deck must be new - they're still in factory order, with 0 on the
; top, then 1, then 2, and so on, all the way through to 10006 on the bottom.

; You've been practicing three different techniques that you use while
; shuffling. Suppose you have a deck of only 10 cards (numbered 0 through 9):

; To deal into new stack, create a new stack of cards by dealing the top card
; of the deck onto the top of the new stack repeatedly until you run out of
; cards:

; Top          Bottom
; 0 1 2 3 4 5 6 7 8 9   Your deck
;                       New stack

;   1 2 3 4 5 6 7 8 9   Your deck
;                   0   New stack

;     2 3 4 5 6 7 8 9   Your deck
;                 1 0   New stack

;       3 4 5 6 7 8 9   Your deck
;               2 1 0   New stack

; Several steps later...

;                   9   Your deck
;   8 7 6 5 4 3 2 1 0   New stack

;                       Your deck
; 9 8 7 6 5 4 3 2 1 0   New stack

; Finally, pick up the new stack you've just created and use it as the deck for
; the next technique.

; To cut N cards, take the top N cards off the top of the deck and move them as
; a single unit to the bottom of the deck, retaining their order. For example,
; to cut 3:

; Top          Bottom
; 0 1 2 3 4 5 6 7 8 9   Your deck

;       3 4 5 6 7 8 9   Your deck
; 0 1 2                 Cut cards

; 3 4 5 6 7 8 9         Your deck
;               0 1 2   Cut cards

; 3 4 5 6 7 8 9 0 1 2   Your deck

; You've also been getting pretty good at a version of this technique where N
; is negative! In that case, cut (the absolute value of) N cards from the
; bottom of the deck onto the top. For example, to cut -4:

; Top          Bottom
; 0 1 2 3 4 5 6 7 8 9   Your deck

; 0 1 2 3 4 5           Your deck
;             6 7 8 9   Cut cards

;         0 1 2 3 4 5   Your deck
; 6 7 8 9               Cut cards

; 6 7 8 9 0 1 2 3 4 5   Your deck

; To deal with increment N, start by clearing enough space on your table to lay
; out all of the cards individually in a long line. Deal the top card into the
; leftmost position. Then, move N positions to the right and deal the next card
; there. If you would move into a position past the end of the space on your
; table, wrap around and keep counting from the leftmost card again. Continue
; this process until you run out of cards.

; For example, to deal with increment 3:


; 0 1 2 3 4 5 6 7 8 9   Your deck
; . . . . . . . . . .   Space on table
; ^                     Current position

; Deal the top card to the current position:

;   1 2 3 4 5 6 7 8 9   Your deck
; 0 . . . . . . . . .   Space on table
; ^                     Current position

; Move the current position right 3:

;   1 2 3 4 5 6 7 8 9   Your deck
; 0 . . . . . . . . .   Space on table
;       ^               Current position

; Deal the top card:

;     2 3 4 5 6 7 8 9   Your deck
; 0 . . 1 . . . . . .   Space on table
;       ^               Current position

; Move right 3 and deal:

;       3 4 5 6 7 8 9   Your deck
; 0 . . 1 . . 2 . . .   Space on table
;             ^         Current position

; Move right 3 and deal:

;         4 5 6 7 8 9   Your deck
; 0 . . 1 . . 2 . . 3   Space on table
;                   ^   Current position

; Move right 3, wrapping around, and deal:

;           5 6 7 8 9   Your deck
; 0 . 4 1 . . 2 . . 3   Space on table
;     ^                 Current position

; And so on:

; 0 7 4 1 8 5 2 9 6 3   Space on table

; Positions on the table which already contain cards are still counted; they're
; not skipped. Of course, this technique is carefully designed so it will never
; put two cards in the same position or leave a position empty.

; Finally, collect the cards on the table so that the leftmost card ends up at
; the top of your deck, the card to its right ends up just below the top card,
; and so on, until the rightmost card ends up at the bottom of the deck.

; The complete shuffle process (your puzzle input) consists of applying many of
; these techniques. Here are some examples that combine techniques; they all
; start with a factory order deck of 10 cards:

; deal with increment 7
; deal into new stack
; deal into new stack
; Result: 0 3 6 9 2 5 8 1 4 7

; cut 6
; deal with increment 7
; deal into new stack
; Result: 3 0 7 4 1 8 5 2 9 6

; deal with increment 7
; deal with increment 9
; cut -2
; Result: 6 3 0 7 4 1 8 5 2 9

; deal into new stack
; cut -2
; deal with increment 7
; cut 8
; cut -4
; deal with increment 7
; cut 3
; deal with increment 9
; deal with increment 3
; cut -1
; Result: 9 2 5 8 1 4 7 0 3 6

; Positions within the deck count from 0 at the top, then 1 for the card
; immediately below the top card, and so on to the bottom. (That is, cards
; start in the position matching their number.)

; After shuffling your factory order deck of 10007 cards, what is the position
; of card 2019?

(defn deal-dispatch [_ instruction]
  (str/trim (re-find #"[a-z\s]+" instruction)))

(defmulti deal deal-dispatch)

(defmethod deal "cut"
  [deck instruction]
  (let [n (Integer/parseInt (re-find #"\S++$" instruction))
        n (if (pos? n) n (+ (count deck) n))]
    (into (subvec deck n) (subvec deck 0 n))))

(defmethod deal "deal into new stack"
  [deck _]
  (vec (reverse deck)))

(defmethod deal "deal with increment"
  [deck instruction]
  (let [n (Integer/parseInt (re-find #"\S+$" instruction))
        deck-size (count deck)]
    (loop [new-deck (vec (repeat deck-size -1)) #_deck
           i 0]
      (if-let [card (get deck i)]
        (recur (assoc new-deck (mod (* n i) deck-size) card)
               (inc i))
        new-deck))))

(defn part1 [f]
  (->> (reduce deal (vec (range 10007)) (util/lines f))
       (map-indexed vector)
       (filter #(= 2019 (second %)))
       (ffirst)))

#_ (time (util/run part1))

;;; Part 2

; After a while, you realize your shuffling skill won't improve much more with
; merely a single deck of cards. You ask every 3D printer on the ship to make
; you some more cards while you check on the ship repairs. While reviewing the
; work the droids have finished so far, you think you see Halley's Comet fly
; past!

; When you get back, you discover that the 3D printers have combined their
; power to create for you a single, giant, brand new, factory order deck of
; 119315717514047 space cards.

; Finally, a deck of cards worthy of shuffling!

; You decide to apply your complete shuffle process (your puzzle input) to the
; deck 101741582076661 times in a row.

; You'll need to be careful, though - one wrong move with this many cards and
; you might overflow your entire ship!

; After shuffling your new, giant, factory order deck that many times, what
; number is on the card that ends up in position 2020?


;;; Instruction simplification
;; all examples use a deck of 11 cards, and "X" for the 10th card

(defmulti simplify-instruction deal-dispatch)

;; "cut n" means (+ n %)

; cut 5
; 0 1 2 3 4 5 6 7 8 9 X
; 5 6 7 8 9 X 0 1 2 3 3

(defmethod simplify-instruction "cut"
  [_ instruction]
  (let [n (Integer/parseInt (re-find #"\S++$" instruction))]
    {:plus n}))

;; "deal with increment n" means (* x %) where x is the modular multiplicative
;; inverse of n: https://en.wikipedia.org/wiki/Modular_multiplicative_inverse

;; Here's how this works:
;; we need to solve for the card at index 1

; increment 3
; 0 1 2 3 4 5 6 7 8 9 X
; 0 . . 1 . . 2 . . 3 .
; 0 4 . 1 . . 2 . . 3 . <-- 4: 3x = 1 (mod 11), so x = 4

; increment 5
; 0 1 2 3 4 5 6 7 8 9 X
; 0 . . . . 1 . . . . 2
; 0 . . . 3 1 . . . 4 2
; 0 . . 5 3 1 . . 6 4 2
; 0 . 7 5 3 1 . 8 6 4 2
; 0 9 7 5 3 1 X 8 6 4 2 <-- 9: 5x = 1 (mod 11), so x = 9

; increment 7
; 0 1 2 3 4 5 6 7 8 9 X
; 0 . . . . . . 1 . . .
; 0 . . 2 . . . 1 . . 3
; 0 . . 2 . . 4 1 . . 3
; 0 . 5 2 . . 4 1 . 6 3
; 0 . 5 2 . 7 4 1 . 6 3
; 0 8 5 2 . 7 4 1 9 6 3 <-- 8: 7x = 1 (mod 11), so x = 8

; increment 9
; 0 1 2 3 4 5 6 7 8 9 X
; 0 . . . . . . . . 1 .
; 0 . . . . . . 2 . 1 .
; 0 . . . . 3 . 2 . 1 .
; 0 . . 4 . 3 . 2 . 1 .
; 0 . . 4 . 3 . 2 . 1 .
; 0 . . 4 . 3 . 2 . 1 .
; 0 5 . 4 . 3 . 2 . 1 . <-- 5: 9x = 1 (mod 11), so x = 5

(defn mod-inverse [a m]
  ;; hey look, java has this built in
  (.modInverse (biginteger a) (biginteger m)))

(defmethod simplify-instruction "deal with increment"
  [deck-size instruction]
  (let [n (Integer/parseInt (re-find #"\S+$" instruction))]
    {:times (mod-inverse n deck-size)}))

;; "deal into new stack" equates to "cut -1" + "deal with increment -1"
;; based on the "combining instructions" section, this can be simplified to
;; (+ -1 (* (mod-inverse -1 deck-size) %))

; cut -1
; 0 1 2 3 4 5 6 7 8 9 X
; X 0 1 2 3 4 5 6 7 8 9
; increment -1
; X . . . . . . . . . 0
; X . . . . . . . . 1 0
; [...]
; X 9 8 7 6 5 4 3 2 1 0

(defmethod simplify-instruction "deal into new stack"
  [deck-size _]
  {:times (mod-inverse -1 deck-size)
   :plus -1})

;; combining instructions happens from the outside in; that is, earlier steps
;; go on the outside, and later steps go on the inside:

; cut 2; deal with increment 3
; (+ 2 (* (mod-inverse 3 11) %))

; deal 3; cut 2
; (* (mod-inverse 3 11) (+ 2 %))

; cut 2; deal 3; cut 4
; (+ 2 (* (mod-inverse 3 11) (+ 4 %)))

; cut 2; deal 3; cut 4; deal 7
; (+ 2 (* (mod-inverse 3 11) (+ 4 (* (mod-inverse 8 11) %)))))

; deal 9; deal 7
; (* (mod-inverse 9 11) (* (mod-inverse 7 11) %))

; cut 2; cut 4
; (+ 2 (+ 4 %))

;; This means we can reduce any set of instructions to ax + b, e.g.

; - cut 2; deal 3; cut 4; deal 7
;   (+ 2 (* (mod-inverse 3 11) (+ 4 (* (mod-inverse 7 11) %))))
;   (+ 2 (* 4 (+ 4 (* 8 %))))
;   (+ 2 16 (* 4 8 %))
;   (+ 18 (* 32 %))   ; or 32x + 18

(defn combine-instructions
  "Combines two simplified instructions"
  [deck-size a b]
  (let [times' (*' (:times a 1) (:times b 1))
        plus' (+' (:plus b 0) (*' (:times b 1) (:plus a 0)))]
    ;; prevent overflow by taking both terms mod deck-size at each step
    {:times (mod times' deck-size)
     :plus (mod plus' deck-size)}))

(defn simplify-instructions
  "Simplifies a set of textual instructions into a map of `:times` and `:plus`"
  [deck-size instructions]
  (->> instructions
       (map (partial simplify-instruction deck-size))
       ;; the last instruction needs to be on the inside
       (reverse)
       (reduce (partial combine-instructions deck-size))))

;;; Instruction repetition

;; Repeating a set of instructions `n` times is simply:
;; (reduce combine-instructions (repeat n instruction))

(defn repeat-instructions
  "Repeats a simplified instruction `n` times."
  [n deck-size a]
  (case n
    0 {:times 1 :plus 0}
    1 a
    2 (combine-instructions deck-size a a)
    ;; else split the problem into pieces
    (let [doubled (repeat-instructions 2 deck-size a)]
      (combine-instructions deck-size
                 ;; repeat the doubled version as many times as possible
                 (repeat-instructions (quot n 2) deck-size doubled)
                 ;; repeat the single version the remainder
                 (repeat-instructions (rem n 2) deck-size a)))))

(defn instructions->formula
  "Returns a formula (as a function) derived from a set of textual
  `instructions`, optionally repeated `n` times."
  ([deck-size instructions]
   (instructions->formula deck-size instructions 1))
  ([deck-size instructions n]
   (let [;; parse the initial instructions
         simplified (simplify-instructions deck-size instructions)
         ;; repeat the instructions `n` times
         {:keys [plus times]} (repeat-instructions n deck-size simplified)]
     (fn [x] (mod (+ plus (* times x)) deck-size)))))

;; checking our work
#_ (time
    (let [f (instructions->formula 10007 (util/lines (util/data)) 3)]
      (map f (range 10))))

#_ (time
    (->> (reduce deal (vec (range 10007)) (concat (util/lines (util/data))
                                                  (util/lines (util/data))
                                                  (util/lines (util/data))))
         (take 10)))


(defn part2 [f]
  (let [deck-size 119315717514047
        repeat-n 101741582076661
        f (instructions->formula deck-size (util/lines f) repeat-n)]
    (f 2020)))

#_ (time (util/run part2))

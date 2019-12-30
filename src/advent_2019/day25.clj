(ns advent-2019.day25
  "Cryostasis"
  (:require [advent.util :as util]
            [advent-2019.intcode :as ic]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]))

;;; Part 1

; As you approach Santa's ship, your sensors report two important details:

; First, that you might be too late: the internal temperature is -40 degrees.

; Second, that one faint life signature is somewhere on the ship.

; The airlock door is locked with a code; your best option is to send in a
; small droid to investigate the situation. You attach your ship to Santa's,
; break a small hole in the hull, and let the droid run in before you seal it
; up again. Before your ship starts freezing, you detach your ship and set it
; to automatically stay within range of Santa's ship.

; This droid can follow basic instructions and report on its surroundings; you
; can communicate with it through an Intcode program (your puzzle input)
; running on an ASCII-capable computer.

; As the droid moves through its environment, it will describe what it
; encounters. When it says Command?, you can give it a single instruction
; terminated with a newline (ASCII code 10). Possible instructions are:

;   - Movement via north, south, east, or west.
;   - To take an item the droid sees in the environment, use the command take
;     <name of item>. For example, if the droid reports seeing a red ball, you
;     can pick it up with take red ball.
;   - To drop an item the droid is carrying, use the command drop <name of
;     item>. For example, if the droid is carrying a green ball, you can drop
;     it with drop green ball.
;   - To get a list of all of the items the droid is currently carrying, use
;     the command inv (for "inventory").

; Extra spaces or other characters aren't allowed - instructions must be
; provided precisely.

; Santa's ship is a Reindeer-class starship; these ships use pressure-sensitive
; floors to determine the identity of droids and crew members. The standard
; configuration for these starships is for all droids to weigh exactly the same
; amount to make them easier to detect. If you need to get past such a sensor,
; you might be able to reach the correct weight by carrying items from the
; environment.

; Look around the ship and see if you can find the password for the main
; airlock.

;;; Utils

(defn str-output [program]
  (->> program
       (ic/read-output)
       (map char)
       (apply str)))

(defn run-command [program command]
  (let [input (map #(.codePointAt (str %) 0) (str command "\n"))]
    (-> program
        (ic/consume-output)
        (ic/send-input input)
        (ic/resume))))

(defn inventory [program]
  (->> (run-command program "inv")
       (str-output)
       (re-seq #"(?m)^- (.*)$")
       (map second)))

(defn drop-items [program items]
  (reduce run-command program (map #(str "drop " %) items)))

(defn take-items [program items]
  (reduce run-command program (map #(str "take " %) items)))

(defn find-correct-items-brute-force
  "Tries all possible combinations of items."
  [program test-direction]
  (let [all-items (inventory program)
        ;; drop everything
        program (drop-items program all-items)]
    ;; try every possible combination of items
    (loop [[items & more] (map set (combo/subsets all-items))]
      (when-not items
        (throw (ex-info "Out of items!" {:items all-items})))
      (let [;; pick up these items and try to move
            program' (-> program
                         (take-items items)
                         (run-command test-direction))
            result (->> (str-output program')
                        (re-find #"Alert! Droids on this ship are (lighter|heavier)")
                        (second))]
        (cond
          ;; remove any subsets that are lighter
          (= "heavier" result)
          (recur (remove #(set/superset? items %) more))
          ;; remove any subsets that are heavier
          (= "lighter" result)
          (recur (remove #(set/subset? items %) more))
          ;; otherwise we're done
          :else
          items)))))

;; gray-code based solution
;; https://en.wikipedia.org/wiki/Gray_code
;; This minimizes the number of _commands_ (rather than the number of _tests_),
;; which is important, since running a command is the expensive part.
;; Since items in gray-code differ by only 1 bit, each successive test is just
;; 1 take or drop + 1 movement.

(defn gray-code-seq [n-bits]
  (if (= 1 n-bits)
    [[0] [1]]
    (let [prev (gray-code-seq (dec n-bits))]
      (concat (map #(cons 0 %) prev)
              (map #(cons 1 %) (reverse prev))))))

(defn gray-code-item-seq [items]
  (let [items (vec items)]
    (for [bits (gray-code-seq (count items))]
      (->> bits
           (keep-indexed (fn [idx bit]
                           (when (= 1 bit)
                             (get items idx))))
           (set)))))

(defn take-drop-seq [items]
  (let [items (vec items)]
    (cons ;; start empty
          {:commands (map #(str "drop " %) items)
           :items #{}}
          ;; then try each test
          (for [[prev curr] (partition 2 1 (gray-code-item-seq items))]
            {:commands (concat (map #(str "drop " %) (set/difference prev curr))
                               (map #(str "take " %) (set/difference curr prev)))
             :items curr}))))

(defn find-correct-items-gray-code
  "Tries all combinations of items in an order that results in only dropping or
  taking 1 item at a time."
  [program test-direction]
  (let [all-items (inventory program)]
    (loop [program program
           [{:keys [commands items]} & more] (take-drop-seq all-items)]
      (when-not commands
        (throw (ex-info "Out of items!" {:items all-items})))
      (let [;; switch items and try to move
            program (reduce run-command program (concat commands [test-direction]))
            result (->> (str-output program)
                        (re-find #"Alert! Droids on this ship are (lighter|heavier)")
                        (second))]
        (if result
          (recur program more)
          items)))))


;;; The interactive version

(defonce states
  (atom [{:program (-> (ic/program (ic/parse-code (util/data)))
                       (ic/resume))
          :commands []}]))

(defn output!
  ([] (output! (peek @states)))
  ([state]
   (println (str-output (:program state)))))

(defn undo! []
  (printf "undo `%s`" (peek (:commands (peek @states))))
  (swap! states pop)
  (output!))

(defn restart! []
  (reset! states [(first @states)])
  (output!))

(defn hist
  ([] (hist 1))
  ([n]
   (run! (fn [state]
           (println ">" (peek (:commands state)))
           (output! state))
         (take-last n @states))))

(defn input! [command]
  (swap! states
         (fn [states]
           (let [{:keys [program commands]} (peek states)]
             (conj states
                   {:program (run-command program command)
                    :commands (conj commands command)}))))
  (output!))

#_ (do
     (output!)
     (input! "west")
     (input! "west")
     ,,,)


;;; A programmatic version

;; Explore all the rooms

(def end-room "Pressure-Sensitive Floor")
(def checkpoint-room "Security Checkpoint")

(defn parse-room [program]
  (let [output (str/trim (str-output program))
        parts (filter seq (str/split output #"\n\n"))
        room (second (re-find #"== (.*) ==" output))
        doors (some->> (first (filter #(re-find #"Doors here lead" %) parts))
                       (re-seq #"(?m)^- (.*)$")
                       (mapv second))
        items (some->> (first (filter #(re-find #"Items here" %) parts))
                       (re-seq #"(?m)^- (.*)$")
                       (mapv second))]
    (if (= end-room room)
      ;; special case since this ejects you back to the checkpoint
      {:room room
       :doors nil
       :items nil
       :program program}
      {:room room
       :doors doors
       :items items
       :program program})))

(defn build-graph [program]
  (loop [g {}
         frontier (conj (clojure.lang.PersistentQueue/EMPTY) program)]
    (if-let [program (peek frontier)]
      (let [{:keys [room doors] :as node} (parse-room program)
            neighbors (into {}
                            (for [d doors]
                              [d (parse-room (run-command program d))]))
            node (assoc node :edges
                        (->> neighbors
                             (map (fn [[door {:keys [room]}]] [room door]))
                             (into {})))]
        (recur (assoc g room node)
               (->> (vals neighbors)
                    ;; remove already explored
                    (remove (comp g :room))
                    (map :program)
                    (into (pop frontier)))))
      g)))

;; Path finding

(defn dijkstra
  [graph start target]
  (loop [graph graph
         frontier (conj (clojure.lang.PersistentQueue/EMPTY) [start])]
    (let [path (peek frontier)
          room (peek path)]
      (cond
        ;; Found the target
        (= target room)
        (assoc-in graph [room :path] path)
        ;; Explored the whole graph
        (not room)
        graph
        ;; Next step
        :else
        (let [neighbors (keys (get-in graph [room :edges]))]
          (recur (assoc-in graph [room :path] path)
                 (->> neighbors
                      ;; remove already seen
                      (remove #(get-in graph [% :path]))
                      (map #(conj path %))
                      (into (pop frontier)))))))))

(defn shortest-path
  [graph start target]
  (-> (dijkstra graph start target)
      (get-in [target :path])))

(defn shortest-direction-path [graph start target]
  (when-let [path (shortest-path graph start target)]
    (map (fn [[a b]]
           (get-in graph [a :edges b]))
         (partition 2 1 path))))

;; Solve the puzzle

(defn move-to-room [{:keys [room graph program] :as state} target]
  (assoc state
         :program (reduce run-command program
                          (shortest-direction-path graph room target))
         :room target))

(defn move-to-item [{:keys [graph] :as state} item]
  (let [room-with-item (->> (vals graph)
                            (filter #(contains? (set (:items %)) item))
                            (first)
                            (:room))]
    (move-to-room state room-with-item)))

(defn find-and-take-item [state item]
  (let [;; try to take the item
        state' (-> state
                   (move-to-item item)
                   (update :program run-command (str "take " item)))]
    (if (ic/end? (:program state'))
      ;; if taking the item caused us to die, return the old state
      state
      ;; if everything is ok, return the new state
      state')))

(defn part1 [f]
  (let [;; 1. explore the whole ship
        program (-> (ic/program (ic/parse-code f))
                    (ic/resume))
        state {:program program
               :room (:room (parse-room program))
               :graph (build-graph program)}
        ;; 2. pick up all the items
        all-items (-> (mapcat :items (vals (:graph state)))
                      (set)
                      ;; this actually sends the program into an infinite loop
                      (disj "infinite loop")
                      ;; this causes us to stop moving
                      (disj "giant electromagnet"))
        state (reduce find-and-take-item state all-items)
        ;; 3. move to the end
        state (move-to-room state checkpoint-room)
        ;; 4. find the combination of items that will allow us through the door
        advance-direction (get-in state [:graph checkpoint-room :edges end-room])
        required-items (find-correct-items-gray-code (:program state) advance-direction)
        ;; ... and move through the door
        state (update state :program
                      #(-> %
                           (drop-items all-items)
                           (take-items required-items)
                           (run-command advance-direction)))]
    ;; 5. read the password
    (re-find #"\d+" (str-output (:program state)))))


;;; Part 2

; As you move through the main airlock, the air inside the ship is already
; heating up to reasonable levels. Santa explains that he didn't notice you
; coming because he was just taking a quick nap. The ship wasn't frozen; he
; just had the thermostat set to "North Pole".

; You make your way over to the navigation console. It beeps. "Status:
; Stranded. Please supply measurements from 49 stars to recalibrate."

; "49 stars? But the Elves told me you needed fifty--"

; Santa just smiles and nods his head toward the window. There, in the
; distance, you can see the center of the Solar System: the Sun!

; The navigation console beeps again.

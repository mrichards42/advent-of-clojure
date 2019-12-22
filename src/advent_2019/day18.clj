(ns advent-2019.day18
  "Many-Worlds Interpretation"
  (:require [advent.grid :as grid]
            [advent.util :as util]
            [clojure.string :as str]))

;;; Part 1

; As you approach Neptune, a planetary security system detects you and
; activates a giant tractor beam on Triton! You have no choice but to land.

; A scan of the local area reveals only one interesting feature: a massive
; underground vault. You generate a map of the tunnels (your puzzle input). The
; tunnels are too narrow to move diagonally.

; Only one entrance (marked @) is present among the open passages (marked .)
; and stone walls (#), but you also detect an assortment of keys (shown as
; lowercase letters) and doors (shown as uppercase letters). Keys of a given
; letter open the door of the same letter: a opens A, b opens B, and so on. You
; aren't sure which key you need to disable the tractor beam, so you'll need to
; collect all of them.

; For example, suppose you have the following map:

; #########
; #b.A.@.a#
; #########

; Starting from the entrance (@), you can only access a large door (A) and a
; key (a). Moving toward the door doesn't help you, but you can move 2 steps to
; collect the key, unlocking A in the process:

; #########
; #b.....@#
; #########

; Then, you can move 6 steps to collect the only other key, b:

; #########
; #@......#
; #########

; So, collecting every key took a total of 8 steps.

; Here is a larger example:

; ########################
; #f.D.E.e.C.b.A.@.a.B.c.#
; ######################.#
; #d.....................#
; ########################

; The only reasonable move is to take key a and unlock door A:

; ########################
; #f.D.E.e.C.b.....@.B.c.#
; ######################.#
; #d.....................#
; ########################

; Then, do the same with key b:

; ########################
; #f.D.E.e.C.@.........c.#
; ######################.#
; #d.....................#
; ########################

; ...and the same with key c:

; ########################
; #f.D.E.e.............@.#
; ######################.#
; #d.....................#
; ########################

; Now, you have a choice between keys d and e. While key e is closer, collecting it now would be slower in the long run than collecting key d first, so that's the best choice:

; ########################
; #f...E.e...............#
; ######################.#
; #@.....................#
; ########################

; Finally, collect key e to unlock door E, then collect key f, taking a grand
; total of 86 steps.

; Here are a few more examples:

;     ########################
;     #...............b.C.D.f#
;     #.######################
;     #.....@.a.B.c.d.A.e.F.g#
;     ########################

;     Shortest path is 132 steps: b, a, c, d, f, e, g

;     #################
;     #i.G..c...e..H.p#
;     ########.########
;     #j.A..b...f..D.o#
;     ########@########
;     #k.E..a...g..B.n#
;     ########.########
;     #l.F..d...h..C.m#
;     #################

;     Shortest paths are 136 steps;
;     one is: a, f, b, j, g, n, h, d, l, o, e, p, c, i, k, m

;     ########################
;     #@..............ac.GI.b#
;     ###d#e#f################
;     ###A#B#C################
;     ###g#h#i################
;     ########################

;     Shortest paths are 81 steps; one is: a, c, f, i, d, g, b, e, h

; How many steps is the shortest path that collects all of the keys?

(defn parse-tile [pos t]
  (cond
    (= \. t) {:type :empty :pos pos}
    (= \# t) {:type :wall :pos pos}
    (= \@ t) {:type :entrance :pos pos}
    (Character/isUpperCase t) {:type :door :id t :pos pos}
    (Character/isLowerCase t) {:type :key :id (Character/toUpperCase t) :pos pos}))

(defn parse-tunnel [f]
  (let [nodes (->> (grid/parse f parse-tile)
                   (map (juxt :pos identity))
                   (into {}))]
    {:nodes nodes
     :edges {}
     :robots (->> (vals nodes)
                  (filter #(= :entrance (:type %)))
                  (map-indexed #(assoc %2
                                       :type :robot
                                       :idx %1
                                       :steps 0
                                       :visited []))
                  (vec))
     :doors (->> (vals nodes)
                 (filter #(= :door (:type %)))
                 (map (juxt :id :pos))
                 (into {}))}))

(defn draw-tunnel [g]
  (let [colors [41 42 43 44]]
    (grid/draw-map (:nodes g)
                   (fn [node]
                     (if-let [robot (first (filter #(= (:pos node) (:pos %)) (:robots g)))]
                       (format "[%sm%s[0m" (get colors (:idx robot)) "@")
                       (case (:type node)
                         :empty " "
                         :wall #_"#" "█"
                         :entrance " "
                         :key (Character/toLowerCase (:id node))
                         :door (:id node)))))))

;; edge discovery

(defn neighbors
  "Returns nodes for all neighbors of the input `pos`."
  [graph [x y]]
  (->> #{[x (dec y)]
         [x (inc y)]
         [(dec x) y]
         [(inc x) y]}
       (map (:nodes graph))
       (remove #(= :wall (:type %)))))

(defn dijkstra-infinite
  "Dijkstra's algorithm on a graph of unknown size.

  Returns an explored graph with distances."
  [graph start-pos]
  (loop [graph graph
         frontier (conj (clojure.lang.PersistentQueue/EMPTY) [start-pos 0])]
    (let [[pos dist] (peek frontier)]
      (if-not pos
        ;; Explored the whole graph
        graph
        ;; Next step
        (let [;; only explore neighbors if we can move through this tile
              neighbors (when (or (contains? #{:empty :entrance}
                                             (get-in graph [:nodes pos :type]))
                                  (= start-pos pos))
                          (neighbors graph pos))]
          (recur (assoc-in graph [:nodes pos :distance] dist)
                 (->> neighbors
                      (remove :distance) ;; remove already seen
                      (map #(vector (:pos %) (inc dist)))
                      (into (pop frontier)))))))))

(defn discover-edges [g node]
  (let [g (dijkstra-infinite g (:pos node))]
    (->> (vals (:nodes g))
         ;; reachable nodes ...
         (filter #(pos? (:distance % -1)))
         ;; that are keys, doors, or entrances
         (filter (comp #{:key :door :entrance} :type))
         (map (juxt :pos :distance))
         (into {}))))

(defn build-edges [g]
  ;; This consolidates the very expansive graph (where each square is a node)
  ;; into a much smaller graph (where each key/door/entrance is a node)
  (let [nodes (filter (comp #{:key :door :entrance} :type) (vals (:nodes g)))]
    (reduce (fn [g n]
              (assoc-in g [:edges (:pos n)] (discover-edges g n)))
            g
            nodes)))

;; edge pruning

(defn join-edges [g predecessor-pos pos-to-remove]
  (let [old-weight (get-in g [:edges pos-to-remove predecessor-pos])
        old-edges (-> (get-in g [:edges pos-to-remove])
                      ;; don't include an edge to the new predecessor
                      (dissoc predecessor-pos))]
    (update-in g [:edges predecessor-pos]
               #(reduce (fn [edges [to edge-weight]]
                          ;; use the new edge only if the predecessor didn't
                          ;; already have a lower weight edge to the `to` node
                          (update edges to
                                  (fnil min Integer/MAX_VALUE)
                                  (+ old-weight edge-weight)))
                        %
                        old-edges))))

(defn join-all-edges [g pos-to-remove]
  (let [;; this is really an undirected graph, so all successors are also
        ;; predecessors
        predecessors (keys (get-in g [:edges pos-to-remove]))]
    (reduce (fn [g predecessor]
              (join-edges g predecessor pos-to-remove))
            g
            predecessors)))

(defn remove-node-edges [g pos]
  (-> g
      ;; update nodes with edges _to_ this node so that they also have edges
      ;; to all successors of this node (with adjusted weights).
      (join-all-edges pos)
      ;; remove any edges _from_ this node
      (update :edges dissoc pos)
      ;; remove any edges _to_ this node
      (update :edges (fn [edges] (util/map-vals #(dissoc % pos) edges)))))

;; path finding

(defn reachable-keys [g]
  (apply
   concat
   (for [{:keys [pos]} (:robots g)]
     (->> (get-in g [:edges pos])
          (keys)
          (filter #(= :key (get-in g [:nodes % :type])))))))

(defn move-to-key [g key-pos]
  (let [key-id (get-in g [:nodes key-pos :id])
        door-pos (get-in g [:doors key-id])
        ;; find the robot that can reach this key
        robot (first (filter #(get-in g [:edges (:pos %) key-pos]) (:robots g)))
        weight (get-in g [:edges (:pos robot) key-pos])]
    (cond-> g
      ;; move the robot
      robot
      (update-in [:robots (:idx robot)]
                 #(-> %
                      (assoc :pos key-pos)
                      (update :steps + weight)
                      (update :visited conj key-id)))
      robot
      (update-in [:nodes (:pos robot)] assoc :type :empty)
      ;; remove the edges for the robot's current location
      robot
      (remove-node-edges (:pos robot))
      ;; unlock the door
      door-pos
      (update-in [:nodes door-pos] assoc :type :empty)
      door-pos
      (remove-node-edges door-pos))))

(defn explore-step [g]
  (let [reachable (reachable-keys g)]
    (if (seq reachable)
      (map #(move-to-key g %) reachable)
      [g])))

(defn select-best-graphs [graphs]
  ;; graphs are functionally identical if the robots have
  ;; visited the same keys and are at the same position
  ;; this selects only graphs that are unique by that key, and for graphs which
  ;; have the same keys, selects the one with the fewest steps taken
  (->> graphs
       (map (fn [g]
              {:key [(mapv :pos (:robots g))
                     (set (mapcat :visited (:robots g)))]
               :steps (reduce + (map #(:steps % 0) (:robots g)))
               :graph g}))
       (group-by :key)
       (vals)
       (map #(apply min-key :steps %))
       (map :graph)))

(defn explore-all-paths* [graphs]
  (let [graphs' (mapcat explore-step (select-best-graphs graphs))]
    (if (empty? (reachable-keys (first graphs')))
      graphs'
      (recur graphs'))))

(defn explore-all-paths
  "Visits every key, branching when multiple keys are available.

  Returns a seq of all possible graphs."
  [g]
  (explore-all-paths* [g]))


#_ (do
     (defn move-demo [g n]
       (if (or (zero? n)
               (empty? (reachable-keys g)))
         g
         (do
           (Thread/sleep 200)
           #_(println "[2J")
           (println (draw-tunnel g))
           #_(clojure.pprint/pprint (:robots g))
           #_(clojure.pprint/pprint (reachable-keys g))
           (recur (move-to-key g (last (reachable-keys g)))
                  (dec n)))))

     (def demo-tunnel
       (-> "########################
           #f.D.E.e.C.b.A.@.a.B.c.#
           ######################.#
           #d.....................#
           ########################"
           (clojure.string/replace #" " "")
           (.getBytes)
           (parse-tunnel)
           (build-edges)))


     #_ (->> demo-tunnel
             (explore-all-paths)
             (doall)
             (map #(reduce + (map :steps (:robots %))))
             (apply min)
             (time))

     #_ (-> demo-tunnel
            (move-demo 6)
            (some?))

     #_ (-> (parse-tunnel (util/data))
            (build-edges)
            (move-demo 6)
            (some?))
     )

(defn part1 [f]
  (->> (parse-tunnel f)
       (build-edges)
       (explore-all-paths)
       (map #(reduce + (map :steps (:robots %))))
       (apply min)))

#_ (time (util/run part1))

;;; Part 2

; You arrive at the vault only to discover that there is not one vault, but
; four - each with its own entrance.

; On your map, find the area in the middle that looks like this:

; ...
; .@.
; ...

; Update your map to instead use the correct data:

; @#@
; ###
; @#@

; This change will split your map into four separate sections, each with its
; own entrance:

; #######       #######
; #a.#Cd#       #a.#Cd#
; ##...##       ##@#@##
; ##.@.##  -->  #######
; ##...##       ##@#@##
; #cB#Ab#       #cB#Ab#
; #######       #######

; Because some of the keys are for doors in other vaults, it would take much
; too long to collect all of the keys by yourself. Instead, you deploy four
; remote-controlled robots. Each starts at one of the entrances (@).

; Your goal is still to collect all of the keys in the fewest steps, but now,
; each robot has its own position and can move independently. You can only
; remotely control a single robot at a time. Collecting a key instantly unlocks
; any corresponding doors, regardless of the vault in which the key or door is
; found.

; For example, in the map above, the top-left robot first collects key a,
; unlocking door A in the bottom-right vault:

; #######
; #@.#Cd#
; ##.#@##
; #######
; ##@#@##
; #cB#.b#
; #######

; Then, the bottom-right robot collects key b, unlocking door B in the
; bottom-left vault:

; #######
; #@.#Cd#
; ##.#@##
; #######
; ##@#.##
; #c.#.@#
; #######

; Then, the bottom-left robot collects key c:

; #######
; #@.#.d#
; ##.#@##
; #######
; ##.#.##
; #@.#.@#
; #######

; Finally, the top-right robot collects key d:

; #######
; #@.#.@#
; ##.#.##
; #######
; ##.#.##
; #@.#.@#
; #######

; In this example, it only took 8 steps to collect all of the keys.

; Sometimes, multiple robots might have keys available, or a robot might have
; to wait for multiple keys to be collected:

; ###############
; #d.ABC.#.....a#
; ######@#@######
; ###############
; ######@#@######
; #b.....#.....c#
; ###############

; First, the top-right, bottom-left, and bottom-right robots take turns
; collecting keys a, b, and c, a total of 6 + 6 + 6 = 18 steps. Then, the
; top-left robot can access key d, spending another 6 steps; collecting all of
; the keys here takes a minimum of 24 steps.

; Here's a more complex example:

; #############
; #DcBa.#.GhKl#
; #.###@#@#I###
; #e#d#####j#k#
; ###C#@#@###J#
; #fEbA.#.FgHi#
; #############

;   - Top-left robot collects key a.
;   - Bottom-left robot collects key b.
;   - Top-left robot collects key c.
;   - Bottom-left robot collects key d.
;   - Top-left robot collects key e.
;   - Bottom-left robot collects key f.
;   - Bottom-right robot collects key g.
;   - Top-right robot collects key h.
;   - Bottom-right robot collects key i.
;   - Top-right robot collects key j.
;   - Bottom-right robot collects key k.
;   - Top-right robot collects key l.

; In the above example, the fewest steps to collect all of the keys is 32.

; Here's an example with more choices:

; #############
; #g#f.D#..h#l#
; #F###e#E###.#
; #dCba@#@BcIJ#
; #############
; #nK.L@#@G...#
; #M###N#H###.#
; #o#m..#i#jk.#
; #############

; One solution with the fewest steps is:

;   - Top-left robot collects key e.
;   - Top-right robot collects key h.
;   - Bottom-right robot collects key i.
;   - Top-left robot collects key a.
;   - Top-left robot collects key b.
;   - Top-right robot collects key c.
;   - Top-left robot collects key d.
;   - Top-left robot collects key f.
;   - Top-left robot collects key g.
;   - Bottom-right robot collects key k.
;   - Bottom-right robot collects key j.
;   - Top-right robot collects key l.
;   - Bottom-left robot collects key n.
;   - Bottom-left robot collects key m.
;   - Bottom-left robot collects key o.

; This example requires at least 72 steps to collect all keys.

; After updating your map and using the remote-controlled robots, what is the
; fewest steps necessary to collect all of the keys?

(defn parse-tunnel-2 [f]
  (let [;; Change the middle 3x3 square
        char-grid (vec (util/lines f))
        cy (/ (dec (count char-grid)) 2)
        cx (/ (dec (count (first char-grid))) 2)]
    (-> char-grid
        (update (dec cy) #(str (subs % 0 (dec cx)) "@#@" (subs % (+ 2 cx))))
        (update cy       #(str (subs % 0 (dec cx)) "###" (subs % (+ 2 cx))))
        (update (inc cy) #(str (subs % 0 (dec cx)) "@#@" (subs % (+ 2 cx))))
        (->> (str/join "\n"))
        (.getBytes)
        ;; then parse as normal
        (parse-tunnel))))

#_ (do

     (def demo-tunnel-2
       (-> "#############
           #DcBa.#.GhKl#
           #.###...#I###
           #e#d#.@.#j#k#
           ###C#...###J#
           #fEbA.#.FgHi#
           #############"
           (clojure.string/replace #" " "")
           (.getBytes)
           (parse-tunnel-2)
           (build-edges)))

     #_ (-> demo-tunnel-2
            (move-demo 20)
            (some?)
            )

     #_ (-> (parse-tunnel-2 (util/data))
            (build-edges)
            (move-demo 20)
            (some?))
     )

(defn part2 [f]
  (->> (parse-tunnel-2 f)
       (build-edges)
       (explore-all-paths)
       (map #(reduce + (map :steps (:robots %))))
       (apply min)))

#_ (time (util/run part2))

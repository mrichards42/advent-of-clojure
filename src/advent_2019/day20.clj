(ns advent-2019.day20
  "Donut Maze"
  (:require [advent.grid :as grid]
            [advent.util :as util]))

;;; Part 1

; You notice a strange pattern on the surface of Pluto and land nearby to get a
; closer look. Upon closer inspection, you realize you've come across one of
; the famous space-warping mazes of the long-lost Pluto civilization!

; Because there isn't much space on Pluto, the civilization that used to live
; here thrived by inventing a method for folding spacetime. Although the
; technology is no longer understood, mazes like this one provide a small
; glimpse into the daily life of an ancient Pluto citizen.

; This maze is shaped like a donut. Portals along the inner and outer edge of
; the donut can instantly teleport you from one side to the other. For example:

;          A           
;          A           
;   #######.#########  
;   #######.........#  
;   #######.#######.#  
;   #######.#######.#  
;   #######.#######.#  
;   #####  B    ###.#  
; BC...##  C    ###.#  
;   ##.##       ###.#  
;   ##...DE  F  ###.#  
;   #####    G  ###.#  
;   #########.#####.#  
; DE..#######...###.#  
;   #.#########.###.#  
; FG..#########.....#  
;   ###########.#####  
;              Z       
;              Z       

; This map of the maze shows solid walls (#) and open passages (.). Every maze
; on Pluto has a start (the open tile next to AA) and an end (the open tile
; next to ZZ). Mazes on Pluto also have portals; this maze has three pairs of
; portals: BC, DE, and FG. When on an open tile next to one of these labels, a
; single step can take you to the other tile with the same label. (You can only
; walk on . tiles; labels and empty space are not traversable.)

; One path through the maze doesn't require any portals. Starting at AA, you
; could go down 1, right 8, down 12, left 4, and down 1 to reach ZZ, a total of
; 26 steps.

; However, there is a shorter path: You could walk from AA to the inner BC
; portal (4 steps), warp to the outer BC portal (1 step), walk to the inner DE
; (6 steps), warp to the outer DE (1 step), walk to the outer FG (4 steps),
; warp to the inner FG (1 step), and finally walk to ZZ (6 steps). In total,
; this is only 23 steps.

; Here is a larger example:

;                    A               
;                    A               
;   #################.#############  
;   #.#...#...................#.#.#  
;   #.#.#.###.###.###.#########.#.#  
;   #.#.#.......#...#.....#.#.#...#  
;   #.#########.###.#####.#.#.###.#  
;   #.............#.#.....#.......#  
;   ###.###########.###.#####.#.#.#  
;   #.....#        A   C    #.#.#.#  
;   #######        S   P    #####.#  
;   #.#...#                 #......VT
;   #.#.#.#                 #.#####  
;   #...#.#               YN....#.#  
;   #.###.#                 #####.#  
; DI....#.#                 #.....#  
;   #####.#                 #.###.#  
; ZZ......#               QG....#..AS
;   ###.###                 #######  
; JO..#.#.#                 #.....#  
;   #.#.#.#                 ###.#.#  
;   #...#..DI             BU....#..LF
;   #####.#                 #.#####  
; YN......#               VT..#....QG
;   #.###.#                 #.###.#  
;   #.#...#                 #.....#  
;   ###.###    J L     J    #.#.###  
;   #.....#    O F     P    #.#...#  
;   #.###.#####.#.#####.#####.###.#  
;   #...#.#.#...#.....#.....#.#...#  
;   #.#####.###.###.#.#.#########.#  
;   #...#.#.....#...#.#.#.#.....#.#  
;   #.###.#####.###.###.#.#.#######  
;   #.#.........#...#.............#  
;   #########.###.###.#############  
;            B   J   C               
;            U   P   P               

; Here, AA has no direct path to ZZ, but it does connect to AS and CP. By
; passing through AS, QG, BU, and JO, you can reach ZZ in 58 steps.

; In your maze, how many steps does it take to get from the open tile marked AA
; to the open tile marked ZZ?


;; maze parsing

(defn parse-maze-grid [f]
  (letfn [(parse-tile [xy c]
            (case (str c)
              " " nil
              "." {:pos xy :type :empty}
              "#" {:pos xy :type :wall}
              ;; else it's a letter
              {:pos xy :type :letter :id c}))]
    (->> f
         (grid/parse parse-tile)
         (map (juxt :pos identity))
         (into {}))))

(defn label-square [g {:keys [pos id]}]
  (first
   (for [[dir dx dy] [[:R 1 0] [:L -1 0] [:D 0 1] [:U 0 -1]]
         :let [next-square (get g (mapv + pos [dx dy]))
               prev-square (get g (mapv - pos [dx dy]))]
         :when (= :empty (:type next-square))]
     (let [label (case dir
                   (:L :U) (str id (:id prev-square))
                   (:R :D) (str (:id prev-square) id))]
       (assoc next-square :label label)))))

(defn label-portals [g letter-square]
  (if-let [square (label-square g letter-square)]
    (assoc g (:pos square) square)
    g))

(defn parse-maze [f]
  (let [;; step 1: read the file into a grid
        g (parse-maze-grid f)
        ;; step 2: label the portals
        letters (filter #(= :letter (:type %)) (vals g))
        g (reduce label-portals g letters)]
    ;; step 3: remove the letter squares
    (->> (vals g)
         (filter #(= :letter (:type %)))
         (map :pos)
         (apply dissoc g))))

(defn draw-maze [m]
  (grid/draw-map m
                 #(case (:type %)
                    :empty (if (:label %)
                             (first (:label %))
                             #_(str "[" (:label %) "]")
                             ".")
                    :wall "█"
                    :letter " "
                    nil " ")))

;; path finding

(defn dijkstra
  [graph neighbors start-pos target?]
  (loop [graph graph
         frontier (doto (java.util.PriorityQueue.)
                    (.add [0 start-pos]))]
    (let [[dist pos] (.poll frontier)]
      (cond
        ;; Found the target
        (target? pos)
        (update-in graph [pos :distance] (fnil min Integer/MAX_VALUE) dist)
        ;; Explored the whole graph
        (not pos)
        graph
        ;; Next step
        :else
        (let [neighbors (neighbors graph pos)]
          (recur (update-in graph [pos :distance] (fnil min Integer/MAX_VALUE) dist)
                 (->> neighbors
                      (filter identity)
                      (map #(vector (+ dist (:weight (meta %) 1)) %))
                      ;; remove nodes that are further than already known paths
                      (remove #(<= (get-in graph [(second %) :distance] Integer/MAX_VALUE)
                                   (first %)))
                      (reduce (fn [q neighbor]
                                (.add q neighbor)
                                q)
                              frontier))))))))

(defn portal-neighbor [graph pos]
  (let [node (get graph pos)]
    (when-let [label (:label node)]
      (->> (vals graph)
           (filter #(= label (:label %)))
           (remove #{node})
           (first)
           (:pos)))))

(defn cardinal-neighbors [graph [x y]]
  (->> #{[x (dec y)]
         [x (inc y)]
         [(dec x) y]
         [(inc x) y]}
       (map graph)
       (filter #(= :empty (:type %)))
       (map :pos)))

(defn neighbors-1
  [graph pos]
  (cons (portal-neighbor graph pos)
        (cardinal-neighbors graph pos)))

(defn path-through-maze-1 [g]
  (let [start (:pos (first (filter #(= "AA" (:label %)) (vals g))))
        end (:pos (first (filter #(= "ZZ" (:label %)) (vals g))))
        g (dijkstra g neighbors-1 start #{end})]
    (get-in g [end :distance])))

(defn part1 [f]
  (->> (parse-maze f)
       (path-through-maze-1)))

#_ (time (util/run part1))

;;; Part 2

; Strangely, the exit isn't open when you reach it. Then, you remember: the
; ancient Plutonians were famous for building recursive spaces.

; The marked connections in the maze aren't portals: they physically connect to
; a larger or smaller copy of the maze. Specifically, the labeled tiles around
; the inside edge actually connect to a smaller copy of the same maze, and the
; smaller copy's inner labeled tiles connect to yet a smaller copy, and so on.

; When you enter the maze, you are at the outermost level; when at the
; outermost level, only the outer labels AA and ZZ function (as the start and
; end, respectively); all other outer labeled tiles are effectively walls. At
; any other level, AA and ZZ count as walls, but the other outer labeled tiles
; bring you one level outward.

; Your goal is to find a path through the maze that brings you back to ZZ at
; the outermost level of the maze.

; In the first example above, the shortest path is now the loop around the
; right side. If the starting level is 0, then taking the previously-shortest
; path would pass through BC (to level 1), DE (to level 2), and FG (back to
; level 1). Because this is not the outermost level, ZZ is a wall, and the only
; option is to go back around to BC, which would only send you even deeper into
; the recursive maze.

; In the second example above, there is no path that brings you to ZZ at the
; outermost level.

; Here is a more interesting example:

;              Z L X W       C                 
;              Z P Q B       K                 
;   ###########.#.#.#.#######.###############  
;   #...#.......#.#.......#.#.......#.#.#...#  
;   ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  
;   #.#...#.#.#...#.#.#...#...#...#.#.......#  
;   #.###.#######.###.###.#.###.###.#.#######  
;   #...#.......#.#...#...#.............#...#  
;   #.#########.#######.#.#######.#######.###  
;   #...#.#    F       R I       Z    #.#.#.#  
;   #.###.#    D       E C       H    #.#.#.#  
;   #.#...#                           #...#.#  
;   #.###.#                           #.###.#  
;   #.#....OA                       WB..#.#..ZH
;   #.###.#                           #.#.#.#  
; CJ......#                           #.....#  
;   #######                           #######  
;   #.#....CK                         #......IC
;   #.###.#                           #.###.#  
;   #.....#                           #...#.#  
;   ###.###                           #.#.#.#  
; XF....#.#                         RF..#.#.#  
;   #####.#                           #######  
;   #......CJ                       NM..#...#  
;   ###.#.#                           #.###.#  
; RE....#.#                           #......RF
;   ###.###        X   X       L      #.#.#.#  
;   #.....#        F   Q       P      #.#.#.#  
;   ###.###########.###.#######.#########.###  
;   #.....#...#.....#.......#...#.....#.#...#  
;   #####.#.###.#######.#######.###.###.#.#.#  
;   #.......#.......#.#.#.#.#...#...#...#.#.#  
;   #####.###.#####.#.#.#.#.###.###.#.###.###  
;   #.......#.....#.#...#...............#...#  
;   #############.#.#.###.###################  
;                A O F   N                     
;                A A D   M                     
; 

; One shortest path through the maze is the following:

;   - Walk from AA to XF (16 steps)
;   - Recurse into level 1 through XF (1 step)
;   - Walk from XF to CK (10 steps)
;   - Recurse into level 2 through CK (1 step)
;   - Walk from CK to ZH (14 steps)
;   - Recurse into level 3 through ZH (1 step)
;   - Walk from ZH to WB (10 steps)
;   - Recurse into level 4 through WB (1 step)
;   - Walk from WB to IC (10 steps)
;   - Recurse into level 5 through IC (1 step)
;   - Walk from IC to RF (10 steps)
;   - Recurse into level 6 through RF (1 step)
;   - Walk from RF to NM (8 steps)
;   - Recurse into level 7 through NM (1 step)
;   - Walk from NM to LP (12 steps)
;   - Recurse into level 8 through LP (1 step)
;   - Walk from LP to FD (24 steps)
;   - Recurse into level 9 through FD (1 step)
;   - Walk from FD to XQ (8 steps)
;   - Recurse into level 10 through XQ (1 step)
;   - Walk from XQ to WB (4 steps)
;   - Return to level 9 through WB (1 step)
;   - Walk from WB to ZH (10 steps)
;   - Return to level 8 through ZH (1 step)
;   - Walk from ZH to CK (14 steps)
;   - Return to level 7 through CK (1 step)
;   - Walk from CK to XF (10 steps)
;   - Return to level 6 through XF (1 step)
;   - Walk from XF to OA (14 steps)
;   - Return to level 5 through OA (1 step)
;   - Walk from OA to CJ (8 steps)
;   - Return to level 4 through CJ (1 step)
;   - Walk from CJ to RE (8 steps)
;   - Return to level 3 through RE (1 step)
;   - Walk from RE to IC (4 steps)
;   - Recurse into level 4 through IC (1 step)
;   - Walk from IC to RF (10 steps)
;   - Recurse into level 5 through RF (1 step)
;   - Walk from RF to NM (8 steps)
;   - Recurse into level 6 through NM (1 step)
;   - Walk from NM to LP (12 steps)
;   - Recurse into level 7 through LP (1 step)
;   - Walk from LP to FD (24 steps)
;   - Recurse into level 8 through FD (1 step)
;   - Walk from FD to XQ (8 steps)
;   - Recurse into level 9 through XQ (1 step)
;   - Walk from XQ to WB (4 steps)
;   - Return to level 8 through WB (1 step)
;   - Walk from WB to ZH (10 steps)
;   - Return to level 7 through ZH (1 step)
;   - Walk from ZH to CK (14 steps)
;   - Return to level 6 through CK (1 step)
;   - Walk from CK to XF (10 steps)
;   - Return to level 5 through XF (1 step)
;   - Walk from XF to OA (14 steps)
;   - Return to level 4 through OA (1 step)
;   - Walk from OA to CJ (8 steps)
;   - Return to level 3 through CJ (1 step)
;   - Walk from CJ to RE (8 steps)
;   - Return to level 2 through RE (1 step)
;   - Walk from RE to XQ (14 steps)
;   - Return to level 1 through XQ (1 step)
;   - Walk from XQ to FD (8 steps)
;   - Return to level 0 through FD (1 step)
;   - Walk from FD to ZZ (18 steps)

; This path takes a total of 396 steps to move from AA at the outermost layer
; to ZZ at the outermost layer.

; In your maze, when accounting for recursion, how many steps does it take to
; get from the open tile marked AA to the open tile marked ZZ, both at the
; outermost layer?

;; parse and compress the graph

(defn find-edges [g pos]
  (let [g' (dijkstra g cardinal-neighbors pos (constantly false))
        reachable (->> (vals g')
                       (filter #(and (:distance %) (:label %)))
                       (remove #(zero? (:distance %))))
        portal (portal-neighbor g pos)]
    (cond-> g
      ;; portals on the same level
      true (assoc-in [pos :edges]
                     (->> reachable
                          (map (juxt :pos :distance))
                          (into {})))
      ;; the other side of this portal
      portal (assoc-in [pos :edges portal] 1))))

(defn compress-maze [g]
  (as-> g $
    (reduce find-edges $ (map :pos (filter :label (vals $))))
    (filter (comp :edges val) $)
    (into {} $)))

(defn mark-portal-orientation [g]
  (let [{:keys [x-min x-max y-min y-max]} (grid/bounds (map :pos (vals g)))]
    (reduce (fn [g {:keys [pos]}]
              (let [[x y] pos]
                (if (or (= x x-min)
                        (= x x-max)
                        (= y y-min)
                        (= y y-max))
                  (assoc-in g [pos :outer?] true)
                  (assoc-in g [pos :inner?] true))))
            g
            (filter :label (vals g)))))

(defn parse-maze-2 [f]
  (-> (parse-maze f)
      (mark-portal-orientation)
      (compress-maze)))

;; path finding

(defn neighbors-2
  [graph [x y z]]
  (let [{:keys [edges inner?]} (get graph [x y])]
    (keep (fn [[pos weight]]
            ;; metadata is a little hacky, but it's simpler to shoehorn it into
            ;; the part 1 implementation than figure out the right abstraction
            (cond
              ;; normal edge
              (< 1 weight)
              (with-meta (conj pos z) {:weight weight})
              ;; inner portal
              inner?
              (with-meta (conj pos (inc z)) {:weight weight})
              ;; outer portal; only allowed when we're down at least 1 level
              (pos? z)
              (with-meta (conj pos (dec z)) {:weight weight})))
          edges)))

(defn path-through-maze-2 [g]
  (let [[x1 y1] (:pos (first (filter #(= "AA" (:label %)) (vals g))))
        [x2 y2] (:pos (first (filter #(= "ZZ" (:label %)) (vals g))))
        g (dijkstra g neighbors-2 [x1 y1 0] #{[x2 y2 0]})]
    (get-in g [[x2 y2 0] :distance])))

(defn part2 [f]
  (->> (parse-maze-2 f)
       (path-through-maze-2)))

#_ (time (util/run part2))

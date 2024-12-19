(ns advent.algo)

(defn reconstruct-path
  "Reconstructs a path given the output cost map of `dijkstra` and the start and
  end nodes"
  [cost-map start end]
  (let [{::keys [predecessors]} cost-map]
    (loop [node end
           path []]
      (if (= start node)
        path
        (when-let [predecessor (predecessors node)]
          (recur predecessor (cons predecessor path)))))))

(defn- update-costs [cost-map node cost predecessor]
  (-> cost-map
      (assoc node cost)
      (update ::predecessors assoc node predecessor)))

(defn dijkstra
  "Dijkstra's algorithm. Returns a map of

    {node cost
     ::predecessors {node predecessor}}

  Arguments:

    `neighbors` - function: node -> seq of [weight node]
    `inits`     - seq of node
    `target?`   - function: node -> boolean
  "
  [neighbors inits target?]
  (loop [cost-map {::predecessors {}}
         frontier (doto (java.util.PriorityQueue.)
                    (.addAll (map #(vector 0 % nil) inits)))]
    (let [[cost node predecessor] (.poll frontier)]
      (cond
        ;; Found the target
        (target? node)
        (update-costs cost-map node cost predecessor)
        ;; Explored the whole graph
        (not node)
        cost-map
        ;; Already seen this node
        (<= (cost-map node Integer/MAX_VALUE) cost)
        (recur cost-map frontier)
        ;; Next step
        :else
        (do
          (doseq [[next-weight next-node] (neighbors node)
                  :when next-node
                  :let [next-cost (+ cost next-weight)]
                  :when (< next-cost (cost-map next-node Integer/MAX_VALUE))]
            (.add frontier [next-cost next-node node]))
          (recur (update-costs cost-map node cost predecessor)
                 frontier))))))

(defn dijkstra-path
  "Returns the path from a single `start` to a single `end` node using
  `dijkstra`."
  [neighbors start end]
  (reconstruct-path (dijkstra neighbors #{start} #{end}) start end))

(defn binary-search
  "Runs a binary search of a function `f` taking an integer value, attempting
  to hit `target` between `lower-bound` and `upper-bound`.

  Returns the value that produced `target` if successful, or nil if
  unsuccessful."
  [f target lower-bound upper-bound]
  (if (< upper-bound lower-bound)
    nil
    (let [mid (quot (+ lower-bound upper-bound) 2)
          result (compare target (f mid))]
      (cond
        (zero? result) mid
        (neg? result) (recur f target lower-bound (dec mid))
        :else (recur f target (inc mid) upper-bound)))))

(ns advent-2024.day05
  "Print Queue"
  (:require [advent.util :as util]
            [clojure.string :as str]))

(def example
  (util/example-input
   "47|53
   97|13
   97|61
   97|47
   75|29
   61|13
   75|53
   29|13
   97|29
   53|29
   61|53
   97|53
   61|29
   47|13
   75|47
   97|75
   47|61
   75|61
   47|29
   75|13
   53|13

   75,47,61,53,29
   97,61,53,29,13
   75,29,13
   75,97,47,61,53
   61,13,29
   97,13,75,29,47
   "))

;;; Part 1
;; Find pages that are already in order

(defn split-longs [txt]
  (mapv parse-long (re-seq #"\d+" txt)))

(defn parse-input [input]
  (let [[rules pages] (str/split input #"\n\n")]
    {:rules (map split-longs (str/split-lines rules))
     :pages (map split-longs (str/split-lines pages))}))

(defn mid [v]
  (nth v (/ (dec (count v)) 2)))

(defn rule-graph [rules]
  (let [nodes (reduce into #{} rules)
        graph (into {} (map vector nodes (repeat {})))]
    (reduce (fn [g [from to]] (assoc-in g [from to] true))
            graph
            rules)))

(defn page-comparator [rules]
  (let [graph (rule-graph rules)]
    (fn [a b]
      ;; This assumes that the input is a complete graph, which does seem to be
      ;; true. Otherwise we'd need to traverse the graph.
      (cond
        (get-in graph [a b]) -1
        (get-in graph [b a]) 1
        (= a b) 0
        :else (throw (ex-info "not sure how to compare these!" [a b]))))))

(defn part1 [input]
  (let [{:keys [rules pages]} (parse-input input)
        cmp (page-comparator rules)]
    (->> pages
         (filter #(= (sort cmp %) %))
         (map mid)
         (apply +))))

(comment
  (part1 example)
  ;; => 143

  (time (util/run part1)))

;;; Part 2
;; Re-order the out-of-order pages

(defn part2 [input]
  (let [{:keys [rules pages]} (parse-input input)
        cmp (page-comparator rules)]
    (->> pages
         (keep (fn [p]
                 (let [sorted (sort cmp p)]
                   (when (not= p sorted)
                     sorted))))
         (map mid)
         (apply +))))

(comment
  (part2 example)
  ;; => 123
  
  (time (util/run part2)))

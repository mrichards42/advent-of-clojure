(ns advent-2024.day03
  "Mull It Over"
  (:require [advent.util :as util]
            [clojure.string :as str]))

;;; Part 1
;; Run the mult () instructions

(defn zipmap-some
  "Like zipmap, but ignores nil values"
  [ks vs]
  (->> (map (fn [k v] (when v [k v])) ks vs)
       (filter identity)
       (into {})))

(def valid-instruction-seq
  (let [patterns [[#"(mul)\((\d{1,3}),(\d{1,3})\)" :op :a :b]
                  [#"(do)\(\)" :op]
                  [#"(don't)\(\)" :op]]
        pat (re-pattern (str/join "|" (map first patterns)))
        ks (cons :& (mapcat rest patterns))]
    (fn [input]
      (map (partial zipmap-some ks)
           (re-seq pat input)))))

(defn parse-input [input]
  (for [{:keys [op a b]} (valid-instruction-seq input)]
    (case op
      "mul" {:op op :args [(parse-long a) (parse-long b)]}
      "do" {:op op}
      "don't" {:op op})))

(defn part1 [input]
  (->> input
       (parse-input)
       (filter (comp #{"mul"} :op))
       (map (fn [{[a b] :args}] (* a b)))
       (reduce +)))

(comment
  (part1 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
  ;;=> 161
  (time (util/run part1)))

;;; Part 2
;; do() and don't() turn instructions on and off

(defn filter-do-dont
  ([instructions] (filter-do-dont true instructions))
  ([on? instructions]
   (when (seq instructions)
     (let [[prefix [pivot & more]] (split-with (comp not #{"do" "don't"} :op) instructions)]
       (concat (when on? prefix)
               (lazy-seq (filter-do-dont (= "do" (:op pivot)) more)))))))

(defn part2 [input]
  (->> input
       (parse-input)
       (filter-do-dont)
       (filter (comp #{"mul"} :op))
       (map (fn [{[a b] :args}] (* a b)))
       (reduce +)))

(comment
  (part2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
  ;;=> 48
  (time (util/run part2)))

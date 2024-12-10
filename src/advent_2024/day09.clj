(ns advent-2024.day09
  "Disk Fragmenter"
  (:require [advent.util :as util]))

(def example "2333133121414131402")

;;; Part 1
;; Defragment the disk block-by-block

(defn parse-input-1 [input]
  (let [numbers (map (comp parse-long str) input)
        disk (mapcat repeat numbers (interpose nil (range)))]
    (vec disk)))

(defn find-free-idx [disk idx]
  (cond
    (<= (count disk) idx) nil
    (nil? (disk idx)) idx
    :else (recur disk (inc idx))))

(defn defrag-1
  ([disk] (defrag-1 disk (find-free-idx disk 0)))
  ([disk free-idx]
   (if (or (nil? free-idx) (<= (count disk) free-idx))
     disk
     (if-let [x (peek disk)]
       (let [disk' (assoc (pop disk) free-idx x)]
         (recur disk' (find-free-idx disk' free-idx)))
       (recur (pop disk) free-idx)))))

(defn part1 [input]
  (->> (parse-input-1 input)
       (defrag-1)
       (map-indexed *)
       (apply +)))

(comment
  (time (part1 example))
  ;; => 1928
  (time (util/run part1)))

;;; Part 2
;; Defragment the disk file-by-file

(defn parse-input-2 [input]
  (let [numbers (map (comp parse-long str) input)
        disk (map (fn [size id idx]
                    {:id id
                     :size size
                     :idx idx})
                  numbers
                  (interpose nil (range))
                  (reductions + 0 numbers))]
    {:disk (into (sorted-map)
                 (comp (filter :id)
                       (map (juxt :idx identity)))
                 disk)
     :free (into (sorted-map)
                 (comp (remove :id)
                       (map (juxt :idx :size)))
                 disk)}))

(defn expand-disk [disk]
  (->> (vals disk)
       (partition-all 2 1)
       (mapcat (fn [[{:keys [idx size id]} next-file]]
                 (concat (repeat size (or id \.))
                         (repeat (- (:idx next-file 0) (+ idx size)) \.))))))

(defn find-free-space [free size stop-idx]
  (first
   (for [[idx space] free
         :while (< idx stop-idx)
         :when (<= size space)]
     [idx space])))

(defn defrag-2
  ([disk free] (defrag-2 disk free (reverse (vals disk))))
  ([disk free [file & more-files]]
   #_(println (apply str (expand-disk disk)))
   (let [{file-size :size file-idx :idx} file]
     (if-not file
       disk
       (if-let [[free-idx free-size] (find-free-space free file-size file-idx)]
         (let [disk' (-> disk
                         (assoc free-idx (assoc file :idx free-idx))
                         (dissoc file-idx))
               new-free-idx (+ free-idx file-size)
               new-free-size (- free-size file-size)
               free' (cond-> (dissoc free free-idx)
                       (< file-size free-size) (assoc new-free-idx new-free-size))]
           (recur disk' free' more-files))
         (recur disk free more-files))))))

(defn part2 [input]
  (let [{:keys [disk free]} (parse-input-2 input)]
    (->> (expand-disk (defrag-2 disk free))
         (map #(if (= \. %) 0 %))
         (map-indexed *)
         (apply +))))

(comment
  (part2 example)
  ;; => 2858
  (time (util/run part2)))

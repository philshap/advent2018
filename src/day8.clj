(ns day8
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def input
  (map edn/read-string
       (-> "src/day8-input.txt"
           slurp
           (str/split #" "))))

(def test-input
  (map edn/read-string
       (-> "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
           (str/split #" "))))

(defn sum-metadata [total tree]
  (let [num-child (first tree)
        num-meta (fnext tree)]
    (loop [child num-child
           tree-state [total (nnext tree)]]
      (if (zero? child)
        [(+ (first tree-state)
            (reduce + (take num-meta (second tree-state))))
         (drop num-meta (second tree-state))]
        (recur (dec child) (sum-metadata (first tree-state) (second tree-state)))))))

(defn part1 []
  (first (sum-metadata 0 input)))

(defn sum-index-metadata [tree]
  (let [num-child (first tree)
        num-meta (fnext tree)]
    (loop [child num-child
           child-totals []
           tree-state (nnext tree)]
      (if (zero? child)
        [(->> (take num-meta tree-state)
              ; if no children, meta value is itself, otherwise use it as an index
              (map #(if (empty? child-totals) % (nth child-totals (dec %) 0)))
              (reduce +))
         (drop num-meta tree-state)]
        (let [child-sum (sum-index-metadata tree-state)]
          (recur (dec child) (conj child-totals (first child-sum)) (second child-sum)))))))

(defn part2 []
  (first (sum-index-metadata input)))

;(println "part 1: " (part1))
;(println "part 2: " (part2))

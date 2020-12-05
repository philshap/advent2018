(ns day2
  (:require [clojure.string :as str])
  )

(def input
  (-> "src/day2-input.txt"
      slurp
      str/split-lines))

(defn part1 []
  (loop [words input
         twos 0
         threes 0]
    (if (empty? words)
      (* twos threes)
      (let [values
            (-> words
                first
                seq
                frequencies
                vals
                set)]
        (recur (rest words)
               (if (values 2) (inc twos) twos)
               (if (values 3) (inc threes) threes))))))

(defn expand-word [word]
  "given a word, return all words made by removing one character at any position"
  (let [chars (seq word)]
    (map (partial apply str)
     (map #(concat (take % chars) (drop (inc %) chars))
          (range (count chars))))))

(defn part2 []
  (->> input
       (map expand-word)
       flatten
       frequencies
       (filter (fn [[_k v]] (= v 3)))
       ffirst))

(println "part 1: " (part1))
(println "part 2: " (part2))

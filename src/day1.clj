(ns day1
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def input
  (map edn/read-string
       (-> "src/day1-input.txt"
           slurp
           str/split-lines)))

(defn part1 []
  (reduce + input))

(defn part2 []
  (loop [seen #{}
         l input
         freq 0]
    (or (seen freq)
      (let [new-freq (+ freq (first l))]
        (recur (conj seen freq)
               (or (next l) input)
               new-freq)))))

;(println "part 1: " (part1))
;(println "part 2: " (part2))

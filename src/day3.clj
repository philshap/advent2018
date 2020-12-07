(ns day3
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (-> "src/day3-input.txt"
      slurp
      str/split-lines))

(defstruct claim :id :left :top :w :h)

;; "#14 @ 341,283: 20x12" => (14 341 283 20 12)
(defn parse-entry [entry]
  (apply (partial struct claim)
         (map
           edn/read-string
           (rest (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" entry)))))

(defn expand-claim [claim]
  "given a claim, return all the points inside the claim"
  (for [i (range (claim :w))
        j (range (claim :h))]
    [(+ (claim :left) i) (+ (claim :top) j)]))

(defn part1 []
  (->> input
      (map parse-entry)
      (map expand-claim)
      (apply concat)
      frequencies
      vals
      (filter #(> % 1))
      count))

(defn part2 []
  (let [all-claims (->> input (map parse-entry))
        ;; create a set of all coordinates that only belong to a single claim
        all-ones (->> all-claims
                      (map expand-claim)
                      (apply concat)
                      frequencies
                      (filter (fn [[_k v]] (= v 1)))
                      keys
                      set)]
    (->> all-claims
         (filter (fn [claim]
                   (empty? (set/difference (set (expand-claim claim)) all-ones))))
         first
         :id)))

(println "part 1: " (part1))
(println "part 2: " (part2))

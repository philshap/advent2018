(ns day5
  (:require [clojure.string :as str]))

(def input
  (-> "src/day5-input.txt"
      slurp
      ))

(def react-regex
  (->> (map char (range (int \a) (inc (int \z))))
       (map #(str % (str/upper-case (str %)) "|" (str/upper-case (str %)) %))
       (str/join "|")
       re-pattern))

(defn poly-react [polymer]
  (str/replace polymer react-regex ""))

(defn react-fully [input]
  (loop [polymer input
         prev-poly nil]
    (if (= polymer prev-poly)
      (count polymer)
      (recur (poly-react polymer) polymer))))

(defn part1 []
  (react-fully input))

(defn remove-all [c polymer]
  (str/replace polymer (re-pattern (str c "|" (str/upper-case (str c)))) ""))

(defn part2 []
  (->> (range (int \a) (inc (int \z)))
       (map char)
       (map #(remove-all % input))
       (map react-fully)
       sort
       first))

;; very slow! it works but there should be a faster way to do it

(println "part 1: " (part1))
(println "part 2: " (part2))

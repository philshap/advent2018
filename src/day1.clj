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

;; Input: "aaaabbbcca":small_orange_diamond: Output: [("a", 4), ("b", 3), ("c", 2), ("a", 1)]

(defn part3 [input]
  (-> (reduce (fn [result ch]
                (let [[[last-char last-count] & cdr] result]
                  (if (= last-char ch)
                    (cons [last-char (inc last-count)] cdr)
                    (cons [ch 1] result))))
              ()
              (->> input seq (map str)))
      reverse))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
  )
(ns day12
  (:require [clojure.string :as str]))

(def input
  (-> "src/day12-input.txt"
      slurp
      (str/split #"\n\n")))

(def test-input
  ["#..#.#..##......###...###" "initial state: #..#.#..##......###...###\n\n...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #"])

(defn parse-input [input]
  (let [initial (let [pots (-> input
                               first
                               (str/split #" ")
                               last)]
                  (->> (range 0 (count pots))
                      (filter #(= \# (nth pots %)))
                       set))
        rules (-> input
                  second
                  str/split-lines
                  (->>
                    (map #(str/split % #" "))
                    (filter #(= "#" (last %)))
                    (map first)
                    set))]
    {:init initial :rules rules}))

(defn grow [rules pots]
  (->> (range (- (apply min pots) (count (first rules))) (+ (apply max pots) (count (first rules))))
       (map
         (fn [index]
           [(->> (range (- index 2) (+ index 3))
                 (map #(if (contains? pots %) \# \.))
                 (apply str))
            index]))
       (filter #(contains? rules (first %)))
       (map second)
       set))

(defn print-pots [pots]
  (->> (range (apply min pots) (apply max pots))
       (map #(if (contains? pots %) \# \.))
       (apply str)))

(defn grow-pots [generations]
  (let [{pots :init rules :rules} (parse-input input)]
    (->> (iterate (partial grow rules) pots)
         (drop generations)
         first
         ;print-pots
         (reduce +))))

(defn part1 []
  (grow-pots 20))

(defn part2 []
  (grow-pots 1000))

(comment
  (println (part1))
  (println (part2))
  )

(ns day6
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def input
  (-> "src/day6-input.txt"
      slurp
      str/split-lines))

(def test-input
  '("1, 1"
    "1, 6"
    "8, 3"
    "3, 4"
    "5, 5"
    "8, 9"))

(defn make-point [entry]
  (let [split (str/split entry #", ")]
    {:x (edn/read-string (first split))
     :y (edn/read-string (last split))}))

(defn compute-distance [c1 c2]
  (+ (Math/abs ^int (- (c1 :x) (c2 :x)))
     (Math/abs ^int (- (c1 :y) (c2 :y)))))

(defn compute-owner [coords coord]
  (let [maybe-owner
        (->> coords
             (map #(assoc % :distance (compute-distance coord %)))
             ;; the owner is the one with the smallest distance
             (sort-by :distance))]
    ;; a point can only have one owner
    (if (not= ((first maybe-owner) :distance)
              ((second maybe-owner) :distance))
      (dissoc (first maybe-owner) :distance))))

;; convert input to list of coords
;; find grid min/max x, y
;; for entire grid, find owner of each coord
;; use frequencies to find largest owner

(defn part1 []
  (let* [coords (map make-point input)
         sort-x (sort (map :x coords))
         sort-y (sort (map :y coords))]
    (->> (for [x (range (first sort-x) (inc (last sort-x)))
               y (range (first sort-y) (inc (last sort-y)))]
           ; generate a list of all points in the grid
           {:x x :y y})
         ; compute the owner of each point
         (map (partial compute-owner coords))
         ; group by owner to count number of points owned in total
         frequencies
         (map last)
         sort
         ; return the largest area
         last)))

(defn compute-total-distance [coords coord]
  (->> coords
       (map (partial compute-distance coord))
       (reduce +)))

(defn part2 []
  (let* [coords (map make-point input)
         sort-x (sort (map :x coords))
         sort-y (sort (map :y coords))]
    (->> (for [x (range (first sort-x) (inc (last sort-x)))
               y (range (first sort-y) (inc (last sort-y)))]
           {:x x :y y})
         (map (partial compute-total-distance coords))
         (filter #(< % 10000))
         count)))

(println "part 1: " (part1))
(println "part 2: " (part2))

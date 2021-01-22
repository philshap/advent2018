(ns day10
  (:require [clojure.string :as str]))

; position=< 20709,  51575> velocity=<-2, -5>

(defn parse-line [line]
  (let [[_ x y dx dy] (re-find #"position=< *(.*), *(.*)> velocity=< *(.*), *(.*)>" line)]
    [(read-string x) (read-string y) (read-string dx) (read-string dy)]))

(def input
  (->> "src/day10-input.txt"
      slurp
      str/split-lines
      (map parse-line)))

(defn move-points [points]
  (map (fn [[x y dx dy]] [(+ x dx) (+ y dy) dx dy]) points))

(defn bounding-box [points]
  (let [xs (map first points)
        ys (map second points)]
    [[(reduce min xs) (reduce max xs)] [(reduce min ys) (reduce max ys)]]))

(defn smaller-box? [box1 box2]
  (let [[[x1-min x1-max] [y1-min y1-max]] box1
        [[x2-min x2-max] [y2-min y2-max]] box2]
    (< (+ (- x1-max x1-min) (- y1-max y1-min))
       (+ (- x2-max x2-min) (- y2-max y2-min)))))

(defn find-message [points]
  (loop [points points
         box (bounding-box points)
         cycles 0]
    (let [next-points (move-points points)
          next-box (bounding-box next-points)]
      (if (smaller-box? box next-box)
        [points cycles]
        (recur next-points next-box (inc cycles))))))

(defn print-message [points]
  (let [[[x-min x-max] [y-min y-max]] (bounding-box points)
        all-points (->> points (map #(subvec % 0 2)) set)]
    (->> (for [y (range y-min (inc y-max))
               x (range x-min (inc x-max))]
           (if (contains? all-points [x y]) "##"  "  "))
         (partition (- (inc x-max) x-min))
         (map (partial apply str)))))

;; prints GFNKCGGH
(defn part1 []
  (-> input
      find-message
      first
      print-message
      (->> (map println) dorun)))

;; 10274
(defn part2 []
  (-> input find-message second))

(comment
  (println (part1))
  (println (part2))
  )

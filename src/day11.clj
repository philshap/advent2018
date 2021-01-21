(ns day11)

(def grid-size 300)
(def grid-serial-number 1133)

; The power level in a given fuel cell can be found through the following process:
;
;Find the fuel cell's rack ID, which is its X coordinate plus 10.
;Begin with a power level of the rack ID times the Y coordinate.
;Increase the power level by the value of the grid serial number (your puzzle input).
;Set the power level to itself multiplied by the rack ID.
;Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
;Subtract 5 from the power level.

;; gsn is grid serial number
(defn power-level [[^int x ^int y] gsn]
  (let [rack-id (+ x 10)
        s1 (* rack-id y)
        s2 (+ s1 gsn)
        s3 (* s2 rack-id)
        s4 (mod (quot s3 100) 10)
        s5 (- s4 5)]
    s5))

;(defn square-power [[x y] gsn square-size]
;  (reduce + (for [dx (range 0 square-size)
;                  dy (range 0 square-size)]
;              (power-level [(+ x dx) (+ y dy)] gsn))))

;; modest (10-20%) speedup by precomputing power levels and storing in a vector.
(defn all-power [grid-size gsn]
  (vec (for [x (range 0 grid-size)
             y (range 0 grid-size)]
         (power-level [x y] gsn))))

(defn square-power [[x y] grid-power square-size]
  (reduce + (for [dx (range 0 square-size)
                  dy (range 0 square-size)]
              (nth grid-power (+ (* grid-size (+ x dx)) (+ y dy))))))

(defn find-most-power [square-size]
  (let [grid-power (all-power grid-size grid-serial-number)]
    (->> (for [x (range 0 (- grid-size square-size))
               y (range 0 (- grid-size square-size))
               :let [cell [x y]]]
           [(square-power cell grid-power square-size) cell])
         (reduce #(if (> (first %1) (first %2)) %1 %2)))))

(defn part1 []
  (second (find-most-power 3)))

;;; approach:
;; generate all power levels for grid
;; generate subgrids, find maximum

;; seemed that max was early in search, [108 [237 227] 14]
(defn part2 []
  (->> (for [square-size (range 4 15)]
         (conj (find-most-power square-size) square-size))
       (reduce #(if (> (first %1) (first %2)) %1 %2))))

(comment
  (println (part1))
  (println (part2))
  )

(ns day13
  (:require [clojure.string :as str]))

(def input
  (->>
    ;"src/day13-input-small.txt"
    ;"src/day13-input-small2.txt"
    "src/day13-input.txt"
    slurp
    str/split-lines
    (map vec)
    vec))

(def up \^)
(def down \v)
(def left \<)
(def right \>)
(def corner1 \\)
(def corner2 \/)

(def cart->delta {right [1 0] left [-1 0] down [0 1] up [-0 -1]})
(def corner1->cart {right down, left up, up left, down right})
(def corner2->cart {right up, left down, up right, down left})
(def dir->left {right up, up left, left down, down right})
(def dir->right {right down, down left, left up, up right})
(def dir->edge {right \-, left \-, up \|, down \|})

(def edges #{\- \|})
(def corners #{\\ \/})
(def cross \+)

(defn get-data [input [x y]]
  (nth (nth input y) x))

(defn parse-carts [input]
  (vec (for [y (range (count input))
             :let [row (nth input y)]
             x (range (count row))
             :let [data (nth row x)
                   is-cart? ((-> cart->delta keys set) data)]
             :when is-cart?]
         {:pos [x y] :dir data :rep 0})))

(defn remove-carts [track carts]
  (reduce
    (fn [track cart]
      (let [pos (:pos cart)]
        (assoc track (last pos)
                     (assoc (nth track (last pos)) (first pos) (dir->edge (:dir cart))))))
    track carts))

(defn update-pos [pos delta]
  [(+ (first pos) (first delta)) (+ (second pos) (second delta))])

(defn compare-cart [c1 c2]
  (let [p1 (:pos c1)
        p2 (:pos c2)]
    (or (< (second p1) (second p2))
        (< (first p1) (first p2)))))

(defn move-cart [cart input]
  (let [pos (:pos cart)
        dir (:dir cart)
        rep (:rep cart)
        current (get-data input pos)
        update-cart
        (fn [new-dir new-rep]
          {:pos (update-pos pos (cart->delta new-dir))
           :dir new-dir :rep new-rep})]
    ;(println (list pos dir count current))
    (case current
      \\ (update-cart (corner1->cart dir) rep)
      \/ (update-cart (corner2->cart dir) rep)
      \+ (update-cart (case (mod rep 3)
                        0 (dir->left dir)
                        1 dir
                        2 (dir->right dir))
                      (inc rep))
      (update-cart dir rep))))

(defn place-carts [carts track]
  (reduce
    (fn [track cart]
      (let [pos (:pos cart)]
        (assoc track (last pos)
                     (assoc (nth track (last pos)) (first pos) (:dir cart)))))
    track carts))

(defn draw-track [track carts]
  (dorun (->> track
              (place-carts carts)
              (map str/join)
              (map println))))

(defn carts-collide [carts]
  (->> carts
       (map :pos)
       frequencies
       (filter #(> (val %) 1))
       ffirst))

(defn run-carts-1 []
  (let [carts (parse-carts input)
        track (remove-carts input carts)]
    (loop [carts carts
           index 0]
      (or (carts-collide carts)
          (if (= index (count carts))
            (recur (vec (sort compare-cart carts)) 0)
            (recur (update carts index move-cart track) (inc index)))))))

(defn carts-collide-2 [carts]
  (let [to-remove
        (->> carts
             (map :pos)
             frequencies
             (filter #(> (val %) 1))
             keys
             set)]
    (->> carts
        (map #(if (contains? to-remove (:pos %))
                (assoc % :crashed true)
                %))
        vec)))

(defn move-cart-2 [cart track]
  (if (:crashed cart)
    cart
    (move-cart cart track)))

(defn count-active-carts [carts]
  (->> carts
       (remove :crashed)
       count))

(defn run-carts-2 []
  (let [carts (parse-carts input)
        track (remove-carts input carts)]
    (loop [carts carts
           index 0]
      (if (= (count-active-carts carts) 1)
        (move-cart (->> carts (remove :crashed) first) track)
        (if (= index (count carts))
          (recur (vec (sort compare-cart carts)) 0)
          (recur (carts-collide-2 (update carts index move-cart-2 track))
                 (inc index)))))))

(defn part1 []
  (run-carts-1))

(defn part2 []
  (run-carts-2))

;(comment
(println (part1))
(println (part2))
;)

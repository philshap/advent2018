(ns day14
  (:require [clojure.edn :as edn]))

(def input "503761")
(defn read-recipe [s]
  (->> s
       seq
       (map str)
       (map edn/read-string)
       vec))
(def recipes (read-recipe "37"))
(def times (edn/read-string input))

(defn make-recipes [total]
  (if (> total 9)
    [1 (mod total 10)]
    [total]))

(defn part1 []
  (loop [elf1 0
         elf2 1
         recipes recipes]
    (if (= (count recipes) (+ 10 times))
      (subvec recipes (- (count recipes) 10))
      (let [recipe1 (nth recipes elf1)
            recipe2 (nth recipes elf2)
            new-recipes (into recipes (make-recipes (+ recipe1 recipe2)))]
        (recur (mod (+ elf1 recipe1 1) (count new-recipes))
               (mod (+ elf2 recipe2 1) (count new-recipes))
               new-recipes)))))

; very slow ?
; answer 771617653 was "too high"
(defn part2 []
  (let [target (read-recipe input)]
    (loop [elf1 0
           elf2 1
           recipes recipes]
      (if (and (> (count recipes) (count target))
               (= (subvec recipes (- (count recipes) (count target))) target))
        (- (count recipes) (count target))
        (if (and (> (count recipes) (count target))
                 (= (subvec recipes (- (dec (count recipes)) (count target))) target))
          (- (dec (count recipes)) (count target))
          (let [recipe1 (nth recipes elf1)
                recipe2 (nth recipes elf2)
                new-recipes (into recipes (make-recipes (+ recipe1 recipe2)))]
            (recur (mod (+ elf1 recipe1 1) (count new-recipes))
                   (mod (+ elf2 recipe2 1) (count new-recipes))
                   new-recipes)))))))

;(comment
(println "part 1: " (part1))
(println "part 2: " (part2))
;)

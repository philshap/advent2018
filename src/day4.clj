(ns day4
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def input
  (-> "src/day4-input.txt"
      slurp
      str/split-lines
      sort))

  ;"[1518-02-24 23:57] Guard #1913 begins shift"
  ;"[1518-02-25 00:17] falls asleep"
  ;"[1518-02-25 00:29] wakes up"

(defn parse-guard-line [line]
  (let [split (str/split line #"[\[ :\]]")
        min (Integer/parseInt (nth split 3))
        id (nth split 6)]
    {:min min :id id}))

(def parsed (->> input
                 (map parse-guard-line)))

(defn collect-guard-data [parsed]
  "return map of guard => list of minutes asleep"
  (loop [guards {}
         guard nil
         sleep 0
         events parsed]
    (if events
      (let [min ((first events) :min)
            id ((first events) :id)]
        (cond
          (= id "asleep") (recur guards guard min (next events))
          (= id "up") (recur (update guards guard #(concat (range sleep min) %))
                             guard 0 (next events))
          :else (recur guards id sleep (next events))))
      guards)))

(defn part1 []
  (let [sleepy
        (->> parsed
             collect-guard-data
             ; find the guard with the most minutes asleep
             (sort (fn [x y] (> (count (fnext x)) (count (fnext y)))))
             first
             )
        sleep-min
        (->> sleepy
             ; get minutes
             fnext
             frequencies
             (sort-by last)
             ; return minute of most frequent item
             last
             first)
        ]
    (* (edn/read-string (subs (first sleepy) 1))
       sleep-min)))

(defn part2 []
  (let [sleepy
        (->> parsed
             collect-guard-data
             (map (fn [[k v]] [k (->> v
                                      frequencies
                                      ; find the minute that the guard was sleepiest
                                      (sort-by last)
                                      last)]))
             ; find the guard with the most minutes asleep
             (sort-by #(last (last %)))
             last
             )]
    (* (edn/read-string (subs (first sleepy) 1))
       (first (last sleepy)))))

(println "part 1: " (part1))
(println "part 2: " (part2))

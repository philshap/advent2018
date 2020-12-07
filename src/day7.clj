(ns day7
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (-> "src/day7-input.txt"
      slurp
      str/split-lines))

(def test-input
  '("Step C must be finished before step A can begin."
     "Step C must be finished before step F can begin."
     "Step A must be finished before step B can begin."
     "Step A must be finished before step D can begin."
     "Step B must be finished before step E can begin."
     "Step D must be finished before step E can begin."
     "Step F must be finished before step E can begin."))

(defn parse-step [entry]
  (let [split (str/split entry #" ")]
    [(nth split 1)
     (nth split 7)]))

;; for all steps, collect into map of step => set of required steps
(defn collect-steps [parsed]
  (->> parsed
       ; map step -> required step and required step -> nil to handle steps that have no requirements
       (map (fn [[f l]] {l #{f} f #{}}))
       ; merge map values using union
       (apply (partial merge-with set/union))
       ; sort map by key
       (into (sorted-map))))

(defn order-steps [steps]
  (loop [path ()
         steps steps]
    (if (empty? steps)
      (apply str (reverse path))
      (let [step
            (->> steps
                 (filter (fn [[_k v]] (clojure.set/subset? v (set path))))
                 first
                 first)]
        (recur (conj path step)
               (dissoc steps step))))))

(defn part1 []
  (->> input
      (map parse-step)
      collect-steps
      order-steps))

(defn step-duration [step-name]
  (+ 60 (inc (- (int (first step-name)) (int \A)))))

(defn get-ready-steps [steps complete]
  "Given a collection of steps with dependents and a set of complete steps,
   return ready steps with duration per step"
  (->> steps
       (filter (fn [[_k v]] (clojure.set/subset? v complete)))
       (map (fn [[k _v]] [k (step-duration k)]))))

;; loop until all steps are complete
;; if any steps are in progress, decrement their counters.
;; if any in-counters are zero, move them to completed
;; if any workers are free, and a step is ready, add them to in-progress
(defn time-steps [workers all-steps]
  (loop [seconds -1
         completed #{}
         steps all-steps
         ; list of (step, time left) pairs
         in-progress ()]
    ;(println seconds completed steps in-progress)
    (if (or (= (count completed) (count all-steps)))
      seconds
      (let* [
             ; just completed steps are ones whose completion time is now
             just-completed (filter (fn [[_k v]] (= v seconds)) in-progress)
             ; add these to the new list of completed steps
             new-completed (set/union completed (set (map first just-completed)))
             ; remove completed steps from in-progress
             new-in-progress (filter (fn [[_k v]] (not= v seconds)) in-progress)
             ; given the current completed steps and available workers, find steps we can start
             ready-to-start (->> (get-ready-steps steps new-completed)
                                 ; available workers is total works minus the number of steps being worked on
                                 (take (- workers (count new-in-progress)))
                                 ; update time to be the time when the step will be complete
                                 (map (fn [[k v]] [k (+ v seconds)])))]
        (recur (inc seconds)
               new-completed
               ; remove steps that are ready to start
               (apply dissoc (conj (map first ready-to-start) steps))
               (concat new-in-progress ready-to-start))))))

(defn part2 []
  (->> input
       (map parse-step)
       collect-steps
       (time-steps 5)))

(println "part 1: " (part1))
(println "part 2: " (part2))

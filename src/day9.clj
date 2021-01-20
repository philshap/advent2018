(ns day9)

(defn make-node [value]
  [(atom nil) (atom nil) value])

(defn n-prev [node] (nth node 0))
(defn n-next [node] (nth node 1))
(defn n-val [node] (nth node 2))

(defn ll-prev [node] (deref (n-prev node)))
(defn ll-next [node] (deref (n-next node)))

(defn make-ll [value]
  (let [node (make-node value)]
    (reset! (n-next node) node)
    (reset! (n-prev node) node)))

(defn ll-insert! [node value]
  (let [old-next (ll-next node)
        new-node [(atom node) (atom old-next) value]]
    (reset! (n-next node) new-node)
    (reset! (n-prev old-next) new-node)))

(defn ll-remove! [node]
  (let [new-current (ll-prev node)
        new-next (ll-next node)]
    (reset! (n-next new-current) new-next)
    (reset! (n-prev new-next) new-current)))

(defn play-game [players last]
  (let [current (atom (make-ll 0))
        scores (atom {})]
    (dorun
      (for [marble (range 1 (inc last))]
        (if (= 0 (mod marble 23))
          (let [to-remove (nth (iterate ll-prev @current) 7)]
            (ll-remove! to-remove)
            (reset! current (ll-next to-remove))
            (swap! scores (fn [scores] (update scores (mod marble players) #(+ (if % % 0) marble (n-val to-remove))))))
          (do
           (ll-insert! (ll-next @current) marble)
           (reset! current (ll-next (ll-next @current)))))))
    (reduce max (vals @scores))))

;; 477 players; last marble is worth 70851 points
(defn part1 []
  (play-game 477 70851))

; What would the new winning Elf's score be if the number of the last marble were 100 times larger?
(defn part2 []
  ;; slow, takes about 4s
  (play-game 477 (* 100 70851)))

(comment
  (println "part 1: " (part1))
  (println "part 2: " (part2))
 )

(comment
  (defn ll-print [node]
    (loop [vals [(n-val node)]
           next (ll-next node)
           count 0]
      (if (or (= count 80) (= next node))
        vals
        (recur (conj vals (n-val next)) (ll-next next) (inc count))))))

;;; -*- lexical-binding: t -*-


(setq input
      (with-temp-buffer
        (insert-file-contents "day4-input.txt")
        (sort-lines nil (point-min) (point-max))
        (split-string (buffer-string) "\n" t)))
;; => ("[1518-02-24 23:57] Guard #1913 begins shift" "[1518-02-25 00:17] falls asleep" "[1518-02-25 00:29] wakes up" "[1518-02-25 00:54] falls asleep" "[1518-02-25 00:56] wakes up" "[1518-02-26 00:03] Guard #3469 begins shift" "[1518-02-26 00:07] falls asleep" "[1518-02-26 00:14] wakes up" "[1518-02-26 00:37] falls asleep" "[1518-02-26 00:49] wakes up" "[1518-02-27 00:01] Guard #2557 begins shift" "[1518-02-27 00:27] falls asleep" ...)


(setq test-input
      '("[1518-11-01 00:00] Guard #10 begins shift"
        "[1518-11-01 00:05] falls asleep"
        "[1518-11-01 00:25] wakes up"
        "[1518-11-01 00:30] falls asleep"
        "[1518-11-01 00:55] wakes up"
        "[1518-11-01 23:58] Guard #99 begins shift"
        "[1518-11-02 00:40] falls asleep"
        "[1518-11-02 00:50] wakes up"
        "[1518-11-03 00:05] Guard #10 begins shift"
        "[1518-11-03 00:24] falls asleep"
        "[1518-11-03 00:29] wakes up"
        "[1518-11-04 00:02] Guard #99 begins shift"
        "[1518-11-04 00:36] falls asleep"
        "[1518-11-04 00:46] wakes up"
        "[1518-11-05 00:03] Guard #99 begins shift"
        "[1518-11-05 00:45] falls asleep"
        "[1518-11-05 00:55] wakes up"))

(setq line "[1518-02-24 23:57] Guard #1913 begins shift")
(string-to-number (substring (elt (split-string line) 1) -3))

(setq split (split-string line "[ :[]"))

;; ("" "1518-02-24" "23" "57]" "Guard" "#1913" "begins" "shift")

(defun parse-guard-line (line)
  (let* ((split (split-string line "[ :[]"))
         (date (elt split 1))
         (min (string-to-number (substring (elt split 3) 0 2)))
         (id (elt split 5)))
    (cons min id)))

;;(parse-guard-line "[1518-11-05 00:55] wakes up")

;; a guard is a hash of minute -> number of times sleeping at that minute
(defun make-guard ()
  (make-hash-table))

(defun parse-input (input)
  (let ((guards (make-hash-table :test 'equal))
        (guard)
        (sleep-start))
    (cl-loop
     for line in input do
     (let* ((parsed (parse-guard-line line))
            (min (car parsed))
            (event (cdr parsed)))
       (cond
        ((equal event "up")
         (cl-loop
          for time from sleep-start below min do
          (puthash time (1+ (gethash time guard 0)) guard)))
        ((equal event "asleep") (setq sleep-start min))
        ;; event is guard ID
        (t
         (setq guard (puthash event (gethash event guards (make-guard)) guards))))))
    guards))

;; total time asleep per minute per guard
;; add all these to get total minutes asleep per guard

(setq test-result (parse-input test-input))

(setq result (parse-input input))

(defun part-one (guards)
  (let ((most-time 0)
        (sleepy-id)
        (longest-minute))
    (maphash
     (lambda (id guard)
       (let ((total-time 0)
             (long-minute)
             (long-minute-time 0))
         (maphash (lambda (minute duration)
                    (setq total-time (+ total-time duration))
                    (if (> duration long-minute-time)
                        (progn
                          (setq long-minute-time duration)
                          (setq long-minute minute))))
                  guard)
         (if (> total-time most-time)
             (progn
               (setq most-time total-time)
               (setq sleepy-id id)
               (setq longest-minute long-minute)))))
     guards)
    (list most-time sleepy-id longest-minute)))

;; (part-one result)
;; => (511 "#523" 38)

;; part one:
;; (* 523 38)
;; => 19874


(defun part-two (guards)
  (let ((sleepy-id)
        (longest-minute)
        (longest-minute-time 0))
    (maphash
     (lambda (id guard)
       (let ((long-minute)
             (long-minute-time 0))
         (maphash (lambda (minute duration)
                    (if (> duration long-minute-time)
                        (progn
                          (setq long-minute-time duration)
                          (setq long-minute minute))))
                  guard)
         (if (> long-minute-time longest-minute-time)
             (progn
               (setq sleepy-id id)
               (setq longest-minute long-minute)
               (setq longest-minute-time long-minute-time)))))
     guards)
    (list sleepy-id longest-minute longest-minute-time)))

;; (part-two test-result)
;; => ("#99" 45 3)
;; (part-two result)
;; => ("#463" 49 20)

;; (* 463 49)
;; => 22687

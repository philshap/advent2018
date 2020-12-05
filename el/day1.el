;;; -*- lexical-binding: t -*-

(setq input
      (with-temp-buffer
        (insert-file-contents "day1-input.txt")
        (mapcar 'string-to-number (split-string (buffer-string) "\n" t))))
(apply '+ input)
;; => 502

(let ((seen (make-hash-table))
      (frequency 0)
      (data input))
  (cl-loop
   for i from 0 to 1000000
   for new-frequency = (+ frequency (car data))
   if (gethash new-frequency seen) return (list 'i: i 'frequency: new-frequency)
   do
   (progn
     (setq frequency new-frequency)
     (setq data (or (cdr data) input))
     (puthash frequency t seen))))
;; => (i: 140092 frequency: 71961)

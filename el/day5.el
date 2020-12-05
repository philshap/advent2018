;;; -*- lexical-binding: t -*-

(setq input
      (with-temp-buffer
        (insert-file-contents "day5-input.txt")
        (buffer-string)))

(setq test-input "dabAcCaCBAcCcaDA")

(defun case-match (c1 c2)
  (= (abs (- c1 c2)) 32))

;; (defun reduce-one (input char)
;;   (let ((lower (downcase char)))
;;     (replace-regexp-in-string (string lower char) ""
;;                               (replace-regexp-in-string (string char lower) "" input t)
;;                               t)))

;; (defun reduce-all (input)
;;   (cl-loop
;;    for char from ?A to ?Z do
;;    (setq input (reduce-one input char))
;;    finally return input))


(setq case-regex
  (regexp-opt
   (mapcan
    (lambda (char)
      (list (concat (list char (downcase char))) (concat (list (downcase char) char))))
    (number-sequence ?A ?Z))))

(length "dabAcCaCBAcCcaDA")

(defun reduce (input)
  (let ((size 0))
    (cl-loop
     while (not (= size (length input))) do
     (progn
       (setq size (length input))
       (setq input (replace-regexp-in-string case-regex "" input t))))
    input))

(setq case-fold-search nil)
(setq reduced (reduce input))

(length input)

(length reduced)

(length (reduce input))

;; One of the unit types is causing problems; it's preventing the
;; polymer from collapsing as much as it should. Your goal is to
;; figure out which unit type is causing the most problems, remove all
;; instances of it (regardless of polarity), fully react the remaining
;; polymer, and measure its length.
;;
;; For example, again using the polymer dabAcCaCBAcCcaDA from above:
;;
;; Removing all A/a units produces dbcCCBcCcD. Fully reacting this
;; polymer produces dbCBcD, which has length 6.
;;
;; Removing all B/b units produces daAcCaCAcCcaDA. Fully reacting this
;; polymer produces daCAcaDA, which has length 8.
;;
;; Removing all C/c units produces dabAaBAaDA. Fully reacting this
;; polymer produces daDA, which has length 4.
;;
;; Removing all D/d units produces abAcCaCBAcCcaA. Fully reacting this
;; polymer produces abCBAc, which has length 6.
;;
;; In this example, removing all C/c units was best, producing the
;; answer 4.
;;
;; What is the length of the shortest polymer you can produce by
;; removing all units of exactly one type and fully reacting the
;; result?

(setq case-fold-search t)
(defun remove (input char)
  (replace-regexp-in-string (string char) "" input t))

(defun reduce-2 (input)
  (let ((size (length input)))
    (cl-loop
     for char from ?A to ?Z do
     (setq case-fold-search t)
     (let ((removed (remove input char)))
       (setq case-fold-search nil)
       (let ((reduced (reduce removed)))
         (if (> size (length reduced))
             (setq size (length reduced))))))
    size))

(reduce-2 test-input)
(reduce-2 input)
6189


;;; -*- lexical-binding: t -*-

(setq input
      (with-temp-buffer
        (insert-file-contents "day3-input.txt")
        (split-string (buffer-string) "\n" t)))

;; claim format:
;; #1 @ 16,576: 17x14
(defun make-claim (text)
  (let* ((split (split-string text))
         (id (elt split 0))
         (pos (elt split 2))
         (size (elt split 3)))
    (list
     (cons 'id (string-to-number (substring id 1)))
     (cons 'left (string-to-number (car (split-string pos ","))))
     (cons 'top (string-to-number (cadr (split-string pos ","))))
     (cons 'width (string-to-number (car (split-string size "x"))))
     (cons 'height (string-to-number (cadr (split-string size "x")))))))

;; (make-claim (car input))
;; ==> ((id . 1) (top . 16) (left . 576) (width . 17) (height . 14))

(defun apply-claim (claim sheet)
  (cl-loop
   for i from 0 below (alist-get 'width claim) do
   (cl-loop
    for j from 0 below (alist-get 'height claim) do
    (let ((x (+ i (alist-get 'left claim)))
          (y (+ j (alist-get 'top claim))))
      (puthash (cons x y)
               (cons (alist-get 'id claim) (gethash (cons x y) sheet))
               sheet)))))

(defun apply-claims (claims)
  (let ((sheet (make-hash-table :test 'equal)))
    (mapcar (lambda (claim) (apply-claim claim sheet)) claims)
    sheet))

(defun count-multi-claims (sheet)
  (let ((count 0))
    (maphash (lambda (key value) (if (> (length value) 1) (setq count (1+ count)))) sheet)
    count))

(defun find-no-overlaps (sheet claims)
  (let (no-overlaps)
    (mapc (lambda (claim)
            (let (overlaps)
              (cl-loop
               for i from 0 below (alist-get 'width claim) do
               (cl-loop
                for j from 0 below (alist-get 'height claim) do
                (let ((x (+ i (alist-get 'left claim)))
                      (y (+ j (alist-get 'top claim))))
                  (if (> (length (gethash (cons x y) sheet)) 1)
                      (setq overlaps t)))))
              (if (not overlaps)
                  (setq no-overlaps (cons claim no-overlaps)))))
          claims)
    no-overlaps))

(setq sheet (apply-claims (mapcar 'make-claim input)))
(count-multi-claims sheet)
;; => 111630

(find-no-overlaps sheet (mapcar 'make-claim input))
;; => ((id . 724) (left . 629) (top . 588) (width . 10) (height . 11))

(setq test-input (list "#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"))
(setq sheet2 (apply-claims (mapcar 'make-claim test-input)))
(count-multi-claims sheet2)
;; => 4
(find-no-overlaps sheet2 (mapcar 'make-claim test-input))
;; => (((id . 3) (left . 5) (top . 5) (width . 2) (height . 2)))

(cl-loop
 for i from 0 to 7 do
 (progn
   (cl-loop
    for j from 0 to 7 do
    (let ((value (gethash (cons i j) sheet2)))
      (cond
       ((> (length value) 1) (princ "X"))
       (value (princ (car value)))
       (t (princ ".")))))
   (princ "\n")))

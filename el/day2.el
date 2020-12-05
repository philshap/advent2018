;;; -*- lexical-binding: t -*-

(setq input
      (with-temp-buffer
        (insert-file-contents "day2-input.txt")
        (split-string (buffer-string) "\n" t)))

(defun word-freq (word)
  (let ((freq))
    (mapc (lambda (char)
            (setf (alist-get char freq) (1+ (alist-get char freq 0))))
          word)
    freq))

(mapcar 'cdr (word-freq (car input)))
;; => (1 1 1 1 1 1 1 1 1 1 1 1 ...)

(defun twos-threes (words)
  (let ((twos 0)
        (threes 0))
    (mapc
     (lambda (word)
       (let ((vals (mapcar 'cdr (word-freq word))))
         (if (seq-contains vals 2) (setq twos (1+ twos)))
         (if (seq-contains vals 3) (setq threes (1+ threes)))))
     words)
    (* twos threes)))

(twos-threes input)
;; => 5750

(setq *hole* ?_)

(defun holify-nth (word n)
  (let ((holified (copy-sequence word)))
    (setf (elt holified n) *hole*)
    holified))

;; (holify-nth (car input) 3)

(defun holify-seq (word)
  (mapcar
   (lambda (index) (holify-nth word index))
   (number-sequence 0 (1- (length word)))))

;; (holify-seq (car input))

(defun find-holified-duplicate (input)
  (let ((all-holified (mapcan 'holify-seq input))
        (holified-hash (make-hash-table :test 'equal))
        result)
    (mapc (lambda (name) (puthash name (1+ (gethash name holified-hash 0)) holified-hash)) all-holified)
    (maphash (lambda (key val) (if (= val 2) (setq result key))) holified-hash)
    (apply 'string (seq-filter (lambda (char) (not (eq char *hole*))) result))))

(find-holified-duplicate input)
;; => "tzyvunogzariwkpcbdewmjhxi"

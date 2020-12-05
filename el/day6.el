;;; -*- lexical-binding: t -*-

(setq input
      (with-temp-buffer
        (insert-file-contents "day6-input.txt")
        (split-string (buffer-string) "\n" t)))

(setq test-input
      '("1, 1"
        "1, 6"
        "8, 3"
        "3, 4"
        "5, 5"
        "8, 9"))


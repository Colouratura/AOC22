#lang racket

(require racket/set)
(require threading)

(define item-priority (let ((lowercase (map list
                                            (map integer->char (range 97 123))
                                            (range 1 27)))
                            (uppercase (map list
                                            (map integer->char (range 65 91))
                                            (range 27 53))))
                        (append lowercase uppercase)))

(define (read-in file-name)
  (port->string (open-input-file file-name) #:close? #t))

(define (to-elves str)
  (~> str
      (string-split _ "\n")
      (map (λ (line) (string-trim line)) _)))

(define (list-duplicates lst)
  (~> lst
      (map (λ (group) (map (λ (elf) (string->list elf)) group)) _)
      (map (λ (group) (set-intersect (first group) (second group) (third group))) _)))

(define (group-up lst n)
   (if (not (empty? lst))
       (cons (take lst n) (group-up (drop lst n) n))
       '()))

(define (sum-priority lst)
  (foldl + 0 (map (λ (item) (cadr (assoc item item-priority))) lst)))

(define (solution file-name)
  (~> file-name
      (read-in)
      (to-elves)
      (group-up _ 3)
      (list-duplicates)
      (flatten)
      (sum-priority)))

(solution "./puzzle-input.dat")
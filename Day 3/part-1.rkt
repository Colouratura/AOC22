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

(define (half-string str)
  (let* ((length (string-length str))
         (middle (/ length 2))
         (first-half (substring str 0 middle))
         (second-half (substring str middle length)))
    (list first-half second-half)))

(define (to-compartments lst)
  (map (λ (elf) (half-string elf)) lst))

(define (get-duplicate comp-a comp-b)
  (let ((items-a (string->list comp-a))
        (items-b (string->list comp-b)))
    (car (set-intersect items-a items-b))))

(define (list-duplicates lst)
  (map (λ (elf) (get-duplicate (car elf) (cadr elf))) lst))

(define (sum-priority lst)
  (foldl + 0 (map (λ (item) (cadr (assoc item item-priority))) lst)))

(define (solution file-name)
  (~> file-name
      (read-in)
      (to-elves)
      (to-compartments)
      (list-duplicates)
      (sum-priority)))

(solution "./puzzle-example.dat")
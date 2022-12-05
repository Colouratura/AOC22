#lang racket

(require threading)

(define (file-to-lines file-name)
  (~> (port->string (open-input-file file-name) #:close? #t)
      (string-split _ "\n")
      (map (λ (line) (string-trim line)) _)))

(define (lines-to-elves lst)
  (map (λ (line) (string-split line ",")) lst))

(define (string-to-range str)
  (let ((boundaries (~> str
                        (string-split _ "-")
                        (map string->number _))))
    (range (car boundaries) (+ (cadr boundaries) 1))))

(define (elves-to-assignments lst)
  (map (λ (group) (let ((elf-a (car group))
                        (elf-b (cadr group)))
                    (list (string-to-range elf-a)
                          (string-to-range elf-b))))
       lst))

(define (determine-overlap lst)
  (map (λ (group) (cond
                    ((subset? (car group) (cadr group)) #f)
                    ((subset? (cadr group) (car group)) #f)
                    (else #t)))
       lst))

(define (solution file-name)
  (~> file-name
      (file-to-lines)
      (lines-to-elves)
      (elves-to-assignments)
      (determine-overlap)
      (filter false? _)
      (length)))

(solution "./puzzle-input.dat")
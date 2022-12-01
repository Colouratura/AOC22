#lang racket

(require threading)

(define (from-file name)
 (port->string (open-input-file name) #:close? #t))

(define (to-elves str)
    (string-split str "\n\n"))

(define (collect-elves lst)
 (sort
  (map (λ (elf)
        (apply +
              (~>> (string-split elf "\n")
               (map (λ (n) (string->number n)))))) 
       lst)
  >))

(define (solution name)
 (~> (~> name
      (from-file)
      (to-elves)
      (collect-elves))
  (take _ 3)
  (apply + _)))
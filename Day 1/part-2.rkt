#lang racket

;; define the two files, one for the example and one for the
;; test data sets.
(define EXAMPLE "puzzle-example.dat")
(define TEST "puzzle-input.dat")

;; get the whole contents of a file by path
(define (get-file-content fn)
  (port->string (open-input-file fn) #:close? #t))

;; splits the file into individual entries for elves
(define (string->elves str)
  (string-split str "\n\n"))

;; convert entries to numbers and then sum them for each elf
(define (tabulate-elves elves)
  (map (lambda (elf)
         (apply + (map (lambda (val)
                         (string->number val))
                       (string-split elf "\n"))))
       elves))

;; gives the solution to how many calories the top three elves are carrying
(define (solution fn)
  (let ((calorie-list (tabulate-elves (string->elves (get-file-content fn)))))
    (apply + (take (sort calorie-list >) 3))))
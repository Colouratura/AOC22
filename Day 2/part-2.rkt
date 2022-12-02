#lang racket

(require threading)
(require (prefix-in seq: seq))

(define (read-in file-name)
  (port->string (open-input-file file-name) #:close? #t))

(define (to-matches str)
  (~>> (string-split str "\n")
      (map (λ (line) (string-split line " ")))))

(define (trim lst)
  (~>> lst
       (map (λ (m) (list (car m) (string-trim (cadr m)))))))

(define (to-move c)
  (cond
    ((equal? c "A") "rock")
    ((equal? c "B") "paper")
    ((equal? c "C") "scissors")))

(define (convert op me)
    (cond
        ((and (equal? op "A") (equal? me "X")) (list "rock" "scissors"))
        ((and (equal? op "A") (equal? me "Z")) (list "rock" "paper"))
        ((and (equal? op "B") (equal? me "X")) (list "paper" "rock"))
        ((and (equal? op "B") (equal? me "Z")) (list "paper" "scissors"))
        ((and (equal? op "C") (equal? me "X")) (list "scissors" "paper"))
        ((and (equal? op "C") (equal? me "Z")) (list "scissors" "rock"))
        (else (list (to-move op) (to-move op)))))

(define (normalize lst)
  (~>> lst
  (map (λ (m) (convert (car m) (cadr m))))))

(define (score lst)
  (~>> lst
       (map (λ (m) (list m (cond
                             ((equal? "rock" (cadr m)) 1)
                             ((equal? "paper" (cadr m)) 2)
                             ((equal? "scissors" (cadr m)) 3)))))
       (map (λ (m) (list (first m) (let ((op (caar m))
                                         (me (cadar m))
                                         (score (cadr m)))
                                     (cond
                                       ((equal? op me) (+ 3 score))
                                       ((and (equal? me "rock") (equal? op "scissors")) (+ 6 score))
                                       ((and (equal? me "paper") (equal? op "rock")) (+ 6 score))
                                       ((and (equal? me "scissors") (equal? op "paper")) (+ 6 score))
                                       (else score))))))))

(define (total lst)
  (~>> lst
       (map (λ (m) (cadr m)))
       (foldl + 0)))

(define (solution file-name)
  (~> file-name
      (read-in)
      (to-matches)
      (trim)
      (normalize)
      (score)
      (total)))

(solution "./puzzle-input.dat")
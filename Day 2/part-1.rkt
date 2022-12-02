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

(define (convert c)
  (cond
    ((equal? c "A") "rock")
    ((equal? c "X") "rock")
    ((equal? c "B") "paper")
    ((equal? c "Y") "paper")
    ((equal? c "C") "scissors")
    ((equal? c "Z") "scissors")))

(define (normalize lst)
  (~>> lst
  (map (λ (m) (list (convert (car m)) (convert (cadr m)))))))

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
#lang racket

(require threading)
(require seq)

(define (read-in file-name)
  (port->string (open-input-file file-name) #:close? #t))

(define (to-matches str)
  (~>> (string-split str "\n")
      (map (λ (line) (string-split line " ")))))

(define (trim lst)
  (~>> lst
       (map (λ (m) (list (first m) (string-trim (last m)))))))

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
  (map (λ (m) (list (convert (first m)) (convert (last m)))))))

(define (score lst)
  (~>> lst
       (map (λ (m) (list m (cond
                             ((equal? "rock" (last m)) 1)
                             ((equal? "paper" (last m)) 2)
                             ((equal? "scissors" (last m)) 3)))))
       (map (λ (m) (list (first m) (let ((op (first (first m)))
                                         (me (last (first m)))
                                         (score (last m)))
                                     (cond
                                       ((equal? op me) (+ 3 score))
                                       ((and (equal? me "rock") (equal? op "paper")) score)
                                       ((and (equal? me "rock") (equal? op "scissors")) (+ 6 score))
                                       ((and (equal? me "paper") (equal? op "rock")) (+ 6 score))
                                       ((and (equal? me "paper") (equal? op "scissors")) score)
                                       ((and (equal? me "scissors") (equal? op "rock")) score)
                                       ((and (equal? me "scissors") (equal? op "paper")) (+ 6 score))
                                       (else score))))))))

(define (total lst)
  (~>> lst
       (map (λ (m) (last m)))
       (sequence->list)
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
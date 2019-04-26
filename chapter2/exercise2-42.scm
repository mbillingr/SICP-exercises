(import (builtin core)
        (sicp utils))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
              (lambda (rest-of-queens)
                (map (lambda (new-row)
                       (adjoin-position
                         new-row k rest-of-queens))
                     (enumerate-interval 1 board-size)))
              (queen-cols (- k 1))))))
  (queen-cols board-size))


(define empty-board nil)

(define (adjoin-position row col board)
  (cons (cons row col) board))

; let's just assume the queen in col is the last queen placed,
; so we can take it from the car rather than go looking for it...
(define (safe? col board)
  (null? (filter (lambda (p) (reachable? p (car board)))
                 (cdr board))))

(define (reachable? pos1 pos2)
  (let ((r1 (car pos1))
        (c1 (cdr pos1))
        (r2 (car pos2))
        (c2 (cdr pos2)))
    (or (= r1 r2)
        (= c1 c2)
        (= (abs (- r1 r2))
           (abs (- c1 c2))))))

(define solutions (queens 7))

(display (length solutions))
(newline)

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

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 2 n)))


(define (unique-triples n)
  (flatmap (lambda (i) (map (lambda (jk) (cons i jk))
                            (unique-pairs (- i 1))))
           (enumerate-interval 3 n)))

(define (unique-triples-with-sum n s)
  (filter (lambda (triple) (= s (accumulate + 0 triple)))
          (unique-triples n)))

(display (unique-triples-with-sum 8 15))
(newline)

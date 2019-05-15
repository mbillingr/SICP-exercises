(import (builtin core)
        (sicp utils)
        (sicp utils prime))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (println x))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (partial-sums s)
  (define sum (add-streams s (cons-stream 0 sum)))
  sum)

(define (stream-cadr stream)
  (stream-car (stream-cdr stream)))
(define (stream-caddr stream)
  (stream-car (stream-cdr (stream-cdr stream))))
(define (stream-cdddr stream)
  (stream-cdr (stream-cdr (stream-cdr stream))))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) S1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream s2car (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream s1car
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2)))))))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) S1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< (weight s1car) (weight s2car))
                   (cons-stream s1car
                                (merge-weighted (stream-cdr s1) s2 weight)))
                  (else
                   (cons-stream s2car
                                (merge-weighted s1 (stream-cdr s2) weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

(define (take stream n)
  (if (= n 0)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (take (stream-cdr stream) (- n 1)))))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (integral delayed-integrand initial-value dt)
 (define int
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand)))
    (add-streams (scale-stream integrand dt) int))))
 int)

; ===========================

(define (list->stream list)
  (if (null? list)
    the-empty-stream
    (cons-stream (car list)
                 (list->stream (cdr list)))))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define c 11)
(define m (power 2 48))
(define (rand-update x)
  (remainder (+ (* 6364136223846793005 x) c) m))

(define random-numbers
  (cons-stream (rand-update (runtime))
               (stream-map rand-update random-numbers)))

(define (estimate-integral pred x1 x2 y1 y2)
  (define x-rand (stream-map (lambda (r) (+ x1 (* r (- x2 x1))))
                             (step random-numbers 2)))
  (define y-rand (stream-map (lambda (r) (+ y1 (* r (- y2 y1))))
                             (step random-numbers 2)))
  (monte-carlo (stream-map pred x-rand y-rand)
               0 0))

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

(define (print-stream s)
  (stream-for-each (lambda (x) (display x) (display " ")) s))

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

(define (integral integrand initial-value dt)
  (define int (cons-stream initial-value
                           (add-streams (scale-stream integrand dt)
                                        int)))
  int)

; ===========================

(define (sign-change-detector a b)
  (cond ((and (< a 0) (>= b 0)) 1)
        ((and (>= a 0) (< b 0)) -1)
        (else 0)))

(define (rand-stream l h)
  (cons-stream (+ l (random (+ 1 (- h l))))
               (rand-stream l h)))

(define sense-data (rand-stream -9 9))

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream
      (sign-change-detector avpt last-avpt)
      (make-zero-crossings
        (stream-cdr input-stream) (stream-car input-stream) avpt))))

(define zero-crossings
  (make-zero-crossings sense-data 0 0))

(print-stream (take sense-data 30))
(newline)
(print-stream (take zero-crossings 30))
(newline)

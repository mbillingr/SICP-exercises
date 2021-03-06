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

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define fibs (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))
(define factorials (cons-stream 1 (mul-streams integers factorials)))

(define (integrate-series coefs)
  (div-streams coefs integers))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (stream-map +
                           (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2)))
                           (scale-stream (stream-cdr s1)
                                         (stream-car s2))
                           (scale-stream (stream-cdr s2)
                                         (stream-car s1)))))

(define (invert-unit-series s)
  (define x (cons-stream 1 (mul-series (stream-cdr (scale-stream s -1)) x)))
  x)

(define exp-series (cons-stream 1 (integrate-series exp-series)))

(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream 1 (scale-stream (integrate-series sine-series)
                                                   -1)))

;===========================

(define (div-series num den)
  (if (not (= (stream-car den) 1))
      (error "denominator is not unit series -- DIV-SERIES"))
  (mul-series num (invert-unit-series den)))

(define tan-series (div-series sine-series cosine-series))

(println (stream-ref tan-series 0))  ; 0
(println (stream-ref tan-series 1))  ; 1
(println (stream-ref tan-series 2))  ; 0
(println (stream-ref tan-series 3))  ; 0.3333
(println (stream-ref tan-series 4))  ; 0
(println (stream-ref tan-series 5))  ; 0.1333
(println (stream-ref tan-series 6))  ; 0
(println (stream-ref tan-series 7))  ; 0.0540

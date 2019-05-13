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

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define fibs (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))
(define factorials (cons-stream 1 (mul-streams integers factorials)))

;===========================

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

; Q: Give an interpretation of above procedure.
; A: It constructs a stream where the first item is the integer quotient of
;    num*radix / den. The following items are the recursive application of
;    this operation on the remainder of the division.
;    The result is a stream of digits of given base (radix) as obtained from
;    the division. Basically, this is the manual division algorithm that we
;    learned in school :)

; Q: What are successive elements produced by (expand 1 7 10)?
; A: 1*10 / 7 = 1, 3  => 1
;    3*10 / 7 = 4, 2  => 4
;    2*10 / 7 = 2, 6  => 2
;    6*10 / 7 = 8, 4  => 8
;    4*10 / 7 = 5, 5  => 5
;    5*10 / 7 = 7, 1  => 7
;    1*10 / 7 = 1, 3  => 1
;    3*10 / 7 = ...

; Q: What is produced by (expand 3 8 10)?
; A: 3*10 / 8 = 3, 6  => 3
;    6*10 / 8 = 7, 4  => 7
;    4*10 / 8 = 5, 0  => 5
;    0*10 / 8 = 0, 0  => 0
;    0*10 / 8 = 0, 0  => 0
;    0*10 / 8 = ...

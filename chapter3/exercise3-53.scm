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

;===========================

; Q: Without running the program, describe the elements of the stream defined
;    below.
; A: The first element is 1 and further elements are defined as the sum of the
;    two previous elements.
;    Thus the second element is 2.
;    The third element is 4, then 8, and so on...
;    Mathematically, it seems reasonable that (add-streams s s) produces a
;    stream similar to (scale-stream s 2), which was described in the book as
;    a stream creating consecutive powers of 2.

(define s (cons-stream 1 (add-streams s s)))

(println (stream-ref s 0))
(println (stream-ref s 1))
(println (stream-ref s 2))
(println (stream-ref s 3))
(println "...")
(println (stream-ref s 8))

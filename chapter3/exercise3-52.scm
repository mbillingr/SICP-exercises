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

(define (show x)
  (display-line x)
  x)

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20))) ; 1 3 6 10 15 21 28 36 ...
(println "sum: " sum)  ; 1
(define y (stream-filter even? seq))
(println "sum: " sum)  ; 6 because stream-filter evaluates stream-cdrs until the predicate matches the first time
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(println "sum: " sum)  ; 10 same reason as above

(println (stream-ref y 2))  ; 28 the third even element
(println "sum: " sum)  ; 28 that's how far the stream was evaluated

(display-stream z) ; 10 15 ... 210

; without memo-proc the beginning of the stream would have been evaluated
; multiple times, but with different accumuluator states each time.
; This would have changed the results almost arbitrarily.

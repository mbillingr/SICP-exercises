(import (sicp utils))
(include "chapter4-query.scm")

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter pair?  ;(lambda (s) (not (stream-null? s)))
                             stream)))

(define (negate operands frame-stream)
  (simple-stream-flatmap
    (lambda (frame)
      (if (stream-null? (qeval (negated-query operands)
                               (singleton-stream frame)))
          (singleton-stream frame)
          the-empty-stream))
    frame-stream))

(define (lisp-value call frame-stream)
  (simple-stream-flatmap
    (lambda (frame)
      (if (execute
            (instantiate
              call
              frame
              (lambda (v f)
                (error "Unknown pat var -- LISP-VALUE" v))))
          (singleton-stream frame)
          the-empty-stream))
    frame-stream))

(define (find-assertions pattern frame)
  (simple-stream-flatmap
    (lambda (datum)
      (check-an-assertion datum pattern frame))
    (fetch-assertions pattern frame)))

; b) If Alyssa's observation is correct - streams are either
;    empty or singleton - then the system's behavior does
;    not change.

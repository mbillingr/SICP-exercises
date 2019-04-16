#!/usr/bin/env -S guile -s
!#

(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* m (cos a)))
          ((eq? op 'imag-part) (* m (sin a)))
          ((eq? op 'magnitude) m)
          ((eq? op 'angle) a)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define pi 3.141592)

(define cval (make-from-mag-ang 2 (/ pi 4)))

(println "real:" (apply-generic 'real-part cval))
(println "imag:" (apply-generic 'imag-part cval))
(println "magnitude:" (apply-generic 'magnitude cval))
(println "angle:" (apply-generic 'angle cval))

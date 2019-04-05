#!/usr/bin/env -S guile -s
!#

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; the width of an interval
(define (width-interval x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))

; 1. Show that the width of the sum of two intervals is a function of only the widths.
; we substitute:
;
; (width-interval (add-interval (make-interval a b)
;                               (make-interval c d)))
;
; (/ (- (+ b d) (+ a c)) 2)
;
; (/ (+ (- b a) (- d c)) 2)
;
; (+ (/ (- b a) 2)
;    (/ (- d c) 2)))
;
; ... and this is the sum of the widths.

; 2. Give examples to show that this is not true for multiplication or division.
;
; The following two multiplications multiply intervals ow width 1 and 0 but
; give different results:

(display (mul-interval (make-interval 2 3)
                       (make-interval 4 4)))
(newline)

(display (mul-interval (make-interval 2 3)
                       (make-interval 5 5)))
(newline)

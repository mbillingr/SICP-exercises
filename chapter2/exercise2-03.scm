#!/usr/bin/env -S guile -s
!#

(define (make-point x y)
  (cons x y))
(define x-point car)
(define y-point cdr)

(define (make-rectangle top-left bottom-right)
  (cons top-left bottom-right))

(define (top-left-rectangle r)
  (car r))

(define (bottom-right-rectangle r)
  (cdr r))

(define (width-rectangle r)
  (abs (- (x-point (bottom-right-rectangle r))
          (x-point (top-left-rectangle r)))))

(define (height-rectangle r)
  (abs (- (y-point (bottom-right-rectangle r))
          (y-point (top-left-rectangle r)))))

(define (area-rectangle r)
  (* (width-rectangle r) (height-rectangle r)))

(define (perimeter-rectangle r)
  (* 2 (+ (width-rectangle r) (height-rectangle r))))


(define (print-rectangle r)
  (display "rectangle: ")
  (display r)
  (newline)
  (display "     area: ")
  (display (area-rectangle r))
  (newline)
  (display "perimeter: ")
  (display (perimeter-rectangle r))
  (newline))

(define r1 (make-rectangle (make-point 1 1) (make-point 8 4)))
(print-rectangle r1)
(newline)


(define (make-rectangle center extent)
  (cons center extent))

(define (center-rectangle r)
  (car r))

(define (extent-rectangle r)
  (cdr r))

(define (top-left-rectangle r)
  (make-point (- (x-point (center-rectangle r))
                 (/ (x-point (extent-rectangle r)) 2))
              (- (y-point (center-rectangle r))
                 (/ (y-point (extent-rectangle r)) 2))))

(define (bottom-right-rectangle r)
  (make-point (+ (x-point (center-rectangle r))
                 (/ (x-point (extent-rectangle r)) 2))
              (+ (y-point (center-rectangle r))
                 (/ (y-point (extent-rectangle r)) 2))))

; could have implemented width-rectangle and height-rectangle in terms of extent

(define r2 (make-rectangle (make-point 10 10) (make-point 7 3)))
(print-rectangle r2)

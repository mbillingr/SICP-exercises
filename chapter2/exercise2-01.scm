(import (builtin core)
        (sicp utils))

; rational arithmetic

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(define (numer x) (car x))
(define (denom x) (cdr x))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n (/ n g))
          (d (/ d g)))
      (if (< d 0)
          (cons (- n) (- d))
          (cons n d)))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 2 3)) (newline)
(print-rat (make-rat -2 3)) (newline)
(print-rat (make-rat 2 -3)) (newline)
(print-rat (make-rat -2 -3)) (newline)

(print-rat (make-rat 2 8)) (newline)
(print-rat (make-rat -2 8)) (newline)
(print-rat (make-rat 2 -8)) (newline)
(print-rat (make-rat -2 -8)) (newline)

(print-rat (make-rat 5 3)) (newline)
(print-rat (make-rat -5 3)) (newline)
(print-rat (make-rat 5 -3)) (newline)
(print-rat (make-rat -5 -3)) (newline)

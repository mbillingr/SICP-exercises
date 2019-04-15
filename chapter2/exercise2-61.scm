#!/usr/bin/env -S guile -s
!#

; Sets as ordered lists

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define a '(1 2 3 4))
(define b '(3 4 5 6))

(println (intersection-set a b))

(println (adjoin-set 3 a))
(println (adjoin-set 7 a))
(println (adjoin-set 6 (adjoin-set 7 a)))

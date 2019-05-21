(import (builtin core)
        (sicp utils))

(include "chapter4-lazy.scm")

; consider the following program:

'(
  (define (div a b) (if (= 0 b) (error "divide by zero") (/ a b)))

  (define (take stream n)
    (if (= n 0)
        the-empty-stream
        (cons-stream (stream-car stream)
                     (take (stream-cdr stream) (- n 1)))))

  (display-stream (take (stream-map (lambda (x) (div 1 (- 5 x)))
                                    integers)
                        4)))

; this should display a list with four elements (1/4 1/3 1/2 1/1) but may
; cause an error because the last element 1/0 is evaluated although it is not
; needed.
; An implementation as lists in the fully lazy evaluator does not have this
; problem (see below).

(eval '(begin
         (define (cons x y) (lambda (m) (m x y)))
         (define (car z) (z (lambda (p q) p)))
         (define (cdr z) (z (lambda (p q) q)))

         (define (div a b) (if (= 0 b) (error "divide by zero") (/ a b)))

         (define (list-ref items n)
          (if (=n 0)
              (car items)
              (list-ref (cdr items) (- n 1))))

         (define (map proc items)
          (if (null? items)
              '()
              (cons (proc (car items))
                    (map proc (cdr items)))))

         (define (add-lists list1 list2)
           (cond ((null? list1) list2)
                 ((null? list2) list1)
                 (else (cons (+ (car list1) (car list2))
                             (add-lists (cdr list1) (cdr list2))))))

         (define (for-each proc list)
           (if (null? list)
               'done
               (begin (proc (car list))
                      (for-each proc (cdr list)))))

         (define ones (cons 1 ones))
         (define integers (cons 1 (add-lists ones integers)))

         (define (take list n)
           (if (= n 0)
               '()
               (cons (car list)
                     (take (cdr list) (- n 1)))))

         (define (print-list list)
          (for-each println list))

         (print-list (take (map (lambda (x) (div 1 (- 5 x)))
                                integers)
                           4)))

      the-global-environment)

(driver-loop)

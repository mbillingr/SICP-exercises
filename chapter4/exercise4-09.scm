(import (builtin core)
        (sicp utils))

(include "chapter4-core.scm")

(define (while? exp) (tagged-list? exp 'while))
(define (while-pred exp) (cadr exp))
(define (while-body exp) (cddr exp))
(define (while->combination exp)
  (list 'let
        'while
        '()
        (make-if (while-pred exp)
                 (make-begin (append (while-body exp) (list (list 'while))))
                 'false)))

(define (until? exp) (tagged-list? exp 'until))
(define (until-pred exp) (cadr exp))
(define (until-body exp) (cddr exp))
(define (until->combination exp)
  (list 'let
        'until
        '()
        (append (until-body exp)
                (make-if (until-pred exp)
                         'false
                         (list 'until)))))

(define (for? exp) (tagged-list? exp 'for))
(define (for-init exp) (cadr exp))
(define (for-cond exp) (caddr exp))
(define (for-update exp) (cadddr exp))
(define (for-body exp) (cddddr exp))
(define (for->combination exp)
  (list 'let
        'for
        (list (for-init exp))
        (make-if (for-cond exp)
                 (make-begin (append (for-body exp)
                                     (list (list 'for (for-update exp)))))
                 'false)))


(define x '(while (> x 0) (set! x (- x 1))))
(println x)
(println (while->combination x))

(define x '(until (< x 0) (set! x (- x 1))))
(println x)
(println (until->combination x))

(define x '(for (i 0) (< i 10) (+ i 1) (display i) (newline)))
(println x)
(println (for->combination x))

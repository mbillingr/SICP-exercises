(import (builtin core)
        (sicp utils))

(include "chapter4-lazy.scm")

; This exercise is a strange beast. We are tasked with extending the evaluator
; So that quotations produce lazy lists. In the text these lazy lists are
; created with compound procedures. I see two implementation strategies:
;   a) Let the evaluator invoke a cons procedure from the environment
;      (which environment? current? global?) to construct lists.
;   b) Directly create lazy lists in the evaluator and provide cons, car, and
;      cdr primitives that work with these lazy pairs.
; Approach b) seems cleaner but also sounds like more work because it would
; require a new type of primitive that takes arguments lazily.
;   c) Hybrid approach: provide cons, car, and cdr as compound procedures but
;      don't use them in the evaluator. Instead, convert quoted lists to
;      something that is equivalent to (cons (cons ...)) without the need to
;      evaluate the compound cons.

(define (text-of-quotation exp)
  (if (pair? (cadr exp))
      (list->lazy-list (cadr exp))
      (cadr exp)))

(define (list->lazy-list list)
  (if (null? list)
      '()
      (proc-cons (if (pair? (car list))
                     (list->lazy-list (car list))
                     (car list))
                 (list->lazy-list (cdr list)))))

(define (proc-cons a d)
  (make-procedure '(m)
                  '((m x y))
                  (extend-environment '(x y)
                                      (list a d)
                                      the-empty-environment)))

; override primitives
(eval '(define (cons x y) (lambda (m) (m x y))) the-global-environment)
(eval '(define (car z) (z (lambda (p q) p))) the-global-environment)
(eval '(define (cdr z) (z (lambda (p q) q))) the-global-environment)

(driver-loop)

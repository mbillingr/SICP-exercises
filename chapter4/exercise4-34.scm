(import (builtin core)
        (sicp utils))

(include "chapter4-lazy.scm")

; I decided to make cons a special form, while car and cdr are primitives.
; This is so much more elegant than defining them as compound functions.
; The representation of pairs is simply a tagged list.

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((cons? exp) (eval-cons exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (text-of-quotation exp)
  (if (pair? (cadr exp))
      (list->lazy-list (cadr exp))
      (cadr exp)))

; Note that the elements in the "lazy" list are not actually delayed.
; I don't think it is necessary to delay them because the have been evaluated
; in the host system anyway.
(define (list->lazy-list list)
  (if (null? list)
      '()
      (lazy-cons (if (pair? (car list))
                     (list->lazy-list (car list))
                     (car list))
                 (list->lazy-list (cdr list)))))

(define (eval-cons exp env)
  (lazy-cons
    (delay-it (cadr exp) env)
    (delay-it (caddr exp) env)))

(define (cons? exp)
  (tagged-list? exp 'cons))

(define (lazy-pair? exp)
  (tagged-list? exp 'pair))
(define (lazy-cons a d)
  (list 'pair a d))
(define (lazy-car p)
  (if (lazy-pair? p)
      (cadr p)
      (error "Not a pair" p)))
(define (lazy-cdr p)
  (if (lazy-pair? p)
      (caddr p)
      (error "Not a pair" p)))

(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((lazy-pair? object)
         (print-lazy-list object))
        (else (display object))))

(define (print-lazy-list object)
  (define (print-pair obj n)
    (user-print (force-it (lazy-car obj)))
    (if (= 0 n)
        (display " ...")
        (let ((d (force-it (lazy-cdr obj))))
          (cond ((null? d) 'ok)
                ((lazy-pair? d) (display " ")
                                (print-pair d (- n 1)))
                (else (display " . ")
                      (user-print d))))))
  (display "(")
  (print-pair object 19)
  (display ")"))

(define primitive-procedures
  (list (list 'car lazy-car)
        (list 'cdr lazy-cdr)
        (list 'null? null?)
        (list '= =)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'error error)
        (list 'println println)))

(define the-global-environment (setup-environment))

; ---------------------------------

(eval '(define (simple-add-lists a b)
         (cons (+ (car a) (car b))
               (simple-add-lists (cdr a) (cdr b))))
      the-global-environment)
(eval '(define ones (cons 1 ones)) the-global-environment)
(eval '(define integers (cons 1 (simple-add-lists ones integers))) the-global-environment)

(user-print (eval 'ones the-global-environment))
(newline)
(user-print (eval 'integers the-global-environment))

(driver-loop)

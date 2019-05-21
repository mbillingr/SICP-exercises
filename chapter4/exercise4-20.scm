(import (builtin core)
        (sicp utils))

(include "chapter4-core.scm")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
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
        ((let? exp) (eval (let->combination exp) env))
        ((letrec? exp) (eval (letrec->let exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-spec exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-firstvar spec)
  (caar spec))
(define (let-firstval spec)
  (cadar spec))
(define (let-next spec)
  (cdr spec))
(define (let-vars spec)
  (if (null? spec)
      '()
      (cons (let-firstvar spec)
            (let-vars (let-next spec)))))
(define (let-values spec)
  (if (null? spec)
      '()
      (cons (let-firstval spec)
            (let-values (let-next spec)))))
(define (let->combination exp)
  (cons (make-lambda (let-vars (let-spec exp))
                     (let-body exp))
        (let-values (let-spec exp))))

; ---------------------------

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec->let exp)
  (define (make-body spec)
    (if (null? spec)
        (let-body exp)
        (cons (make-assignment (let-firstvar spec)
                               (let-firstval spec))
              (make-body (let-next spec)))))
  (cons 'let (cons (map (lambda (var) (list var ''*unassigned*))
                        (let-vars (let-spec exp)))
                   (make-body (let-spec exp)))))

(define (make-assignment var val)
  (list 'set! var val))

(define a '(define (f x)
             (letrec
               ((even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
                (odd? (lambda (n) (if (= n 0) false (even? (- n 1))))))
               (even? x))))

(println (caddr a))
(println (letrec->let (caddr a)))
(println (let->combination (letrec->let (caddr a))))

(eval a the-global-environment)
(user-print (eval '(f 88) the-global-environment))
(newline)
(user-print (eval '(f 99) the-global-environment))
(newline)


(define b '(define (f x)
             (let
               ((even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
                (odd? (lambda (n) (if (= n 0) false (even? (- n 1))))))
               (even? x))))
(println (caddr b))
(println (let->combination (caddr b)))

; If one used let in place of letrec, the expression would be transformed to:
((lambda (even? odd?) (even? x))
 (lambda (n) (if (= n 0) true (odd? (- n 1))))
 (lambda (n) (if (= n 0) false (even? (- n 1)))))
; In the resulting expression the inner definitions are defined outside the
; lambda environment, so recursion is not possible. even? and odd? might even
; refer to other variables with the same name in the outer environment.

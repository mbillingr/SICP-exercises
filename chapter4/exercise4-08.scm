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
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (make-define-func name params body)
  (cons 'define
        (cons (cons name params)
              body)))

(define (let? exp) (tagged-list? exp 'let))
(define (named-let? exp) (symbol? (cadr exp)))
(define (let-name exp)
  (cadr exp))
(define (let-spec exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))
(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))
(define (let-vars spec)
  (if (null? spec)
      '()
      (cons (caar spec) (let-vars (cdr spec)))))
(define (let-values spec)
  (if (null? spec)
      '()
      (cons (cadar spec) (let-values (cdr spec)))))
(define (let->combination exp)
  (if (named-let? exp)
      (make-begin (list (make-define-func (let-name exp)
                                          (let-vars (let-spec exp))
                                          (let-body exp))
                        (cons (let-name exp)
                              (let-values (let-spec exp)))))
      (cons (make-lambda (let-vars (let-spec exp))
                         (let-body exp))
            (let-values (let-spec exp)))))

(define x '(let ((a 1) (b 2)) (+ a b) (* a b)))
(println x)
(println (let->combination x))

(define x '(let fib-iter ((a 1) (b 0) (count n))
             (if (= count 0)
                 b
                 (fib-iter (+ a b) a (- count 1)))))
(println x)
(println (let->combination x))
(import (builtin core)
        (sicp utils))

(include "chapter4-core.scm")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((unbind? exp) (eval-unbind exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (eval-unbind exp env)
  (make-unbound! (unbind-variable exp) env)
  'ok)

(define (unbind? exp) (tagged-list? exp 'make-unbound!))
(define (unbind-variable exp) (cadr exp))
(define (set-first-frame! env frame) (set-car! env frame))

(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (cons '() '()))
            ((eq? var (car vars))
             (scan (cdr vars) (cdr vals)))
            (else
              (let ((bindings (scan (cdr vars) (cdr vals))))
                (cons (cons (car vars) (car bindings))
                      (cons (car vals) (cdr bindings)))))))
    (set-first-frame! env (scan (frame-variables frame)
                                (frame-values frame)))))

; make-unbound! is the opposite of define:
;   it only applies to the first frame of the environment.
;   allowing the unbinding of variables in an outer environment would be super
;   awkward... it would allow unhygienic behavior. consider a function that
;   unbinds 'eval. it would proceed up to the global environment if no such
;   function can be found. in my opinion it makes sense to constrict unbinding
;   to the first frame, so that a procedure can only unbind values that were
;   defined inside that procedure.
; unbinding an undefined variable is not an error. (this makes the implementation simpler)

(define env
  (extend-environment
    '(x y z)
    '(12 42 666)
    (list the-empty-environment)))

(println env)
(make-unbound! 'x env)
(println env)
(make-unbound! 'z env)
(println env)
(make-unbound! 'y env)
(println env)

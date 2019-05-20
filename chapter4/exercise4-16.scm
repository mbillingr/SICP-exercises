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

(define (let? exp) (tagged-list? exp 'let))
(define (let-spec exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-vars spec)
  (if (null? spec)
      '()
      (cons (caar spec) (let-vars (cdr spec)))))
(define (let-values spec)
  (if (null? spec)
      '()
      (cons (cadar spec) (let-values (cdr spec)))))
(define (let->combination exp)
  (cons (make-lambda (let-vars (let-spec exp))
                     (let-body exp))
        (let-values (let-spec exp))))

; ---------------------------

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "Unassigned variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (make-assignment var val)
  (list 'set! var val))

(define (scan-out-defines body)
  (define (initializations exprs)
    (cond ((null? exprs) '())
          ((definition? (car exprs))
           (cons (list (definition-variable (car exprs))
                       ''*unassigned*)
                 (initializations (cdr exprs))))
          (else initializations (cdr exprs))))
  (define (transform body)
    (map (lambda (exp)
           (if (definition? exp)
               (make-assignment (definition-variable exp)
                                (definition-value exp))
               exp))
         body))
  (list (cons 'let (cons (initializations body) (transform body)))))

; We can install scan-out-defines either in make-procedure or in procedure-body.
; If we install it in procedure-body we would perform the transformation every
; time the procedure is applied. This seems inefficient.
; If we install it in make-procudere we perform the transformation only when
; the procedure is created. This is more efficient if we assume that procedures
; are usually called multiple times.
; However, there is a drawback in transforming the procedure during creation:
; we do not store the original representation of the procedure. This might make
; debugging more complicated.

(define (contains-defines body)
  (cond ((null? body) false)
        ((definition? (car body))
         true)
        (else (contains-defines (cdr body)))))

(define (make-procedure parameters body env)
  (if (contains-defines body)
      (list 'procedure parameters (scan-out-defines body) env)
      (list 'procedure parameters body env)))

(define body '((define u exp1) (define v exp2) (define (w) exp3) exp4))

(println body)
(println (scan-out-defines body))

(user-print (eval '(lambda (vars) (define u exp1) (define v exp2) (define (w) exp3) exp4)
                  the-global-environment))
(newline)

(trace scan-out-defines)
(driver-loop)

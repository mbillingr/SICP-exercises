(import (builtin core)
        (sicp utils))

(include "chapter4-amb.scm")

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((require? exp) (analyze-require exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (succeed 'ok fail2)
                   (fail2)))
             fail))))

(all-solutions
  '(begin
    (define x (amb 1 2 3 4 5 6))
    (require (= 0 (remainder x 2)))
    x)
  the-global-environment)

(driver-loop)

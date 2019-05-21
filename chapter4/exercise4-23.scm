(import (builtin core)
        (sicp utils))

(include "chapter4-core.scm")

(define (eval exp env) ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
                      (cproc env)
                      (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
        (fproc env)
        (map (lambda (aproc) (aproc env))
             aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
            (procedure-parameters proc)
            args
            (procedure-environment proc))))
        (else (error "Unknown procedure type: EXECUTE-APPLICATION" proc))))

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

; ----------------------------------

(define prog1 '(begin (define (sqr x) (* x x)) (sqr 5)))
(define prog2 '(begin (define (sqr x) (+ x x) (+ x x) (+ x x) (+ x x) (+ x x) (+ x x) (+ x x) (+ x x) (+ x x) (+ x x) (* x x)) (sqr 5)))
(define prog3 '(begin (define (fib n) (cond ((= n 0) 0) ((= n 1) 1) (else (+ (fib (- n 1)) (fib (- n 2)))))) (fib 5)))

(timeit (lambda () (eval prog1 the-global-environment)))
(timeit (lambda () (eval prog2 the-global-environment)))
(timeit (lambda () (eval prog3 the-global-environment)))

(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs))
           ((car procs) env))
          (else
            ((car procs) env)
            (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

(timeit (lambda () (eval prog1 the-global-environment)))
(timeit (lambda () (eval prog2 the-global-environment)))
(timeit (lambda () (eval prog3 the-global-environment)))

; The original procedure builds a sequence of function calls that is invoked
; during execution. In the case of a single expression body it directly returns
; the execution procedure of the body expression. With two expressions it
; returns a procedure that calls each of the two procedures. With three
; expressions the second call is to a procedure that calls the second and the
; third...
;  # expressions in the body | # function calls
; ---------------------------+------------------
;                          1 | 1
;                          2 | 3
;                          3 | 5
;                          4 | 7

; The new procedure loops through a list of execution procedures. This involves
; recursion and calls to primitives (car, cdr).
;  # expressions in the body | # function calls (+ primitive calls)
; ---------------------------+--------------------------------------
;                          1 | 3 (+3)
;                          2 | 5 (+7)
;                          3 | 7 (+9)
;                          4 | 9 (+11)

; I don't see any practical difference in run time measurements.
; Most likely the difference of 2 more function calls and a few primitives is
; dwarved by the rest of evaluation

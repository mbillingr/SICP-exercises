(import (builtin core)
        (sicp utils))

(include "chapter4-core.scm")

;---------------------------------------------
(define prog1 '(begin (define (sqr x) (* x x)) (sqr 5)))
(define prog2 '(begin (define (fib n) (cond ((= n 0) 0) ((= n 1) 1) (else (+ (fib (- n 1)) (fib (- n 2)))))) (fib 9)))
(define prog3 '(begin (define (fac n) (if (= n 0) 1 (* n (fac (- n 1))))) (fac 10)))

(define prog4 '(begin (define (loop n) (if (= n 0) 'ok (begin (sqr n) (loop (- n 1))))) (loop 1000)))

; --------------------------------------------
; Timing the original eval
(define the-global-environment (setup-environment))
(display "(sqr 5): ") (timeit (lambda () (eval prog1 the-global-environment)))
(display "(fib 9): ") (timeit (lambda () (eval prog2 the-global-environment)))
(display "(fib 11): ") (timeit (lambda () (eval '(fib 11) the-global-environment)))
(display "(fac 10): ") (timeit (lambda () (eval prog3 the-global-environment)))
(display "(fac 20): ") (timeit (lambda () (eval '(fac 20) the-global-environment)))
(display "loop: ") (timeit (lambda () (eval prog4 the-global-environment)))

; --------------------------------------------


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

(println "============================")
; --------------------------------------------
; Timing the new  eval
(define the-global-environment (setup-environment))
(display "(sqr 5): ") (timeit (lambda () (eval prog1 the-global-environment)))
(display "(fib 9): ") (timeit (lambda () (eval prog2 the-global-environment)))
(display "(fib 11): ") (timeit (lambda () (eval '(fib 11) the-global-environment)))
(display "(fac 10): ") (timeit (lambda () (eval prog3 the-global-environment)))
(display "(fac 20): ") (timeit (lambda () (eval '(fac 20) the-global-environment)))
(display "loop: ") (timeit (lambda () (eval prog4 the-global-environment)))

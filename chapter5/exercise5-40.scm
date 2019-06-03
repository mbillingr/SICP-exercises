(import (sicp utils))

(include "chapter5-compiler.scm"
         "chapter5-vm.scm"
         "chapter5-assembler.scm"
         "eceval-operations.scm")

(define (make-lexical-address f d) (cons f d))
(define (frame-number laddr) (car laddr))
(define (displacement-number laddr) (cdr laddr))

(define (lexical-address-lookup laddr env)
  (let ((val (nth-var (displacement-number laddr)
                      (frame-values (nth-env (frame-number laddr)
                                             (env))))))
    (if (eq? val '*unassigned*)
        (error "Uninitialized variable -- LEXICAL-ADDRESS-LOOKUP" laddr)
        val)))

(define (lexical-address-set! laddr new-val env)
  (set-nth-var (displacement-number laddr)
               (frame-values (nth-env (frame-number laddr)
                                      (env)))
               new-val))

(define (nth-env n env)
  (cond ((eq? env the-empty-environment)  ; TODO: compare against global enviroment instead?
         (error "Lexical frame out of range -- NTH-ENV"))
        ((= n 0) (first-frame env))
        (else (nth-env (- n 1) (enclosing-environment env)))))

(define (nth-var n vals)
  (cond ((null? vals)
         (error "Lexical displacement out of range -- NTH-VAR"))
        ((= n 0) (car vals))
        (else (nth-var (- n 1) (cdr vals)))))

(define (set-nth-var! n vals new-value)
  (cond ((null? vals)
         (error "Lexical displacement out of range -- SET-NTH-VAR!" n))
        ((= n 0) (set-car vals new-value))
        (else (nth-var (- n 1) (cdr vals)))))

(define (extend-compiletime-env formals c-env)
  (cons formals c-env))

(define (compile exp target linkage c-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp)
         (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage c-env))
        ((definition? exp)
         (compile-definition exp target linkage c-env))
        ((if? exp)
         (compile-if exp target linkage c-env))
        ((lambda? exp)
         (compile-lambda exp target linkage c-env))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage c-env))
        ((cond? exp)
         (compile (cond->if exp) target linkage c-env))
        ((application? exp)
         (compile-application exp target linkage c-env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (compile-assignment exp target linkage c-env)
  (let ((var (assignment-variable exp))
        (get-value-code
          (compile (assignment-value exp) 'val 'next c-env)))
    (end-with-linkage linkage
      (preserving '(env)
        get-value-code
        (make-instruction-sequence '(env val) (list target)
          `((perform (op set-variable-value!)
                     (const ,var)
                     (reg val)
                     (reg env))
            (assign ,target (const ok))))))))

(define (compile-definition exp target linkage c-env)
  (let ((var (definition-variable exp))
        (get-value-code
          (compile (definition-value exp) 'val 'next c-env)))
    (end-with-linkage linkage
      (preserving '(env)
        get-value-code
        (make-instruction-sequence '(env val) (list target)
          `((perform (op define-variable!)
                     (const ,var)
                     (reg val)
                     (reg env))
            (assign ,target (const ok))))))))

(define (compile-if exp target linkage c-env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
            (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next c-env))
            (c-code (compile (if-consequent exp) target consequent-linkage c-env))
            (a-code (compile (if-alternative exp) target linkage c-env)))
        (preserving '(env continue)
          p-code
          (append-instruction-sequences
            (make-instruction-sequence '(val) '()
              `((test (op false?) (reg val))
                (branch (label ,f-branch))))
            (parallel-instruction-sequences
              (append-instruction-sequences t-branch c-code)
              (append-instruction-sequences f-branch a-code))))))))

(define (compile-sequence seq target linkage c-env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage c-env)
      (preserving '(env continue)
        (compile (first-exp seq) target 'next c-env)
        (compile-sequence (rest-exps seq) target linkage c-env))))

(define (compile-lambda exp target linkage c-env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
        (tack-on-instruction-sequence
          (end-with-linkage lambda-linkage
            (make-instruction-sequence '(env) (list target)
              `((assign ,target
                        (op make-compiled-procedure)
                        (label ,proc-entry)
                        (reg env)))))
          (compile-lambda-body exp proc-entry c-env))
        after-lambda))))

(define (compile-lambda-body exp proc-entry c-env)
  (let ((formals (lambda-parameters exp)))
    (println "lambda body env:" (extend-compiletime-env formals c-env))
    (append-instruction-sequences
      (make-instruction-sequence '(env proc argl) '(env)
        `(,proc-entry
          (assign env (op compiled-procedure-env) (reg proc))
          (assign env
                  (op extend-environment)
                  (const ,formals)
                  (reg argl)
                  (reg env))))
      (compile-sequence (lambda-body exp)
                        'val
                        'return
                        (extend-compiletime-env formals c-env)))))

(define (compile-application exp target linkage c-env)
  (let ((proc-code (compile (operator exp) 'proc 'next c-env))
        (operand-codes
          (map (lambda (operand) (compile operand 'val 'next c-env))
               (operands exp))))
    (preserving '(env continue)
      proc-code
      (preserving '(proc continue)
        (construct-arglist operand-codes)
        (compile-procedure-call target linkage)))))

(define code
  (compile
    '(define (sqr x)
      (define (mul x y) (* x y))
      (cond ((even? x) (mul x x))
            (else (+ x x))))
    'val
    'next
    '()))

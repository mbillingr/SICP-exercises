(import (sicp utils))

(include "chapter5-compiler.scm"
         "chapter5-vm.scm"
         "chapter5-assembler.scm"
         "eceval-operations.scm")

(define apply apply-in-underlying-scheme)

(define (make-lexical-address f d) (cons f d))
(define (frame-number laddr) (car laddr))
(define (displacement-number laddr) (cdr laddr))

(define (extend-compiletime-env formals c-env)
  (cons formals c-env))

(define (find-variable var c-env)
  (define (env-loop n env)
    (define (scan i vars)
      (cond ((null? vars) (env-loop (+ n 1) (cdr env)))
            ((eq? var (car vars))
             (make-lexical-address n i))
            (else (scan (+ i 1) (cdr vars)))))
    (if (null? env)
        'not-found
        (scan 0 (car env))))
  (env-loop 0 c-env))

(define (compile exp target linkage c-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp)
         (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage c-env))
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
        ((let? exp)
         (compile (let->combination exp) target linkage c-env))
        ((cond? exp)
         (compile (cond->if exp) target linkage c-env))
        ((binary-primitive-op? exp c-env)
         (compile-binary-op exp target linkage c-env))
        ((n-ary-primitive-op? exp c-env)
         (compile-primitive-op exp target linkage c-env))
        ((application? exp)
         (compile-application exp target linkage c-env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (compile-variable exp target linkage c-env)
  (let ((l-addr (find-variable exp c-env)))
    (if (eq? l-addr 'not-found)
        (end-with-linkage linkage
          (make-instruction-sequence '() (list target 'env)
            `((assign env (op get-global-environment))
              (assign ,target
                      (op lookup-variable-value)
                      (const ,exp)
                      (reg env)))))
        (end-with-linkage linkage
          (make-instruction-sequence '(env) (list target)
            `((assign ,target
                      (op lexical-address-lookup)
                      (const ,l-addr)
                      (reg env))))))))

(define (compile-assignment exp target linkage c-env)
  (let ((var (assignment-variable exp))
        (get-value-code
          (compile (assignment-value exp) 'val 'next c-env)))
    (let ((l-addr (find-variable var c-env)))
      (if (eq? l-addr 'not-found)
          (end-with-linkage linkage
            (append-instruction-sequences
              get-value-code
              (make-instruction-sequence '(val) (list target 'env)
                `((assign env (op get-global-environment))
                  (perform (op set-variable-value!)
                           (const ,var)
                           (reg val)
                           (reg env))
                  (assign ,target (const ok))))))
          (end-with-linkage linkage
            (preserving '(env)
              get-value-code
              (make-instruction-sequence '(env val) (list target)
                `((perform (op lexical-address-set!)
                           (const ,l-addr)
                           (reg val)
                           (reg env))
                  (assign ,target (const ok))))))))))

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
  (let ((formals (lambda-parameters exp))
        (body (lambda-body exp)))
    (append-instruction-sequences
      (make-instruction-sequence '(env proc argl) '(env)
        `(,proc-entry
          (assign env (op compiled-procedure-env) (reg proc))
          (assign env
                  (op extend-environment)
                  (const ,formals)
                  (reg argl)
                  (reg env))))
      (compile-sequence (if (contains-defines body)
                            (scan-out-defines body)
                            body)
                        'val
                        'return
                        (extend-compiletime-env formals c-env)))))

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
               (list 'set! (definition-variable exp) (definition-value exp))
               exp))
         body))
  (list (cons 'let (cons (initializations body) (transform body)))))

(define (contains-defines body)
  (cond ((null? body) false)
        ((definition? (car body))
         true)
        (else (contains-defines (cdr body)))))

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

(define all-regs '(env proc val argl continue arg1 arg2))

(define (spread-arguments operand-list target-registers c-env)
  (cond
    ((and (not (null? operand-list))
          (null? target-registers))
     (error "not enough registers for remaining arguments -- SPREAD-ARGUMENTS"
            operand-list))
    ((null? operand-list)
     (empty-instruction-sequence))
    (else
      (append-instruction-sequences
        (compile (car operand-list) (car target-registers) 'next c-env)
        (preserving (list (car target-registers))
          (spread-arguments (cdr operand-list) (cdr target-registers))
          (make-instruction-sequence target-registers '() '()))))))

(define (compile-binary-op exp target linkage c-env)
  (end-with-linkage linkage
    (append-instruction-sequences
      (spread-arguments (operands exp) '(arg1 arg2))
      (make-instruction-sequence '(arg1 arg2) (list target)
        `((assign ,target
                  (op ,(primitive-operator exp))
                  (reg arg1)
                  (reg arg2)))))))

; I found it more convenient not to use spread-arguments for the multi-argument
; implementation. Instead, I keep applying tho op to an accumulator.
(define (compile-primitive-op exp target linkage c-env)
  (let ((op (primitive-operator exp)))
    (define (iter ops)
      (if (null? (cdr ops))
          (preserving '(arg1)
            (compile (car ops) 'arg2 'next c-env)
            (make-instruction-sequence '(arg1 arg2) (list target)
              `((assign ,target
                        (op ,op)
                        (reg arg1)
                        (reg arg2)))))
          (append-instruction-sequences
            (preserving '(arg1)
              (compile (car ops) 'arg2 'next c-env)
              (make-instruction-sequence '(arg1 arg2) '(arg1)
                `((assign arg1
                          (op ,op)
                          (reg arg1)
                          (reg arg2)))))
            (iter (cdr ops)))))
    (end-with-linkage linkage
      (append-instruction-sequences
        (compile (car (operands exp)) 'arg1 'next c-env)
        (iter (cdr (operands exp)))))))


(define (binary-primitive-op? exp c-env)
  (cond ((not (pair? exp)) false)
        ((not (eq? 'not-found (find-variable (car exp) c-env)))
         false)
        ((eq? (car exp) '=) true)
        (else false)))
(define (n-ary-primitive-op? exp c-env)
  (cond ((not (pair? exp)) false)
        ((not (eq? 'not-found (find-variable (car exp) c-env)))
         false)
        ((eq? (car exp) '*) true)
        ((eq? (car exp) '+) true)
        ((eq? (car exp) '-) true)
        (else false)))
(define (primitive-operator exp)
  (car exp))

(define code
  (statements
    (compile
      '(begin
        (define (*) 0)  ; this re-definition of * in the global environment is ignored
        (define (sqr x)
          (define (+ a b) (* a b))
          (+ x x))
        (sqr 5))
      'val
      'next
      '())))

(pretty-print-code code)


(define machine
 (make-machine '(exp env val continue proc argl unev arg1 arg2)
               eceval-operations
               code))

(define the-global-environment (setup-environment))

(set-register-contents! machine 'env the-global-environment)
(start machine)
(println "---------------------------------------")
(println (get-register-contents machine 'val))

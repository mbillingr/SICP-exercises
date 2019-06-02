(import (sicp utils))

(include "chapter5-compiler.scm"
         "chapter5-vm.scm"
         "chapter5-assembler.scm"
         "eceval-operations.scm")

(define apply apply-in-underlying-scheme)

(define all-regs '(env proc val argl continue arg1 arg2))

(define (spread-arguments operand-list target-registers)
  (cond
    ((and (not (null? operand-list))
          (null? target-registers))
     (error "not enough registers for remaining arguments -- SPREAD-ARGUMENTS"
            operand-list))
    ((null? operand-list)
     (empty-instruction-sequence))
    (else
      (append-instruction-sequences
        (compile (car operand-list) (car target-registers) 'next)
        (preserving (list (car target-registers))
          (spread-arguments (cdr operand-list) (cdr target-registers))
          (make-instruction-sequence target-registers '() '()))))))

(define (compile-primitive-op exp target linkage)
  (end-with-linkage linkage
    (append-instruction-sequences
      (spread-arguments (operands exp) '(arg1 arg2))
      (make-instruction-sequence '(arg1 arg2) (list target)
        `((assign ,target
                  (op ,(primitive-operator exp))
                  (reg arg1)
                  (reg arg2)))))))

(define (primitive-op? exp)
  (cond ((not (pair? exp)) false)
        ((eq? (car exp) '*) true)
        ((eq? (car exp) '+) true)
        ((eq? (car exp) '-) true)
        ((eq? (car exp) '=) true)
        (else false)))
(define (primitive-operator exp)
  (car exp))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp)
         (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((primitive-op? exp)
         (compile-primitive-op exp target linkage))
        ((if? exp)
         (compile-if exp target linkage))
        ((lambda? exp)
         (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage))
        ((cond? exp)
         (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define code
  (statements
    (compile
      '(begin
        (define (factorial n)
          (if (= n 0)
             1
             (* n (factorial (- n 1)))))
        (+ (factorial 5)
           (factorial 4)))
      'val
      'next)))

(pretty-print-code code)

(define machine
  (make-machine '(exp env val continue proc argl unev arg1 arg2)
                eceval-operations
                code))

(define the-global-environment (setup-environment))

(set-register-contents! machine 'env the-global-environment)
(start machine)
(println (get-register-contents machine 'val))

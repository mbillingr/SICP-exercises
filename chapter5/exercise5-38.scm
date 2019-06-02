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

(define (compile-binary-op exp target linkage)
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
(define (compile-primitive-op exp target linkage)
  (let ((op (primitive-operator exp)))
    (define (iter ops)
      (if (null? (cdr ops))
          (preserving '(arg1)
            (compile (car ops) 'arg2 'next)
            (make-instruction-sequence '(arg1 arg2) (list target)
              `((assign ,target
                        (op ,op)
                        (reg arg1)
                        (reg arg2)))))
          (append-instruction-sequences
            (preserving '(arg1)
              (compile (car ops) 'arg2 'next)
              (make-instruction-sequence '(arg1 arg2) '(arg1)
                `((assign arg1
                          (op ,op)
                          (reg arg1)
                          (reg arg2)))))
            (iter (cdr ops)))))
    (end-with-linkage linkage
      (append-instruction-sequences
        (compile (car (operands exp)) 'arg1 'next)
        (iter (cdr (operands exp)))))))


(define (binary-primitive-op? exp)
  (cond ((not (pair? exp)) false)
        ((eq? (car exp) '=) true)
        (else false)))
(define (n-ary-primitive-op? exp)
  (cond ((not (pair? exp)) false)
        ((eq? (car exp) '*) true)
        ((eq? (car exp) '+) true)
        ((eq? (car exp) '-) true)
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
        ((binary-primitive-op? exp)
         (compile-binary-op exp target linkage))
        ((n-ary-primitive-op? exp)
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

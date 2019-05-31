
(import (sicp utils))

(include "chapter5-vm.scm"
         "chapter5-assembler.scm")

; I changed the syntax so that expressions are optionally identified by their
; type. This makes machine code more terse because not every register, constant,
; or label has to be tagged explicitily.

(define (register-exp? exp)
  (or (symbol? exp)
      (tagged-list? exp 'reg)))
(define (register-exp-reg exp)
  (if (symbol? exp)
      exp
      (cadr exp)))

(define (constant-exp? exp)
  (or (number? exp)
      (tagged-list? exp 'const)))
(define (constant-exp-value exp)
  (if (number? exp)
      exp
      (cadr exp)))

(define (label-exp? exp)
  (or (symbol? exp)
      (tagged-list? exp 'label)))
(define (label-exp-label exp)
  (if (symbol? exp)
      exp
      (cadr exp)))

(define fac-machine
  (make-machine
    '(p c n loop)
    (list (list '> >) (list '* *) (list 'inc inc))
    '(  (assign p 1)
        (assign c 1)
        (assign loop (label test-c))  ; resolve ambiguity with explicit tagging
      test-c
        (test (op >) c n)
        (branch done)
        (assign p (op *) c p)
        (assign c (op inc) c)
        (goto (reg loop))  ; resolve ambiguity with explicit tagging
      done)))

(set-register-contents! fac-machine 'n 10)
(start fac-machine)
(println "10! = " (get-register-contents fac-machine 'p))

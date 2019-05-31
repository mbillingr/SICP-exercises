(import (sicp utils))

(include "chapter5-vm.scm"
         "chapter5-assembler.scm")

(define append-machine
  (make-machine
    '(a b out tmp continue)
    (list (list 'null? null?)
          (list 'cons cons)
          (list 'car car)
          (list 'cdr cdr))
    '(controller
        (assign continue (label done))
      loop
        (test (op null?) (reg a))
        (branch (label base-case))
        (save continue)
        (save a)
        (assign a (op cdr) (reg a))
        (assign continue (label after-cdr))
        (goto (label loop))
      after-cdr
        (restore a)
        (restore continue)
        (assign tmp (op car) (reg a))
        (assign out (op cons) (reg tmp) (reg out))
        (goto (reg continue))
      base-case
        (assign out (reg b))
        (goto (reg continue))
      done)))

(define append!-machine
  (make-machine
    '(a b last-pair)
    (list (list 'null? null?)
          (list 'cdr cdr)
          (list 'set-cdr! set-cdr!))
    '(controller
      loop
        (test (op null?) (reg a))
        (branch (label append))
        (assign last-pair (reg a))
        (assign a (op cdr) (reg a))
        (goto (label loop))
      append
        (perform (op set-cdr!) (reg last-pair) (reg b))
      done)))

(define a (list 1 2 3))
(define b (list 'a 'b 'c))
(define c '())

(define (append a b)
  (set-register-contents! append-machine 'a a)
  (set-register-contents! append-machine 'b b)
  (start append-machine)
  (get-register-contents append-machine 'out))

(println (append a b))
(println (append c a))
(println (append b c))

(define (append! a b)
  (set-register-contents! append!-machine 'a a)
  (set-register-contents! append!-machine 'b b)
  (start append!-machine))

(append! a b)
(println a)
(println b)

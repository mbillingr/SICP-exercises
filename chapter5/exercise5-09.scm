
(import (sicp utils))

(include "chapter5-vm.scm"
         "chapter5-assembler.scm")

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs (map (lambda (e) (make-rorc-exp e machine labels))
                     (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (make-rorc-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((register-exp? exp)
         (let ((r (get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Invalid op expression type -- ASSEMBLE" exp))))

(define test-machine
  (make-machine
    '(r c)
    (list (list '+ +))
    '(start
        ;(assign r (op +) (label start) (label done))  ; this would fail now
        (assign c (label start))  ; this is still valid
      done)))

(start test-machine)
(println (get-register-contents test-machine 'r))
(println (get-register-contents test-machine 'c))

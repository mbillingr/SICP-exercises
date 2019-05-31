
(import (sicp utils))

(include "chapter5-vm.scm"
         "chapter5-assembler.scm")


(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (make-stack-entry (stack-inst-reg-name inst)
                                    (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (let ((top (pop stack)))
        (if (eq? (stack-entry-reg-name top)
                 (stack-inst-reg-name inst))
            (set-contents! reg (stack-entry-value top))
            (error "Wrong register -- RESTORE"
                   (stack-entry-reg-name top)
                   (stack-inst-reg-name inst)))
        (advance-pc pc)))))

(define (make-stack-entry reg-name value)
  (cons reg-name value))
(define (stack-entry-reg-name entry)
  (car entry))
(define (stack-entry-value entry)
  (cdr entry))


(define fib-machine
  (make-machine
    '(n val continue)
    (list (list '< <) (list '+ +) (list '- -))
    '(controller
        (assign continue (label fib-done))
      fib-loop
        (test (op <) (reg n) (const 2))
        (branch (label immediate-answer))
        (save continue)
        (assign continue (label afterfib-n-1))
        (save n)
        (assign n (op -) (reg n) (const 1))
        (goto (label fib-loop))
      afterfib-n-1
        (restore n)
        (assign n (op -) (reg n) (const 2))
        (assign continue (label afterfib-n-2))
        (save val)
        (goto (label fib-loop))
      afterfib-n-2
        (assign n (reg val))
        (restore val)
        ;(restore n)  ; this optimization would now cause an error
        (restore continue)
        (assign val (op +) (reg val) (reg n))
        (goto (reg continue))
      immediate-answer
        (assign val (reg n))
        (goto (reg continue))
      fib-done)))

(set-register-contents! fib-machine 'n 10)
(start fib-machine)
(println "fib 10 = " (get-register-contents fib-machine 'val))

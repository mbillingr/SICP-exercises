
(import (sicp utils))

(include "chapter5-vm.scm"
         "chapter5-assembler.scm")

(define fac-machine
  (make-machine
    '(n val continue)
    (list (list '= =) (list '* *) (list '- -))
    '(controller
        (perform (op initialize-stack))
        (assign continue (label fact-done))
      fact-loop
        (test (op =) (reg n) (const 1))
        (branch (label base-case))
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-fact))
        (goto (label fact-loop))
      after-fact
        (restore n)
        (restore continue)
        (assign val (op *) (reg n) (reg val))
        (goto (reg continue))
      base-case
        (assign val (const 1))
        (goto (reg continue))
      fact-done
        (perform (op print-stack-statistics)))))

(define (compute-fac n)
  (set-register-contents! fac-machine 'n n)
  (start fac-machine)
  (get-register-contents fac-machine 'val))


(compute-fac 1)
(compute-fac 2)
(compute-fac 5)
(compute-fac 10)
(compute-fac 15)

; the maximum stack depth is equal to the number of pushes, and both are
; equal to 2n-2.

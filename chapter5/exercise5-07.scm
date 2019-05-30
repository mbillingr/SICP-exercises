
(import (sicp utils))

(include "chapter5-vm.scm"
         "chapter5-assembler.scm")


(define fac-machine
  (make-machine
    '(p c n)
    (list (list '> >) (list '* *) (list 'inc inc))
    '(  (assign p (const 1))
        (assign c (const 1))
      test-c
        (test (op >) (reg c) (reg n))
        (branch (label done))
        (assign p (op *) (reg c) (reg p))
        (assign c (op inc) (reg c))
        (goto (label test-c))
      done)))

(set-register-contents! fac-machine 'n 5)
(start fac-machine)
(println "5! = " (get-register-contents fac-machine 'p))

(define sqrt-machine
  (make-machine
    '(x y guess diff abs-diff rel sum)
    (list (list '< <)
          (list '+ +)
          (list '- -)
          (list '* *)
          (list '/ /)
          (list 'abs abs))
    '(  (assign guess (const 1.0))
      sqrt-loop
        (assign y (op *) (reg guess) (reg guess))
        (assign diff (op -) (reg y) (reg x))
        (assign abs-diff (op abs) (reg diff))
        (test (op <) (reg abs-diff) (const 0.001))
        (branch (label done))
        (assign rel (op /) (reg x) (reg guess))
        (assign sum (op +) (reg guess) (reg rel))
        (assign guess (op /) (reg sum) (const 2.0))
        (goto (label sqrt-loop))
      done)))

(set-register-contents! sqrt-machine 'x 2.0)
(start sqrt-machine)
(println "(sqrt 2) = " (get-register-contents sqrt-machine 'guess))

(define expt-machine
  (make-machine
    '(n b val continue)
    (list (list '= =)
          (list '- -)
          (list '* *))
    '(  (assign continue (label expt-done))
      expt-loop
        (test (op =) (reg n) (const 0))
        (branch (label base-case))
        (save continue)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-expt))
        (goto (label expt-loop))
      after-expt
        (restore continue)
        (assign val (op *) (reg b) (reg val))
        (goto (reg continue))
      base-case
        (assign val (const 1))
        (goto (reg continue))
      expt-done)))

(set-register-contents! expt-machine 'b 3)
(set-register-contents! expt-machine 'n 4)
(start expt-machine)
(println "3 ** 4 = " (get-register-contents expt-machine 'val))

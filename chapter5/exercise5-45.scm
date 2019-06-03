(import (sicp utils))

(include "chapter5-ec-machine2.scm")

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

(pretty-print-code
  (statements
    (compile
      '(define (factorial n)
          (if (<= n 1)
              1
              (* (factorial (- n 1)) n)))
      'val
      'next)))

(compile-and-go
  '(define (factorial n)
     (if (<= n 1)
         1
         (* (factorial (- n 1)) n))))

;  n | #push | max-depth
; ---+-------+------------
;  0 |     7 |   3
;  1 |     7 |   3
;  2 |    13 |   5
;  3 |    19 |   8
;  4 |    25 |  11
;  5 |    31 |  14
; 10 |    61 |  29
;100 |   601 | 299

; mdepth = 3n - 1  (n>1)
; npush = 6n + 1

;         compiled / interpreted  n>>
; npush   (6n + 1) / (32n - 16)   6/32 = ~0.2
; mdepth  (3n - 1) / (5n + 3)     3/5 = 0.6

;         handwritten / interpreted  n>>
; npush   (2n - 2) / (32n - 16)      2/32 = ~0.06
; mdepth  (2n - 2) / (5n + 3)        2/5 = 0.4

;         handwritten / compiled  n>>
; npush   (2n - 2) / (6n + 1)     2/6 = ~0.33
; mdepth  (2n - 2) / (3n - 1)     2/3 = 0.66



; B) Potential compiler improvements
;    We mesure performance in terms of stack usage, so
;      - More registers: the compiler needs to save/restore if a register is
;        used for multiple purposes. In contrast, the handwritten code only
;        saves what is needed for recursion.
;      - Use registers for local variables
;      - Statically detect recursion and optimize recursive calls (I'm not sure
;        if this is desirable in a dynamic language... In scheme, factorial
;        is supposed to behave differently if someone renames and redefines the
;        variable holding the function. The handcrafted machine is static.)

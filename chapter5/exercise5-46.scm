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
      '(define (fib n)
        (if (< n 2)
            n
            (+ (fib (- n 1))
               (fib (- n 2)))))
      'val
      'next)))

(compile-and-go
  '(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1))
           (fib (- n 2))))))

;         compiled       |    interpreted
;  n | #push | max-depth | #push | max-depth
; ---+-------+-----------+-------+------------
;  0 |     7 |   3       |    16 |   8
;  1 |     7 |   3       |    16 |   8
;  2 |    17 |   5       |    72 |  13
;  3 |    27 |   8       |   128 |  18
;  4 |    47 |  11       |   240 |  23
;  5 |    77 |  14       |   408 |  28
; 10 |   887 |  29       |  4944 |  53
; 15 |  9867 |  44       |     - |   -


; S(n) = a * Fib(n+1) + b

;  17 = a * 2 + b
;  27 = a * 3 + b
;  77 = a * 8 + b

;  a = 10                 a = 56
;  b = -3                 b = -40

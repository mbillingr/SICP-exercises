
; copy-paste into evaluator
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

;  n | #push | max-depth
; ---+-------+------------
;  0 |    16 |   8
;  1 |    16 |   8
;  2 |    72 |  13
;  3 |   128 |  18
;  4 |   240 |  23
;  5 |   408 |  28
; 10 |  4944 |  53

; max.depth = 5n + 3

; S(n) = S(n - 1) + S(n - 2) + k
; S(5) = 408 = 240 + 128 + k
; S(4) = 240 = 128 + 72 + k
;
;   => k = 40

; Fib(n) = Fib(n-1) + Fib(n-2))

; S(n) = a * Fib(n+1) + b
;
;   n | 1 2 3 4 5 6
; fib | 1 1 2 3 5 8

;  72 = a * 2 + b
; 128 = a * 3 + b
; 408 = a * 8 + b

;  a = 56
;  b = -40

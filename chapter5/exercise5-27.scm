
; copy-paste into evaluator
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;  n | #push | max-depth
; ---+-------+------------
;  0 |     - |   -
;  1 |    16 |   8
;  2 |    48 |  13
;  3 |    80 |  18
;  4 |   112 |  23
;  5 |   144 |  28

;  FACTORIAL | mdepth | #pushes
; -----------+--------+----------
;  recursive | 5n + 3 | 32n - 16
;  iterative | 10     | 35n + 29

; If the number of pushes is a measure of time required and the maximum depth
; a measure of space required, we can see a typical time/space tradeoff
; between iteration and recursion.
; (Actually, I'm mildly surprised that iteration is slower... of course, these
; measurements do not take the time into account for allocating the space.)

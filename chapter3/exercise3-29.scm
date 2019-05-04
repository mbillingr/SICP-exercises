(import (builtin core)
        (sicp utils))

; compound or-gate
; total delay = and-gate-delay + 2*inverter-delay
(define (or-gate x1 x2 output)
  (let ((a1 (make-wire)) (a2 (make-wire)) (y (make-wire))))
  (inverter x1 a1)
  (inverter x2 a2)
  (and-gate a1 a2 y)
  (inverter y output))

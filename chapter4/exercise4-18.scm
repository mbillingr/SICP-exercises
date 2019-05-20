(import (builtin core)
        (sicp utils))

(include "chapter4-core.scm")

; The original strategy only forbids depending on variables that are not defined yet.
; In contrast, the strategy in this exercise forbids depending on any variables during definition:
;    1. variables are set to *unassigned*
;    2. expressions are evaluated
;    3. variables are set to the resulting values

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; In this example, the definition of y works correctly because evaluation of dy is delayed.
; However, the definition of dy fails because it depends on y, which is *unassigned*
; when (stream-map f y) is evaluated.

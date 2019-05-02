(import (builtin core)
        (sicp utils))

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

; Q: Explain what mystery does in general.
; A: - store the cdr of x in temp
;    - set the cdr of x to y
;    - loop with (x y) <- (temp x)
;    It sets the cdr of the first element to nil. Then it sets the cdr of the
;    next element to the previous element until all elements have been
;    processed. In consequence, the input list is reversed.

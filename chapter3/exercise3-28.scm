(import (builtin core)
        (sicp utils))

(define (or-gate x1 x2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal x1) (get-signal x2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! x1 or-action-procedure)
  (add-action! x2 or-action-procedure)
  'ok)

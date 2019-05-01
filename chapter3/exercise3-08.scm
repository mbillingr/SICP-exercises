(import (builtin core)
        (sicp utils))

(define (f x)
  (if (= first-arg 0)
      first-arg
      (begin (set! first-arg x)
             x)))

; currently, our interpreter evaluates arguments from left to right
(define first-arg '())
(println (+ (f 0) (f 1)))

; let's simulate right-to-left evaluation, to see if the procedure works correctly
(define first-arg '())
(println (+ (f 1) (f 0)))

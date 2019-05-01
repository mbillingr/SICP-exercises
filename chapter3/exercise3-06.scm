(import (builtin core)
        (sicp utils))

(define rand
  (let ((state 42))
    (define (generate)
      (set! state (rand-update state))
      (quotient state d))
    (define (reset seed)
      (set! state seed))
    (define (dispatch m)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) reset)
            (else (error "Unknown request -- RAND" m))))
    dispatch))

(define a 25214903917)
(define c 11)
(define m (power 2 48))
(define d (power 2 16))

(define (rand-update x)
  (remainder (+ (* 6364136223846793005 x) c) m))

((rand 'reset) 42)
(println (rand 'generate))
(println (rand 'generate))
(println (rand 'generate))

((rand 'reset) 42)
(println (rand 'generate))
(println (rand 'generate))
(println (rand 'generate))

(import (builtin core)
        (sicp utils))

(define (estimate-integral pred x1 x2 y1 y2 n-trials)
  (* (monte-carlo n-trials (lambda () (pred (random-in-range x1 x2)
                                            (random-in-range y1 y2))))
     (- x2 x1)
     (- y2 y1)))

(define (random-in-range low high)
  (+ low (random (- high low))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))


(define (estimate-pi n-trials)
  (estimate-integral (lambda (x y) (<= (+ (sqr x) (sqr y)) 1))
                     -1.0 1.0 -1.0 1.0
                     n-trials))

(println (estimate-pi 100))
(println (estimate-pi 1000))
(println (estimate-pi 10000))
(println (estimate-pi 100000))
(println (estimate-pi 1000000))
;(println (estimate-pi 10000000))

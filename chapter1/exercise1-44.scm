(import (builtin core)
        (sicp utils))

(define (compose f g)
  (lambda (x) (f (g x))))

; iterative
(define (repeated f n)
  (define (iter i result)
    (if (= i 1)
        result
        (iter (- i 1) (compose f result))))
  (iter n f))


(define dx 0.1)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (step x)
  (if (< x 0) 0 1))

(define (plot f)
  (display (f -0.2))
  (display " ")
  (display (f -0.1))
  (display " ")
  (display (f 0.0))
  (display " ")
  (display (f 0.1))
  (display " ")
  (display (f 0.2))
  (newline))

(plot step)
(plot (smooth step))
(plot ((repeated smooth 10) step))
(newline)

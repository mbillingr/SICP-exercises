(import (builtin core)
        (sicp utils))

(define tolerance 0.000001)
(define max-iter 1000)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess n)
    (if (= n max-iter)
        (begin
          (display guess)
          #f)
        (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next (+ n 1))))))
  (try first-guess 0))


(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (pow x n)
  (define (iter i result)
    (if (= i n)
        result
        (iter (+ i 1)
              (* result x))))
  (iter 1 x))

(define (compose f g)
  (lambda (x) (f (g x))))

; iterative
(define (repeated f n)
  (define (iter i result)
    (if (= i 1)
        result
        (iter (- i 1) (compose f result))))
  (iter n f))

(define (n-damp n)
  (lambda (x) ((repeated average-damp n) x)))

(define (n-root x n d)
  (define (iter y) (/ x (pow y (- n 1))))
  ;(define (iter y) (/ x (* y y)))
  (fixed-point ((n-damp d) iter) 10))

(display (n-root 2 2 1))
(newline)

(display (n-root 2 3 1))
(newline)

(display (n-root 2 4 2))
(newline)

(display (n-root 2 5 2))
(newline)

(display (n-root 2 6 2))
(newline)

(display (n-root 2 7 2))
(newline)

(display (n-root 2 8 3))
(newline)

(display (n-root 2 15 3))
(newline)

(display (n-root 2 16 4))
(newline)

(display "Looks like we have to repeat average-damp (floor (log2 n)) times...")
(newline)

(define (n-root x n)
  (define (iter y) (/ x (pow y (- n 1))))
  (let ((d (floor (/ (log n)
                     (log 2)))))
       (fixed-point ((n-damp d) iter) 10)))

(display (n-root 2 16))
(newline)

(import (builtin core)
        (sicp utils))

(define fact
        (lambda (n)
          ((lambda (fact) (fact fact n))
           (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))))))

(println (* 1 2 3 4 5 6 7 8 9 10))
(println (fact 10))

(define fib
        (lambda (n)
          ((lambda (fib) (fib fib n))
           (lambda (f k)
                   (if (< k 2)
                       1
                       (+ (f f (- k 1)) (f f (- k 2))))))))

(println (fib 10))


(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(println (f 88))
(println (f 77))

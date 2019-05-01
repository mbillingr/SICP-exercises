(import (builtin core)
        (sicp utils))

(define (make-monitored f)
  (let ((count 0))
    (define (invoke args)
      (set! count (+ count 1))
      (apply f args))
    (define (query)
      count)
    (define (reset)
      (set! count 0))
    (define (dispatch . args)
      (cond ((equal? args '(how-many-calls?)) (query))
            ((equal? args '(reset-count)) (reset))
            (else (invoke args))))
    dispatch))

(define s (make-monitored sqrt))

(println (s 100))
(println (s 'how-many-calls?))

(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define fib (make-monitored fib))

(println "(fib 15) = " (fib 15) ", called " (fib 'how-many-calls?) " times")
(fib 'reset-count)
(println "(fib 5) = " (fib 5) ", called " (fib 'how-many-calls?) " times")

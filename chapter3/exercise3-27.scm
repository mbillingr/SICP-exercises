(import (builtin core)
        (sicp utils))

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record (cdr record) false)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value)
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define (lookup key table)
  ((table 'lookup-proc) key))

(define (insert! key value table)
  ((table 'insert-proc!) key value))

(define (memoize f)
  (let ((table (make-table =)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize
    (lambda (n)
      (println "computing (memo-fib" n ")")
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (memo-fib (- n 1))
                     (memo-fib (- n 2))))))))

(println (memo-fib 3))
(println (memo-fib 10))
(println (memo-fib 12))
(println "=================================")

; Q: Explain whi memo-fib computes the n-th Fibonacci number in a
;    number of steps proportional to n.
; A: Because it only computes Fibonacci numbers for n that have not been
;    computed before. That happens n+1 times.

; Q: Would the scheme still work if we had simply defined memo-fib to be
;    (memoize fib)?
; A1: No. Because fib calls fib recursively.

(define (fib n)
  (println "computing (fib" n ")")
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define memo-fib (memoize fib))

(println (memo-fib 4))
(println "=================================")

; A2: However, it would work if instead, we redefined fib to use memoization

(define fib (memoize fib))
(println (fib 4))

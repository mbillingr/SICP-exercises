(import (builtin core)
        (sicp utils))

(include "chapter4-lazy.scm")

(eval '(define (scale-list list factor)
         (if (null? list)
             '()
             (cons (* (car list) factor)
                   (scale-list (cdr list) factor))))
      the-global-environment)

(eval '(define (fib n)
         (cond ((= n 0) 0) ((= n 1) 1) (else (+ (fib (- n 1)) (fib (- n 2))))))
      the-global-environment)

(eval '(define (dbg-fib n) (println "fib" n) (fib n))
      the-global-environment)

; Without memoization this would be much slower because (fib 10) would be
; evaluated for every list item. With memoization it is only evaluaeted once.
(println (eval '(scale-list '(1 2 3 4 5 6 7 8 9 10) (dbg-fib 10))
               the-global-environment))

(eval '(begin
         (define count 0)
         (define (id x) (set! count (+ count 1)) x)
         (define (square x) (* x x)))
      the-global-environment)

; with memoization
;
; ;;; L-Eval input:
; (square (id 10))
; ;;; L-Eval value:
; 100  ; trivial :)
; ;;; L-Eval input:
; count
; ;;; L-Eval value:
; 1  ; id is evaluated once


; without memoization
;
; ;;; L-Eval input:
; (square (id 10))
; ;;; L-Eval value:
; 100  ; trivial :)
; ;;; L-Eval input:
; count
; ;;; L-Eval value:
; 2  ; id is evaluated each time x is evaluated in the call to square

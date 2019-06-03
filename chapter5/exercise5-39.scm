(import (sicp utils))

(include "chapter5-compiler.scm"
         "chapter5-vm.scm"
         "chapter5-assembler.scm"
         "eceval-operations.scm")

(define (make-lexical-address f d) (cons f d))
(define (frame-number laddr) (car laddr))
(define (displacement-number laddr) (cdr laddr))

(define (lexical-address-lookup laddr env)
  (let ((val (nth-var (displacement-number laddr)
                      (frame-values (nth-env (frame-number laddr)
                                             (env))))))
    (if (eq? val '*unassigned*)
        (error "Uninitialized variable -- LEXICAL-ADDRESS-LOOKUP" laddr)
        val)))

(define (lexical-address-set! laddr new-val env)
  (set-nth-var (displacement-number laddr)
               (frame-values (nth-env (frame-number laddr)
                                      (env)))
               new-val))

(define (nth-env n env)
  (cond ((eq? env the-empty-environment)  ; TODO: compare against global enviroment instead?
         (error "Lexical frame out of range -- NTH-ENV"))
        ((= n 0) (first-frame env))
        (else (nth-env (- n 1) (enclosing-environment env)))))

(define (nth-var n vals)
  (cond ((null? vals)
         (error "Lexical displacement out of range -- NTH-VAR"))
        ((= n 0) (car vals))
        (else (nth-var (- n 1) (cdr vals)))))

(define (set-nth-var! n vals new-value)
  (cond ((null? vals)
         (error "Lexical displacement out of range -- SET-NTH-VAR!" n))
        ((= n 0) (set-car vals new-value))
        (else (nth-var (- n 1) (cdr vals)))))

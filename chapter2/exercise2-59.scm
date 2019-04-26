(import (builtin core)
        (sicp utils))

; Sets as unordered lists

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (adjoin-set (car set1) set2))))


(define a '(1 2 3 4))
(define b (adjoin-set 5 a))
(define c '(4 5 6 7 8))

(println (intersection-set b c) "is the intersection of" b "and" c)
(println (union-set b c) "is the union of" b "and" c)

(import (builtin core)
        (sicp utils))

; Sets as unordered lists

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

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


(define a '(1 2 3 4 1 2 3 4))
(define b (adjoin-set 5 a))
(define c '(4 5 6 7 8 8 8 8))

(println (intersection-set b c) "is the intersection of" b "and" c)
(println (union-set b c) "is the union of" b "and" c)

; Q: How does the efficiency of each procedure compare with the corresponding
;    procedure for the non-duplicate representanion?)
; A: element-of-set? are both O(n) but n is larger in the duplicate representanion
;    adjoin-set is now O(1) compared to O(n) in the non-duplicate case
;    intersection-set are both O(n**2) but n is larger now, so the square is even larger...
;    union-set is now O(n) compared to O(n**2) in the non-duplicate case but it generates larger outputs, thus contributing to higher n in general

; Q: Are there applications for which you would prefer this representation over
;    the non-duplicate one?)
; A: In applications where it is unlikely that an element is not in the set, I
;    guess the duplicate representation may have benefits. Especially if
;    intersection is not an important operation.

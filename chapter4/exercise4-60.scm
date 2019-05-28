(import (sicp utils))
(include "chapter4-query.scm")
(include "chapter4-sample-db.scm")

; We find all pairs of people for which lives-near maches. *Of course* we get
; each pair listed in forward and reverse order.
; (If (a b) matches, (b a) matches too)

; I don't think it's easily possible to avoid these duplicates. One would need
; to impose an order on names (lexical seems reasonable) and select only the
; pair where the order increases.

(display-query '(lives-near ?person (Hacker Alyssa P)))
(display-query '(lives-near ?person-1 ?person-2))

; relying on a < operator that compares symbols lexically...
(define (order-vars a b)
  (< (contract-question-mark a)
     (contract-question-mark b)))

(display-query '(and (lives-near ?p1 ?p2)
                     (lisp-value order-vars ?p1 ?p2)))

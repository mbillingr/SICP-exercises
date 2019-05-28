(import (sicp utils))
(include "chapter4-query.scm")

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append (find-assertions query-pattern frame)
                     (apply-rules query-pattern frame)))
    frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
        (qeval (first-disjunct disjuncts) frame-stream)
        (disjoin (rest-disjuncts disjuncts) frame-stream))))


; Consider a query with an infinite loop:

(assert! '(a b c))
(assert! '(rule (a ?x ?y)
                (a ?x ?y)))

;(display-query '(son Ben ?who))

; With lazy evaluation we still get to see a result, which may
; contain useful information. However, with immediate evaluation
; we don't see anything because the evaluation never finishes.

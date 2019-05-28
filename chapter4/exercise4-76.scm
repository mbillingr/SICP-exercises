(import (sicp utils))
(include "chapter4-query.scm")

(define (conjoin conjuncts frame-stream)
  (accumulate conjoin-frame-streams
              (singleton-stream '())
              (map (lambda (clause)
                     (qeval clause frame-stream))
                   conjuncts)))
(put 'and 'qeval conjoin)

(define (conjoin-frame-streams fs1 fs2)
  (stream-flatmap
    (lambda (f1)
      (stream-filter
        (lambda (frame) (not (eq? frame 'failed)))
        (stream-map (lambda (f2) (merge-frames f1 f2))
                    fs2)))
    fs1))

(define (merge-frames f1 f2)
  (accumulate (lambda (binding frame)
                (if (eq? frame 'failed)
                    'failed
                    (extend-if-possible (binding-variable binding)
                                        (binding-value binding)
                                        frame)))
              f1
              f2))

(define (first-binding frame) (car frame))
(define (rest-bindings frame) (cdr frame))

(assert! '(rule (append-to-form () ?y ?y)))
(assert! '(rule (append-to-form (?u . ?v) ?y (?u . ?z))
                (append-to-form ?v ?y ?z)))

(display-query '(and (append-to-form (1 2) (3 4) ?x)
                     (append-to-form (1) ?y ?x)))

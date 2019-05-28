(import (sicp utils))
(include "chapter4-query.scm")
(include "chapter4-sample-db.scm")

(assert! '(rule (original-outranked-by ?staff-person ?boss)
                (or (supervisor ?staff-person ?boss)
                    (and (supervisor ?staff-person ?middle-manager)
                         (original-outranked-by ?middle-manager ?boss)))))

(assert! '(rule (outranked-by ?staff-person ?boss)
                (or (supervisor ?staff-person ?boss)
                    (and (outranked-by ?middle-manager ?boss)
                         (supervisor ?staff-person
                                     ?middle-manager)))))

; Theoretical implementation:
;    Assumption: we get an infinite recursion if we enter a query that has been
;                entered before, and both have the same information
;                (var pattern, frames).
;    By tracking the stream of frames in which a query is currently evaluated
;    we should be able to determine if a query should be skipped.
;    If a query is skipped we return an empty frame.

; Practical implementation:
;    Track the history of arguments passed to simple-query, ignoring the
;    rule application id.

(define DEPTH 0)

(define QEVAL-HISTORY '())

(define (simple-query query-pattern frame-stream)
  (let ((instances (enter-query query-pattern frame-stream)))
    (cond ((eq? instances 'loop-detected)
           the-empty-stream)
          (else
            (push-history! instances)
            (let ((result (stream-flatmap
                            (lambda (frame)
                              (stream-append
                                (find-assertions query-pattern frame)
                                (apply-rules query-pattern frame)))
                            frame-stream)))
              (pop-history! instances)
              result)))))

; does it really work looking only at the first frame?
(define (enter-query query-pattern frame-stream)
  (let ((instances (instantiate query-pattern
                                (stream-car frame-stream)
                                (lambda (v f) (canonical-name v)))))
    (if (find-history instances)
        'loop-detected
        instances)))

(define (canonical-name v)
  (if (number? (cadr v))
      (caddr v)
      (cadr v)))

(define (find-history x)
  (define (iter hist)
    (cond ((null? hist) false)
          ((equal? x (car hist)) true)
          (else (iter (cdr hist)))))
  (iter QEVAL-HISTORY))

(define (push-history! x)
  (set! QEVAL-HISTORY (cons x QEVAL-HISTORY))
  'ok)

(define (pop-history! x)
  (set! QEVAL-HISTORY (cdr QEVAL-HISTORY)))

(display-query '(outranked-by ?p ?who))
(display-query '(outranked-by (Bitdiddle Ben) ?who))

(assert! '(married Minnie Mickey))
(assert! '(rule (married ?x ?y) (married ?y ?x)))
(display-query '(married Mickey ?who))

; It does not help in this case, though:
(assert! '(son Ben Max))
(assert! '(son Max Timetraveler))
(assert! '(son Timetraveler Ben))

(assert! '(rule (descendant ?ancestor ?person)
                (or (son ?ancestor ?person)
                    (and (son ?ancestor ?ason)
                         (descendant ?ason ?person)))))

;(display-query '(descendant Ben ?who))

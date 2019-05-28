(import (sicp utils)
        (sicp utils set))
(include "chapter4-query.scm")
(include "chapter4-sample-db.scm")

; The simple accumulation scheme can get passed values multiple times.
; Consider the following query that uses a side effects hack to implement
; accumulation with lisp-value. If we use it to compute the total salary of
; all wheels we get the wrong result beacuse Oliver appears four times.

(define total 0)

(define (sum x)
  (set! total (+ total x))
  true)

(display-query '(and (wheel ?x)
                     (salary ?x ?amount)
                     (lisp-value sum ?amount)))
(println "sum:" total)

; Possible solution:
; Only count unique elements. For example, by keeping a set of seen variable
; values and skipping those already in the set. It should be possible to
; implement this as a compound form to be used like (unique (?x) (wheel ?x))

; this procedure defines a query of the form (unique (<vars>) subquery)
; and filters the stream to contain only unique combinations of all bindings
; in <vars>.
; I'm sure there are some gotchas, but it works with the example (if used
; correctly)
(define (unique query-pattern frame-stream)
  (let ((seen (make-set)))
    (stream-filter
      (lambda (frame)
        (let ((bindings (map (lambda (var) (resolve-binding var frame))
                             (unique-vars query-pattern))))
          (cond ((seen 'contains bindings)
                 false)
                (else
                  (seen 'insert bindings)
                  true))))
      (qeval (unique-subquery query-pattern) frame-stream))))
(put 'unique 'qeval unique)

(define (unique-vars query-pattern) (car query-pattern))
(define (unique-subquery query-pattern) (cadr query-pattern))

(define (resolve-binding variable frame)
  (if (var? variable)
      (resolve-binding (binding-value (binding-in-frame variable frame))
                       frame)
      variable))

(define total 0)

(display-query '(and (unique (?x) (wheel ?x))
                     (salary ?x ?amount)
                     (lisp-value sum ?amount)))
(println "sum:" total)

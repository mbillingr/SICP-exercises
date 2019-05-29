(import (sicp utils))
(include "chapter4-query.scm")

; In this approach I tried to follow the hint in the exercise (... perform the
; filtering in a “delayed” manner by appending to the frame a “promise” to
; filter ...).
; Any (negate) or (lisp-value) on unbound variables will create such a promise.
; The promise is fulfilled by trying to apply all promised filters after a
; conjoin query.
; There are potential problems with this solution:
;      Promises are never fulfilled unless negate/lisp-value are used in
;      combination with a conjunction.
; I'm not sure if this is an issue in practice because I think these filters
; only make sense inside a conjunction.

(define (negate operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      (add-filter (negated-query operands)
                  frame
                  (lambda (frame)
                    (stream-null? (qeval (negated-query operands)
                                         (singleton-stream frame))))))
    frame-stream))
(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
  (stream-flatmap
    (lambda (frame)
      (add-filter
        call
        frame
        (lambda (frame)
          (execute (instantiate call
                                frame
                                (lambda (v f)
                                  (error "Unknown pat var -- LISP-VALUE" v)))))))
    frame-stream))
(put 'lisp-value 'qeval lisp-value)

(put 'and 'qeval (lambda (conjuncts frame-stream)
                   (stream-flatmap
                     apply-filters
                     (conjoin conjuncts frame-stream))))

(define (add-filter exp frame filter)
  (cond ((unbound-vars? exp frame)
         (singleton-stream (make-frame (frame-bindings frame)
                                       (cons filter
                                             (frame-filters frame)))))
        ((filter frame)
         (singleton-stream frame))
        (else
          the-empty-stream)))

(define (make-empty-frame) (cons '() '()))
(define (make-frame bindings filters)
  (cons bindings filters))
(define (frame-bindings frame) (car frame))
(define (frame-filters frame) (cdr frame))
(define (filters-pending? frame) (not (null? (frame-filters frame))))

(define (binding-in-frame variable frame)
  (assoc variable (frame-bindings frame)))

(define (extend variable value frame)
  (make-frame (cons (make-binding variable value) (frame-bindings frame))
              (frame-filters frame)))

(define (first-filter frame) (cadr frame))
(define (rest-filters frame) (cddr frame))

(define (apply-filters frame)
  (if (filters-pending? frame)
      (if ((first-filter frame) frame)
          (apply-filters (make-frame (frame-bindings frame)
                                     (rest-filters frame)))
          the-empty-stream)
      (singleton-stream frame)))

(define (unbound-vars? exp frame)
  (define (walk exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (walk (binding-value binding))
                 true)))
          ((pair? exp)
           (or (walk (car exp)) (walk (cdr exp))))
          (else false)))
  (walk exp))

; ------- test it -------

(include "chapter4-sample-db.scm")

(display-query '(and (supervisor ?x ?y)
                     (not (job ?x (computer programmer)))))

(display-query '(and (not (job ?x (computer programmer)))
                     (supervisor ?x ?y)))

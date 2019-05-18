(import (builtin core)
        (sicp utils))

(include "chapter4-core.scm")

; this function constructs a let expression for use in the => clause of cond so
; that the predicate does not need to be evaluated twice.
; our eval does not yet support let, though...
(define (make-let1 varname varval body)
  (cons 'let (cons (list (list varname varval)) body)))

(define (cond-call-clause? clause)
  (eq? (car (cond-actions clause)) '=>))
(define (cond-call-exp clause)
  (cadr (cond-actions clause)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND->IF" clauses)))
              ((cond-call-clause? first)
               (make-let1 'predval
                          (cond-predicate first)
                          (make-if 'predval
                                   (list (cond-call-exp first) 'predval)
                                   (expand-clauses rest))))
              (else
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest)))))))

(println (cond->if '(cond ((assoc 'b '((a 1) (b 2))) => cadr) (else false))))

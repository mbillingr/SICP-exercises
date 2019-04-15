#!/usr/bin/env -S guile -s
!#

(define (deriv exp var)
  (println "deriv" exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (println 'make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((eq? a1 a2) (make-product 2 a1))
        (else (list '+ a1 a2))))

(define (make-product a b)
  (println 'make-product a b)
  (println (=number? a 0) (=number? b 0))
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        ((and (number? a) (number? b)) (* a b))
        (else (list '* a b))))

(define (make-power b e)
  (cond ((=number? b 1) 1)
        ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

; Q(a): Explain what was done above. Why can’t we assim-
;       ilate the predicates number? and variable? into the
;       data-directed dispatch?
;
; A: the (static?) dispatch on the operator was replaced by
;    (dynamic?) dispatch, looking up the function to apply
;    with the operator as key.
;    Numbers and variables (symbols) are native types in
;    our programming language. We can't simply tag them.
;    (I guess we could, but that would make constructing
;    expressions kind of awkward.)

(define (install-derive-sum-package)
  ;; internal procedures
  (define (derive-sum exp var)
    (accumulate make-sum
                0
                (map (lambda (x) (deriv x var)) exp)))
  ;; public interface
  (put 'deriv '+ derive-sum))

(define (install-derive-product-package)
  ;; internal procedures
  (define (multiplier p)
    (if (null? (cdr p)) 1 (car p)))
  (define (multiplicand p)
    (cond ((null? p) 1)
          ((null? (cddr p)) (cadr p))
          (else (cons '* (cdr p)))))
  (define (derive-product exp var)
    (make-sum (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var)
                            (multiplicand exp))))
  ;; public interface
  (put 'deriv '* derive-product))

(define (install-derive-exp-package)
  ;; internal procedures
  (define (base p) (car p))
  (define (exponent p) (cadr p))
  (define (derive exp var)
    (make-product (make-product (exponent exp)
                                (make-power (base exp) (make-sum (exponent exp) -1)))
                  (deriv (base exp) var)))
  ;; public interface
  (put 'deriv '** derive))

(install-derive-sum-package)
(install-derive-product-package)
(install-derive-exp-package)

; Q(d): In this simple algebraic manipulator the type of an
;       expression is the algebraic operator that binds it to-
;       gether. Suppose, however, we indexed the procedures
;       in the opposite way... What corresponding changes to
;       the derivative system are required?
;
; A: Without testing: I'd say, we'd have to change:
;     - the public interface to e.g. (put '+ 'deriv func)
;    and nothing else because this is only affects how the
;    procedures (methods!?) are stored and retrieved in the
;    table.

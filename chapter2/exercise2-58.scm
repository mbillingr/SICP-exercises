#!/usr/bin/env -S guile -s
!#

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((power? exp)
         (make-product (make-product (exponent exp)
                                     (make-power (base exp) (make-sum (exponent exp) -1)))
                       (deriv (base exp) var)))
        (else error "unknown expression type -- DERIV" exp)))

(define (copy-until item sequence)
  (cond ((null? item) '())
        ((= (car sequence) item) '())
        (else (cons (car sequence) (copy-until item (cdr sequence))))))

(define (contains? item sequence)
  (cond ((null? sequence) #f)
        ((= (car sequence) item) #t)
        (else (contains? item (cdr sequence)))))

(define (flatten-scalar sequence)
  (cond ((not (pair? sequence)) sequence)
        ((null? (cdr sequence)) (car sequence))
        (else sequence)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x) (contains? '+ x))
(define (addend s) (flatten-scalar (copy-until '+ s)))
(define (augend s) (flatten-scalar (cdr (memq '+ s))))

(define (product? x) (contains? '* x))
(define (multiplier p) (flatten-scalar (copy-until '* p)))
(define (multiplicand p) (flatten-scalar (cdr (memq '* p))))

(define (power? x) (and (pair? x) (eq? (cadr x) '**)))
(define (base e) (car e))
(define (exponent e) (caddr e))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((null? m1) m2)
        ((null? m2) m1)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-power b e)
  (cond ((=number? b 1) 1)
        ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list b '** e))))

(display " d\n---- (x + 3 * (x + y + 2)) = ")
(display (deriv '(x + 3 * (x + y + 2)) 'x))
(display "\n dx\n")

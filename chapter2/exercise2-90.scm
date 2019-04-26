(import (builtin core)
        (sicp utils)
        (sicp generic)
        (sicp generic math)
        (sicp generic scheme-number))

(define (empty-like x) (apply-generic 'empty-like x))
(define (empty-termlist? l) (apply-generic 'empty-termlist? l))
(define (first-term l) (apply-generic 'first-term l))
(define (rest-terms l) (apply-generic 'rest-terms l))
(define (adjoin-term t l) (apply-generic-method 'adjoin-term l t))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (install-dense-termlist-package)
  ;; internal representation

  (define (adjoin-term term term-list)
    (let ((len (length term-list)))
      (cond ((=zero? (coeff term)) term-list)
            ((= (order term) len)
             (cons (coeff term) term-list))
            ((> (order term) len)
             (adjoin-term term (cons 0 term-list)))
            (else (error "cannot adjoin" term term-list)))))


  (define (the-empty-termlist) '())
  (define (order-termlist term-list) (- (length term-list) 1))
  (define (first-term term-list) (make-term (order-termlist term-list)
                                            (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-termlist coeffs)
    (if (null? coeffs)
        (the-empty-termlist)
        (adjoin-term (make-term (- (length coeffs) 1)
                                (car coeffs))
                     (make-termlist (cdr coeffs)))))

  ;; public interface
  (define (tag p) (attach-tag 'dense-polyterms p))
  (put 'empty-termlist? '(dense-polyterms) empty-termlist?)
  (put 'first-term '(dense-polyterms) first-term)
  (put 'rest-terms '(dense-polyterms) (lambda (l) (tag (rest-terms l))))
  (put 'adjoin-term 'dense-polyterms (lambda (l t) (tag (adjoin-term t l))))
  (put 'empty-like '(dense-polyterms) (lambda (l) (tag (the-empty-termlist))))
  (put 'make 'dense-polyterms (lambda (coeffs) (tag (make-termlist coeffs)))))

(define (make-dense-termlist coeffs)
  ((get 'make 'dense-polyterms) coeffs))

(define (install-sparse-termlist-package)
  ;; internal representation

  (define (adjoin-term term term-list)
    (if (and (=zero? (coeff term)))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-termlist orders coeffs)
    (if (null? coeffs)
        (the-empty-termlist)
        (adjoin-term (make-term (car orders)
                                (car coeffs))
                     (make-termlist (cdr orders)
                                    (cdr coeffs)))))

  ;; public interface
  (define (tag p) (attach-tag 'sparse-polyterms p))
  (put 'empty-termlist? '(sparse-polyterms) empty-termlist?)
  (put 'first-term '(sparse-polyterms) first-term)
  (put 'rest-terms '(sparse-polyterms) (lambda (l) (tag (rest-terms l))))
  (put 'adjoin-term 'sparse-polyterms (lambda (l t) (tag (adjoin-term t l))))
  (put 'empty-like '(sparse-polyterms) (lambda (l) (tag (the-empty-termlist))))
  (put 'make 'sparse-polyterms (lambda (orders coeffs) (tag (make-termlist orders coeffs)))))

(define (make-sparse-termlist orders terms)
  ((get 'make 'sparse-polyterms) orders terms))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and lists

  ;; operations on polynomials
  (define (=zero-poly? p)
    (=zero-terms? (term-list p)))

  (define (=zero-terms? L)
    (cond ((empty-termlist? L) #t)
          ((=zero? (coeff (first-term L)))
           (=zero-terms? (rest-terms L)))
          (else #f)))

  (define (print-poly p)
    (print-terms (variable p) (term-list p) "["))

  (define (print-terms var L prefix)
    (if (empty-termlist? L)
        (display "]")
        (let ((t (first-term L)))
          (display prefix)
          (display (coeff t))
          (display var)
          (display "^")
          (display (order t))
          (print-terms var (rest-terms L) " + "))))

  (define (neg-poly p)
    (make-poly (variable p)
               (neg-terms (term-list p))))

  (define (neg-terms L)
    (if (empty-termlist? L)
        (empty-like L)
        (let ((t (first-term L)))
          (adjoin-term (make-term (order t)
                                  (neg (coeff t)))
                       (neg-terms (rest-terms L))))))

  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY" (list p1 p2))))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term (make-term (order t1)
                                              (add (coeff t1) (coeff t2)))
                                   (add-terms (rest-terms L1)
                                              (rest-terms L2)))))))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY" (list p1 p2))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (empty-like L1)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (empty-like L)
        (let ((t2 (first-term L)))
          (adjoin-term
            (make-term (+ (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
            (mul-term-by-all-terms t1 (rest-terms L))))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'neg '(polynomial) (lambda (p) (tag (neg-poly p))))
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'print '(polynomial) print-poly)
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))

  'done)

(install-dense-termlist-package)
(install-sparse-termlist-package)
(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define a (make-polynomial 'x (make-dense-termlist '(1 3 0 2))))
(define b (make-polynomial 'x (make-sparse-termlist '(10 5 1 0) '(-1 0 2 1))))
(print a) (newline)
(print b) (newline)

(print (mul a b)) (newline)
(print (mul b a)) (newline)

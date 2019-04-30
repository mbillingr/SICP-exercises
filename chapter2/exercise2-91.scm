(import (builtin core)
        (sicp utils)
        (sicp generic)
        (sicp generic math)
        (sicp generic scheme-number))

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
        (the-empty-termlist)
        (let ((t (first-term L)))
          (adjoin-term (make-term (order t)
                                  (neg (coeff t)))
                       (neg-terms (rest-terms L))))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- SUB-POLY" (list p1 p2))))

  (define (sub-terms L1 L2)
    (add-terms L1 (neg-terms L2)))

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
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
            (make-term (+ (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
            (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((result (div-terms (term-list p1)
                                 (term-list p2))))
          (list (make-poly (variable p1)
                           (car result))
                (make-poly (variable p1)
                           (cadr result))))
        (error "Polys not in same var -- DIV-POLY" (list p1 p2))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                        (div-terms (sub-terms L1
                                      (mul-terms L2
                                         (adjoin-term (make-term new-o new-c)
                                                      (the-empty-termlist))))
                                   L2)))
                  (list (adjoin-term (make-term new-o new-c)
                                     (car rest-of-result))
                        (cadr rest-of-result))))))))


  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
    (lambda (p1 p2)
      (let ((result (div-poly p1 p2)))
        (list (tag (car result))
              (tag (cadr result))))))
  (put 'neg '(polynomial) (lambda (p) (tag (neg-poly p))))
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'print '(polynomial) print-poly)
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))

  'done)

(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))


(define (print-division x y)
  (let ((result (div x y)))
    (print x) (newline)
    (display "--------------------- = ")
    (print (car result))
    (display ", remainder: ")
    (print (cadr result))
    (newline)
    (print y) (newline)))

(define a (make-polynomial 'x '((3 1) (2 3) (0 2))))
(define b (make-polynomial 'x '((2 2))))

(print-division a b)
(newline)

(define n (make-polynomial 'x '((5 1) (0 -1))))
(define d (make-polynomial 'x '((2 1) (0 -1))))

(print-division n d)
(newline)

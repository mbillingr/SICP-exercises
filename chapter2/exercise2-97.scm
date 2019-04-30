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
  (define (variable<? v1 v2)
    (symbol<? v1 v2))
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
          (print (coeff t))
          (display var)
          (display "^")
          (display (order t))
          (print-terms var (rest-terms L) " + "))))

  (define (change-variable v p)
    (if (same-variable? v (variable p))
        p
        (make-poly v (list (make-term 0 (tag p))))))

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

  (define (scale-poly x p)
    (make-poly (variable p)
               (scale-terms x 1 (term-list p))))

  (define (scale-terms n d L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t (first-term L)))
          (adjoin-term (make-term (order t)
                                  (div (mul (coeff t) n)
                                       d))
                       (scale-terms n d (rest-terms L))))))

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
        (if (variable<? (variable p1) (variable p2))
            (add-poly (change-variable (variable p2) p1)
                      p2)
            (add-poly p1
                      (change-variable (variable p1) p2)))))

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
        (if (variable<? (variable p1) (variable p2))
            (mul-poly (change-variable (variable p2) p1)
                      p2)
            (mul-poly p1
                      (change-variable (variable p1) p2)))))

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

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- GCD-POLY" (list p1 p2))))

  (define (gcd-terms L1 L2)
    (if (empty-termlist? L2)
        L1
        (let ((result (gcd-terms L2 (pseudoremainder-terms L1 L2))))
          (scale-terms 1 (gcd-coefs result) result))))

  (define (remainder-terms L1 L2)
    (cadr (div-terms L1 L2)))

  (define (pseudoremainder-terms L1 L2)
    (remainder-terms
      (scale-terms
        (integerizing-factor
          (order (first-term L1))
          L2)
        1 L1)
      L2))

  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((result (reduce-terms (term-list p1)
                                    (term-list p2))))
          (list (make-poly (variable p1)
                           (car result))
                (make-poly (variable p1)
                           (cadr result))))
        (error "Polys not in same var -- REDUCE-POLY" (list p1 p2))))

  (define (reduce-terms N D)
    (let ((divisor (gcd-terms N D)))
      (let ((factor (integerizing-factor (max (order (first-term N))
                                              (order (first-term D)))
                                         divisor)))
        (let ((NN (car (div-terms (scale-terms factor 1 N) divisor)))
              (DD (car (div-terms (scale-terms factor 1 D) divisor))))
          (let ((common (gcd (gcd-coefs NN) (gcd-coefs DD))))
            (list (scale-terms 1 common NN)
                  (scale-terms 1 common DD)))))))

  (define (integerizing-factor o1 L2)
    (if (< o1 (order (first-term L2)))
        1
        (pow (coeff (first-term L2))
             (+ 1 (- o1 (order (first-term L2)))))))

  (define (gcd-coefs L)
    (define (iter acc terms)
      (if (empty-termlist? terms)
          acc
          (iter (gcd acc
                     (coeff (first-term terms)))
                (rest-terms terms))))
    (iter 0 L))

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
  (define (tag-list l) (map tag l))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'mul '(polynomial scheme-number) (lambda (p1 x) (tag (scale-poly x p1))))
  (put 'mul '(scheme-number polynomial) (lambda (x p1) (tag (scale-poly x p1))))
  (put 'div '(polynomial polynomial) (lambda (p1 p2) (tag-list (div-poly p1 p2))))
  (put 'neg '(polynomial) (lambda (p) (tag (neg-poly p))))
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'greatest-common-divisor '(polynomial polynomial)
    (lambda (x y) (tag (gcd-poly x y))))
  (put 'reduce '(polynomial polynomial) (lambda (n d) (tag-list (reduce-poly n d))))
  (put 'print '(polynomial) print-poly)
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))

  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cadr x))
  (define (make-rat n d)
    (reduce n d))

  (define (print-rat r)
    (display "[")
    (print (numer r))
    (display " / ")
    (print (denom r))
    (display "]"))

  (define (raise x) (make-scheme-number (/ (numer x) (denom x))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (equ-rat? a b)
    (and (equ? (numer a) (numer b))
         (equ? (denom a) (denom b))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'raise '(rational) raise)
  (put 'project '(rational) (lambda (x) (round (raise x))))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) (lambda (a b) (equ-rat? a b)))
  (put '=zero? '(rational) (lambda (x) (= (numer x) 0)))
  (put 'print '(rational) print-rat)
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-polynomial-package)
(install-rational-package)

(define p1 (make-polynomial 'x '((1 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))

(print p1) (newline)
(print p2) (newline)
(reduce p1 p2)

(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

(print (add rf1 rf2)) (newline)

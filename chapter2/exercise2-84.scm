#!/usr/bin/env -S guile -s
!#

;; prerequisites

(define square sqr)

(define (attach-tag type-tag contents)
  (cond ((and (eq? type-tag 'scheme-integer)
              (exact? contents))
         contents)
        ((and (eq? type-tag 'scheme-real)
              (number? contents))
         contents)
        (else (cons type-tag contents))))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((exact? datum) 'scheme-integer)
        ((number? datum) 'scheme-real)
        (else (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: CONTENTS" datum))))

(define (type-distance x y)
  (define (iter a b dist)
    (cond ((eq? (type-tag a) (type-tag b)) dist)
          ((get 'raise (list (type-tag a))) (iter (raise a) b (+ dist 1)))
          ((get 'raise (list (type-tag b))) (iter a (raise b) (- dist 1)))
          (else (error "incompatible types:" x y))))
  (iter x y 0))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
            (let ((a1 (car args))
                  (a2 (cadr args)))
              (let ((dist (type-distance a1 a2)))
                (cond ((> dist 0) (apply-generic op (raise a1) a2))
                      ((< dist 0) (apply-generic op a1 (raise a2)))
                      (else (error "No method for these types: APPLY-GENERIC"
                                   (list op type-tags))))))
            (error "No method for these types: APPLY-GENERIC"
                   (list op type-tags)))))))



;; =================


(define (raise x) (apply-generic 'raise x))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (add5 a b c d e) (apply-generic 'add5 a b c d e))

(define (equ? a b) (apply-generic 'equ? a b))
(define (=zero? x) (apply-generic '=zero? x))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (exp x y) (apply-generic 'exp x y))


(define (install-scheme-number-package)
  ;;
  (define (raise x)
    (cond ((exact? x) (make-rational x 1))
          ((number? x) (make-complex-from-real-imag x 0))
          (else (error "invalid scheme-number" x))))
  ;;
  (define (tag x) (attach-tag 'scheme-integer x))
  (put 'raise '(scheme-integer) raise)
  (put 'add '(scheme-integer scheme-integer) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-integer scheme-integer) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-integer scheme-integer) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-integer scheme-integer) (lambda (x y) (tag (/ x y))))
  (put 'exp '(scheme-integer scheme-integer) (lambda (x y) (tag (expt x y))))
  (put 'equ? '(scheme-integer scheme-integer) =)
  (put '=zero? '(scheme-integer) (lambda (x) (= x 0)))
  (put 'make 'scheme-integer (lambda (x) (tag x)))

  (define (tag x) (attach-tag 'scheme-real x))
  (put 'raise '(scheme-real) raise)
  (put 'add '(scheme-real scheme-real) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-real scheme-real) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-real scheme-real) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-real scheme-real) (lambda (x y) (tag (/ x y))))
  (put 'exp '(scheme-real scheme-real) (lambda (x y) (tag (expt x y))))
  (put 'equ? '(scheme-real scheme-real) =)
  (put '=zero? '(scheme-real) (lambda (x) (= x 0)))
  (put 'make 'scheme-real (lambda (x) (tag x)))

  'done)

(define (make-scheme-number n)
  (cond ((exact? n) ((get 'make 'scheme-integer) n))
        ((number? n) ((get 'make 'scheme-real) n))
        (else (error "invalid number:" n))))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (raise x) (make-scheme-number (/ (numer x) (denom x))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ-rat? a b)
    (and (= (numer a) (numer b))
         (= (denom a) (denom b))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'raise '(rational) raise)
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) (lambda (a b) (equ-rat? a b)))
  (put '=zero? '(rational) (lambda (x) (= (numer x) 0)))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z) (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))
  (define (equ? a b)
    (and (= (real-part a) (real-part b))
         (= (imag-part a) (imag-part b))))
  (define (=zero? z)
    (and (= (real-part z) 0)
         (= (imag-part z) 0)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'equ? '(rectangular rectangular) equ?)
  (put '=zero? '(rectangular) =zero?)
  (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))
  (define (equ? a b)
    (and (= (magnitude a) (magnitude b))
         (= (angle a) (angle b))))
  (define (=zero? z)
    (= (magnitude z) 0))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'equ? '(polar polar) equ?)
  (put '=zero? '(polar) =zero?)
  (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
   (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                      (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ-complex? a b)
    (and (= (real-part a) (real-part b))
         (= (imag-part a) (imag-part b))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'complex x))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put '=zero? '(complex) =zero?)
  (put 'equ? '(complex complex) equ-complex?)
  (put 'add5 '(complex complex complex complex complex) (lambda (a b c d e) (tag (add-complex a (add-complex b (add-complex c (add-complex d e)))))))
  (put 'add '(complex complex) (lambda (x y) (tag (add-complex x y))))
  (put 'sub '(complex complex) (lambda (x y) (tag (sub-complex x y))))
  (put 'mul '(complex complex) (lambda (x y) (mul (add-complex x y))))
  (put 'div '(complex complex) (lambda (x y) (div (add-complex x y))))
  (put 'make-from-real-imag 'complex (lambda (r i) (tag (make-from-real-imag r i))))
  (put 'make-from-mag-ang 'complex (lambda (m a) (tag (make-from-mag-ang m a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)


(define n1 (make-scheme-number 0))
(define n2 (make-scheme-number 1))

(define r1 (make-rational 0 5))
(define r2 (make-rational 5 0))
(define r3 (make-rational 0 0))

(define z1 (make-complex-from-real-imag 3 4))
(define z2 (make-complex-from-real-imag 0 0))
(define z3 (make-complex-from-mag-ang 5 0))
(define z4 (make-complex-from-mag-ang 0 5))

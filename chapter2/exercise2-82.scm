(import (builtin core)
        (sicp utils))
        
;; prerequisites

(define square sqr)

(define (attach-tag type-tag contents)
  (cond ((and (eq? type-tag 'scheme-number)
              (number? contents))
         contents)
        (else (cons type-tag contents))))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: CONTENTS" datum))))


; This (not very efficient) implementation of apply-generic tries to coerce
; multiple arguments by trying to coerce all arguments to each one in sequence.
; That procedure will fail in certain cases:
;    - Assume we have two types that can be coerced A -> B.
;    - Further, we have a procedure that takes arguments of types B A B.
;    - If we pass arguments of types B A A we would need to coerce only the last
;      one from A to B but not the second one. This is a combination that this
;      procedure will never try because it coerces all arguments to B.

(define (apply-generic op . args)
  (define (try-apply args remaining-coercions)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (if (null? remaining-coercions)
                (error "No method for these types: APPLY-GENERIC" (list op type-tags))
                (let ((coercions (map (lambda (t) (get-coercion t (car remaining-coercions))) type-tags)))
                  (println coercions args)
                  (if (accumulate (lambda (a b) (and a b)) #t coercions)  ; all coercions possible?
                      (try-apply (map (lambda (coerce a) (coerce a)) coercions args)
                                 (cdr remaining-coercions))
                      (try-apply args
                                 (cdr remaining-coercions)))))))))
  (try-apply args (map type-tag args)))



;; =================


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
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add5 '(scheme-number scheme-number scheme-number scheme-number scheme-number) +)
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'exp '(scheme-number scheme-number) (lambda (x y) (tag (expt x y))))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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


; coercions

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

; my version of apply-generic relies on self-coercion

(define (scheme-number->scheme-number n) n)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)

(define (complex->complex z) z)
(put-coercion 'complex 'complex complex->complex)


(define n1 (make-scheme-number 0))
(define n2 (make-scheme-number 1))

(define r1 (make-rational 0 5))
(define r2 (make-rational 5 0))
(define r3 (make-rational 0 0))

(define z1 (make-complex-from-real-imag 3 4))
(define z2 (make-complex-from-real-imag 0 0))
(define z3 (make-complex-from-mag-ang 5 0))
(define z4 (make-complex-from-mag-ang 0 5))

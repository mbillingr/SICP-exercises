(import (builtin core)
        (sicp utils))

(include "chapter4-lazy.scm")

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
           procedure
           (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameter-names procedure)
             (list-of-processed-args arguments
                                     (procedure-parameters procedure)
                                     env)
             (procedure-environment procedure))))
        (else (error "Unknown procedure type -- APPLY" procedure))))


(define (strict-param? param) (symbol? param))
(define (param-mode param) (cadr param))
(define (param-name param)
  (if (strict-param? param)
      param
      (car param)))

(define (procedure-parameter-names p)
  (map param-name (procedure-parameters p)))

(define (list-of-processed-args exps params env)
  (if (no-operands? exps)
      '()
      (cons (if (strict-param? (first-operand params))
                (actual-value (first-operand exps) env)
                (delay-it (first-operand exps)
                          (param-mode (first-operand params))
                          env))
            (list-of-processed-args (rest-operands exps)
                                    (rest-operands params)
                                    env))))
(define (delay-it exp mode env)
  (cond ((eq? mode 'lazy)
         (list 'thunk exp env))
        ((eq? mode 'lazy-memo)
         (list 'memoized-thunk exp env))
        (else (error "Invalid parameter mode" mode))))

(define (memoized-thunk? obj)
  (tagged-list? obj 'memoized-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (actual-value (thunk-exp obj) (thunk-env obj)))
        ((memoized-thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)   ; replace exp with its value
           (set-cdr! (cdr obj) '())  ; drop unneeded env
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

(eval '(define (sqr x) (* x x)) the-global-environment)
(eval '(define (sqr-lazy (x lazy)) (* x x)) the-global-environment)
(eval '(define (sqr-memo (x lazy-memo)) (* x x)) the-global-environment)

(eval '(define (val) (println "evaluating (val)") 42) the-global-environment)

(println "(sqr (val))")
(eval '(sqr (val)) the-global-environment)
(newline)
(println "(sqr-lazy (val))")
(eval '(sqr-lazy (val)) the-global-environment)
(newline)
(println "(sqr-memo (val))")
(eval '(sqr-memo (val)) the-global-environment)
(newline)

(driver-loop)

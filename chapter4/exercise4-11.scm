(import (builtin core)
        (sicp utils))

(include "chapter4-core.scm")

(define env
  (extend-environment
    '(z)
    '(123)
    (extend-environment
      '(x y)
      '(12 42)
      (list the-empty-environment))))

(println (lookup-variable-value 'x env))
(println (lookup-variable-value 'y env))
(println (lookup-variable-value 'z env))

(set-variable-value! 'x 666 env)
(println (lookup-variable-value 'x env))
(println (lookup-variable-value 'y env))
(println (lookup-variable-value 'z env))

(define-variable! 'pi -3.1415 env)
(println env)

(define (make-frame variables values)
  (if (null? variables)
      '()
      (cons (cons (car variables) (car values))
            (make-frame (cdr variables) (cdr values)))))

(define (add-binding-to-frame! var val frame)
  (if (null? (cdr frame))
      (set-cdr! frame (cons (cons var val)
                            '()))
      (add-bindings-to-frame! var val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar bindings)) (cdar bindings))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (first-frame env))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar bindings)) (set-cdr! (car bindings) val))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (scan (first-frame env))))
  (env-loop env))

(define (define-variable! var val env)
  (define (scan bindings)
    (cond ((null? bindings)
           (add-binding-to-frame! var val (first-frame env)))
          ((eq? var (caar bindings)) (set-cdr! (car bindings) val))
          (else (scan (cdr bindings)))))
  (scan (first-frame env)))

(define env
  (extend-environment
    '(z)
    '(123)
    (extend-environment
      '(x y)
      '(12 42)
      (list the-empty-environment))))

(println (lookup-variable-value 'x env))
(println (lookup-variable-value 'y env))
(println (lookup-variable-value 'z env))

(set-variable-value! 'x 666 env)
(println (lookup-variable-value 'x env))
(println (lookup-variable-value 'y env))
(println (lookup-variable-value 'z env))

(define-variable! 'pi -3.1415 env)
(println env)

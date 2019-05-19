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

(define (scan-frame var frame not-found-action found-action)
  (define (scan vars vals)
    (cond ((null? vars) (not-found-action))
          ((eq? var (car vars)) (found-action vals))
          (else (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables frame)
        (frame-values frame)))

(define (scan-env var env not-found-action found-action)
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (scan-frame var
                  (first-frame env)
                  not-found-action
                  found-action)))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan-frame var
                frame
                (lambda () (add-binding-to-frame! var val frame))
                (lambda (vals) (set-car! vals val)))))

(define (set-variable-value! var val env)
  (scan-env var
            env
            (lambda () (set-variable-value! var val (enclosing-environment env)))
            (lambda (vals) (set-car! vals val))))

(define (lookup-variable-value var env)
  (scan-env var
            env
            (lambda () (lookup-variable-value var (enclosing-environment env)))
            (lambda (vals) (car vals))))

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

(import (builtin core)
        (sicp utils))

(include "chapter4-core.scm")

; syntax
;   assignment: (<var> <- <expr>)
;   definition: (<var> := <exp>)
;               (def <name> (<args>) <body>)
; the remaining syntax is equivalent to scheme

(define (assignment? exp) (eq? (cadr exp) '<-))
(define (assignment-variable exp) (car exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (or (proc-definition? exp)
      (var-definition? exp)))
(define (var-definition? exp)
  (eq? (cadr exp) ':=))
(define (proc-definition? exp)
  (tagged-list? exp 'def))
(define (definition-variable exp)
  (if (var-definition? exp)
      (car exp)
      (cadr exp)))
(define (definition-value exp)
  (if (var-definition? exp)
      (caddr exp)
      (make-lambda (caddr exp)
                   (cdddr exp))))

(define (set-variable-value! var val env)
  (println "set" var "to" val "in" env))

(define (define-variable! var val env)
  (println "bind" var "to" val "in" env))

(define (make-procedure args body env)
  (cons 'lambda (cons args body)))

(eval '(x := 0) "<?>")
(eval '(x <- 42) "<?>")
(eval '(def sqr (x) (* x x)) "<?>")

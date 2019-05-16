(import (builtin core)
        (sicp utils))

; auxillary implementations

(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)

(define (eval exp env)
  (println "Evaluating" exp "in" env))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


(list-of-values '(1 2 3) '())

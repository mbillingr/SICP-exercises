(import (builtin core)
        (sicp utils))

; auxillary implementations

(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)

(define (eval exp env)
  (println "Evaluating" exp "in" env)
  exp)

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(println (list-of-values '(1 2 3) '()))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env)))
        (cons left
              (list-of-values (rest-operands exps) env)))))

(println (list-of-values '(1 2 3) '()))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              right))))

(println (list-of-values '(1 2 3) '()))

; I verified that this works correctly by reversing the evaluation order
; in the native interpreter:

;     let args = (*cdr).map_list(|a| eval(a, env.clone()))?;
; was replaced with
;     let args: Vec<_> = cdr.iter_list().collect::<Result<_>>()?;
;     let args: Vec<_> = args.into_iter().rev().map(|a| eval(a, env.clone())).collect::<Result<_>>()?;
;     let args = args.into_iter().rev().collect()));

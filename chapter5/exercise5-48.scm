(import (sicp utils))

(include "chapter5-ec-machine2.scm")

; Primitive-apply in the EC machine expects the continue label on the stack.
; In order to call the compiled code, compile-and-run pushes the compiled
; instructions on the stack so the evaluator will "return" there after this
; function finished.
; Additionally, the compiled code is augmented to restore the original continue
; and jump there. Now execution continues where it would normally have gone
; after primitive-apply.
(define (compile-and-run expression)
  (let ((stack (eceval 'stack))
        (instructions
          (assemble (statements (append-instruction-sequences
                                  (compile expression 'val 'next)
                                  (make-instruction-sequence '() '()
                                    '((restore continue)
                                      (goto (reg continue))))))
                    eceval)))
    (push stack instructions)
    'ok))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names primitive-procedures)
                              (primitive-procedure-objects primitive-procedures)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    (define-variable! 'compile-and-run (list 'primitive compile-and-run) initial-env)
    initial-env))

(compile-and-go ''ready)

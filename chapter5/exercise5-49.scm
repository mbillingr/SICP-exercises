(import (sicp utils))


(include "chapter5-compiler.scm"
         "chapter5-vm.scm"
         "chapter5-assembler.scm")

(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((compiled-procedure? object)
         (display '<compiled-procedure>))
        (else (display object))))

(include "eceval-operations.scm")

; The assembler needs the apply procedure.
; It would be cleaner to use libraries rather than include files to avoid such
; name conflicts. However, exercises often require modifications of individual
; functions, which is made convenient by include files...
(define apply apply-in-underlying-scheme)

(define (compile-expression exp)
  (statements (compile exp 'val 'return)))

(define (assemble-expression exp)
  (assemble exp eceval))

(define eceval-operations
  (append eceval-operations
          (list (list 'compile compile-expression)
                (list 'assemble assemble-expression))))

(define eceval
  (make-machine
    '(exp env val continue proc argl unev)
    eceval-operations
    '(
      read-compile-execute-loop
        (perform (op initialize-stack))
        (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
        (assign exp (op read))
        (assign env (op get-global-environment))
        (assign continue (label print-result))
        (goto (label compile))
      print-result
        (perform (op print-stack-statistics))
        (perform (op announce-output) (const ";;; EC-Eval value:"))
        (perform (op user-print) (reg val))
        (goto (label read-compile-execute-loop))

      unknown-expression-type
        (assign val (const unknown-expression-type-error))
        (goto (label signal-error))
      unknown-procedure-type
        (restore continue)  ; clean up stack (from apply-dispatch)
        (assign val (const unknown-procedure-type-error))
        (goto (label signal-error))
      signal-error
        (perform (op user-print) (reg val))
        (goto (label read-compile-execute-loop))

      compile
        (assign exp (op compile) (reg exp))
        (assign exp (op assemble) (reg exp))
        (goto (reg exp))
      ec-done)))

(define the-global-environment (setup-environment))

(start eceval)

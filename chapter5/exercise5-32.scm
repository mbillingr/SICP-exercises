(import (sicp utils))

(include "../chapter4/chapter4-core.scm"
         "chapter5-vm.scm"
         "chapter5-assembler.scm")

; The assembler needs the apply procedure.
; It would be cleaner to use libraries rather than include files to avoid such
; name conflicts. However, exercises often require modifications of individual
; functions, which is made convenient by include files...
(define apply apply-in-underlying-scheme)

(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))

(define (get-global-environment) the-global-environment)

(define (let? exp) (tagged-list? exp 'let))
(define (let-spec exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-vars spec)
  (if (null? spec)
      '()
      (cons (caar spec) (let-vars (cdr spec)))))
(define (let-values spec)
  (if (null? spec)
      '()
      (cons (cadar spec) (let-values (cdr spec)))))
(define (let->combination exp)
  (cons (make-lambda (let-vars (let-spec exp))
                     (let-body exp))
        (let-values (let-spec exp))))

(define eceval-operations
  (list (list 'adjoin-arg adjoin-arg)
        (list 'announce-output announce-output)
        (list 'application? application?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'assignment? assignment?)
        (list 'assignment-value assignment-value)
        (list 'assignment-variable assignment-variable)
        (list 'begin? begin?)
        (list 'begin-actions begin-actions)
        (list 'compound-procedure? compound-procedure?)
        (list 'cond? cond?)
        (list 'cond->if cond->if)
        (list 'define-variable! define-variable!)
        (list 'definition? definition?)
        (list 'definition-value definition-value)
        (list 'definition-variable definition-variable)
        (list 'empty-arglist empty-arglist)
        (list 'extend-environment extend-environment)
        (list 'first-exp first-exp)
        (list 'first-operand first-operand)
        (list 'get-global-environment get-global-environment)
        (list 'if? if?)
        (list 'if-alternative if-alternative)
        (list 'if-consequent if-consequent)
        (list 'if-predicate if-predicate)
        (list 'lambda? lambda?)
        (list 'lambda-body lambda-body)
        (list 'lambda-parameters lambda-parameters)
        (list 'last-exp? last-exp?)
        (list 'last-operand? last-operand?)
        (list 'let? let?)
        (list 'let->combination let->combination)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'make-procedure make-procedure)
        (list 'no-operands? no-operands?)
        (list 'operands operands)
        (list 'operator operator)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'procedure-body procedure-body)
        (list 'procedure-environment procedure-environment)
        (list 'procedure-parameters procedure-parameters)
        (list 'prompt-for-input prompt-for-input)
        (list 'quoted? quoted?)
        (list 'read read)
        (list 'rest-exps rest-exps)
        (list 'rest-operands rest-operands)
        (list 'self-evaluating? self-evaluating?)
        (list 'set-variable-value! set-variable-value!)
        (list 'text-of-quotation text-of-quotation)
        (list 'true? true?)
        (list 'user-print user-print)
        (list 'variable? variable?)))

(define eceval
  (make-machine
    '(exp env val continue proc argl unev)
    eceval-operations
    '(read-eval-print-loop
        (perform (op initialize-stack))
        (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
        (assign exp (op read))
        (assign env (op get-global-environment))
        (assign continue (label print-result))
        (goto (label eval-dispatch))
      print-result
        (perform (op print-stack-statistics))
        (perform (op announce-output) (const ";;; EC-Eval value:"))
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))

      unknown-expression-type
        (assign val (const unknown-expression-type-error))
        (goto (label signal-error))
      unknown-procedure-type
        (restore continue)  ; clean up stack (from apply-dispatch)
        (assign val (const unknown-procedure-type-error))
        (goto (label signal-error))
      signal-error
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))

      eval-dispatch
        (test (op self-evaluating?) (reg exp))
        (branch (label ev-self-eval))
        (test (op variable?) (reg exp))
        (branch (label ev-variable))
        (test (op quoted?) (reg exp))
        (branch (label ev-quoted))
        (test (op assignment?) (reg exp))
        (branch (label ev-assignment))
        (test (op definition?) (reg exp))
        (branch (label ev-definition))
        (test (op if?) (reg exp))
        (branch (label ev-if))
        (test (op lambda?) (reg exp))
        (branch (label ev-lambda))
        (test (op begin?) (reg exp))
        (branch (label ev-begin))
        (test (op cond?) (reg exp))
        (branch (label ev-cond))
        (test (op let?) (reg exp))
        (branch (label ev-let))
        (test (op application?) (reg exp))
        (branch (label ev-application))
        (goto (label unknown-expression-type))

      ev-self-eval
        (assign val (reg exp))
        (goto (reg continue))

      ev-variable
        (assign val (op lookup-variable-value) (reg exp) (reg env))
        (goto (reg continue))

      ev-quoted
        (assign val (op text-of-quotation) (reg exp))
        (goto (reg continue))

      ev-lambda
        (assign unev (op lambda-parameters) (reg exp))
        (assign exp (op lambda-body) (reg exp))
        (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
        (goto (reg continue))

      ev-application
        (save continue)
        (assign unev (op operands) (reg exp))
        (assign exp (op operator) (reg exp))
        (test (op variable?) (reg exp))
        (branch (label ev-appl-simple-operator))
        (save unev)
        (save env)
        (assign continue (label ev-appl-did-operator))
        (goto (label eval-dispatch))
      ev-appl-did-operator
        (restore env)
        (restore unev)
        (assign proc (reg val))
        (goto (label ev-appl-proc-ready))
      ev-appl-simple-operator
        (assign proc (op lookup-variable-value) (reg exp) (reg env))
      ev-appl-proc-ready
        (assign argl (op empty-arglist))
        (test (op no-operands?) (reg unev))
        (branch (label apply-dispatch))
        (save proc)
      ev-appl-operand-loop
        (save argl)
        (assign exp (op first-operand) (reg unev))
        (test (op last-operand?) (reg unev))
        (branch (label ev-appl-last-arg))
        (save env)
        (save unev)
        (assign continue (label ev-appl-accumulate-arg))
        (goto (label eval-dispatch))
      ev-appl-accumulate-arg
        (restore unev)
        (restore env)
        (restore argl)
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (assign unev (op rest-operands) (reg unev))
        (goto (label ev-appl-operand-loop))
      ev-appl-last-arg
        (assign continue (label ev-appl-accum-last-arg))
        (goto (label eval-dispatch))
      ev-appl-accum-last-arg
        (restore argl)
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (restore proc)
        (goto (label apply-dispatch))

      apply-dispatch
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-apply))
        (test (op compound-procedure?) (reg proc))
        (branch (label compound-apply))
        (goto (label unknown-procedure-type))
      primitive-apply
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        (restore continue)
        (goto (reg continue))
      compound-apply
        (assign unev (op procedure-parameters) (reg proc))
        (assign env (op procedure-environment) (reg proc))
        (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
        (assign unev (op procedure-body) (reg proc))
        (goto (label ev-sequence))

      ev-begin
        (assign unev (op begin-actions) (reg exp))
        (save continue)
        (goto (label ev-sequence))

      ev-sequence
        (assign exp (op first-exp) (reg unev))
        (test (op last-exp?) (reg unev))
        (branch (label ev-sequence-last-exp))
        (save unev)
        (save env)
        (assign continue (label ev-sequence-continue))
        (goto (label eval-dispatch))
      ev-sequence-continue
        (restore env)
        (restore unev)
        (assign unev (op rest-exps) (reg unev))
        (goto (label ev-sequence))
      ev-sequence-last-exp
        (restore continue)
        (goto (label eval-dispatch))

      ev-if
        (save exp)
        (save env)
        (save continue)
        (assign continue (label ev-if-decide))
        (assign exp (op if-predicate) (reg exp))
        (goto (label eval-dispatch))
      ev-if-decide
        (restore continue)
        (restore env)
        (restore exp)
        (test (op true?) (reg val))
        (branch (label ev-if-consequent))
      ev-if-alternative
        (assign exp (op if-alternative) (reg exp))
        (goto (label eval-dispatch))
      ev-if-consequent
        (assign exp (op if-consequent) (reg exp))
        (goto (label eval-dispatch))

      ev-assignment
        (assign unev (op assignment-variable) (reg exp))
        (save unev)
        (assign exp (op assignment-value) (reg exp))
        (save env)
        (save continue)
        (assign continue (label ev-assignment-1))
        (goto (label eval-dispatch))
      ev-assignment-1
        (restore continue)
        (restore env)
        (restore unev)
        (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
        (assign val (const ok))
        (goto (reg continue))

      ev-definition
        (assign unev (op definition-variable) (reg exp))
        (save unev)
        (assign exp (op definition-value) (reg exp))
        (save env)
        (save continue)
        (assign continue (label ev-definition-1))
        (goto (label eval-dispatch))
      ev-definition-1
        (restore continue)
        (restore env)
        (restore unev)
        (perform (op define-variable!) (reg unev) (reg val) (reg env))
        (assign val (const ok))
        (goto (reg continue))

      ev-cond
        (assign exp (op cond->if) (reg exp))
        (goto (label eval-dispatch))
      ev-let
        (assign exp (op let->combination) (reg exp))
        (goto (label eval-dispatch))
      ec-done)))

(define the-global-environment (setup-environment))

(start eceval)

; b) Checking for special cases also takes time. Building more and more
;    of them into the evaluator would defeat any gains of the optimization.
;    In the end the evaluator would perform the same function as the
;    compiler. However, where the compiler runs once the evaluator would
;    do the work repeatedly (at each evaluation) during runtime.

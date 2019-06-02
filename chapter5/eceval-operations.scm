
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

(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define eceval-operations
  (list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'adjoin-arg adjoin-arg)
        (list 'announce-output announce-output)
        (list 'application? application?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'assignment? assignment?)
        (list 'assignment-value assignment-value)
        (list 'assignment-variable assignment-variable)
        (list 'begin? begin?)
        (list 'begin-actions begin-actions)
        (list 'compound-procedure? compound-procedure?)
        (list 'car car)
        (list 'cdr cdr)
        (list 'compiled-procedure-entry compiled-procedure-entry)
        (list 'compiled-procedure-env compiled-procedure-env)
        (list 'cond? cond?)
        (list 'cond->if cond->if)
        (list 'cons cons)
        (list 'define-variable! define-variable!)
        (list 'definition? definition?)
        (list 'definition-value definition-value)
        (list 'definition-variable definition-variable)
        (list 'empty-arglist empty-arglist)
        (list 'extend-environment extend-environment)
        (list 'false? false?)
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
        (list 'list list)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'make-compiled-procedure make-compiled-procedure)
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
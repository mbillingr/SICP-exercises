
(include "chapter4-core.scm")

; TODO: do we need to implement scanning out internal definitions?

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

; simple interface to ambeval. intended to define procedures during exercises.
(define (eval exp env)
  (ambeval exp
           env
           (lambda (val success) val)
           (lambda () 'undef)))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-mainexpr exp) (cadr exp))
(define (if-fail-alternative exp) (caddr exp))
(define (analyze-if-fail exp)
  (let ((mproc (analyze (if-fail-mainexpr exp)))
        (fproc (analyze (if-fail-alternative exp))))
    (lambda (env succeed fail)
      (mproc env
             (lambda (val fail2) (succeed val fail2))
             (lambda ()
               (fproc env
                      (lambda (fval fail3) (succeed fval fail3))
                      fail))))))
      ;(mproc env))))
             ;succeed))))
             ;fail))))
             ;(lambda (env succeed fail) (println "xxx")(succeed 5 fail))
             ;(lambda () (println "fff") 'fff)))))
               ;(fproc env succeed fail))))))

(define (permanent-assignment? exp) (tagged-list? exp 'permanent-set!))
(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail) (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail) (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain a pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; succees continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var old-value env)
                            (fail2)))))
             fail))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                             proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    ;; success continuation for this aproc
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                ;; success continuation for recursive
                                ;; call to get-args
                                (lambda (args fail3)
                                  (succeed (cons arg args) fail3))
                                fail2))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else (error "Unknown procedure type: EXECUTE-APPLICATION" proc))))

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

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (cond ((null? choices) (fail))
              ((null? (cdr choices))
               ((car choices) env succeed (lambda () (try-next '()))))
              (else
                (let ((next 'undef) (rest 'undef))
                  (cond ((= 0 (random 2))
                         (set! next (car choices))
                         (set! rest (cdr choices)))
                        (else
                         (set! next (cadr choices))
                         (set! rest (cons (car choices)
                                          (cddr choices)))))
                  (next env succeed (lambda () (try-next rest)))))))
      (try-next cprocs))))

(define input-prompt ";;; Amp-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
    (lambda ()
      (newline)
      (display ";;; There is no current problem")
      (driver-loop))))

(define (all-solutions exp env)
  (define n 0)
  (for-each-solution
    (lambda (val)
      (set! n (+ n 1))
      (user-print val) (newline))
    exp
    env)
  (if (= n 1)
      (println "1 solution")
      (println n "solutions")))

(define (for-each-solution proc exp env)
  (ambeval exp
           env
           (lambda (val next-alternative)
             (proc val)
             (next-alternative))
           (lambda () 'done)))

(define (n-solutions n exp env)
  (ambeval exp
           env
           (lambda (val next-alternative)
             (user-print val) (newline)
             (set! n (- n 1))
             (if (> n 0)
                 (next-alternative)))
           (lambda () 'ok)))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names primitive-procedures)
                              (primitive-procedure-objects primitive-procedures)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)

    (eval '(define (require p) (if (not p) (amb))) initial-env)

    initial-env))

(define the-global-environment (setup-environment))

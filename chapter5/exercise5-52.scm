(import (sicp utils))

(include "../chapter4/chapter4-core.scm")

(define (compile exp target linkage c-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp)
         (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage c-env))
        ((assignment? exp)
         (compile-assignment exp target linkage c-env))
        ((definition? exp)
         (compile-definition exp target linkage c-env))
        ((if? exp)
         (compile-if exp target linkage c-env))
        ((lambda? exp)
         (compile-lambda exp target linkage c-env))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage c-env))
        ((let? exp)
         (compile (let->combination exp) target linkage c-env))
        ((cond? exp)
         (compile (cond->if exp) target linkage c-env))
        ((application? exp)
         (compile-application exp target linkage c-env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define the-symbols '())
(define (symbol-index s)
  (define (loop i symbols)
    (cond ((eq? (car symbols) s)
           i)
          ((null? (cdr symbols))
           (set-cdr! symbols (cons s '()))
           (+ i 1))
          (else
            (loop (+ i 1) (cdr symbols)))))
  (if (null? the-symbols)
      (begin
        (set! the-symbols (list s))
        0)
      (loop 0 the-symbols)))

(define (compile-initialize-symbols)
  (define (iter rest output)
    (if (null? rest)
        output
        (iter (cdr rest)
              (string-append output
                             "symbol(\""
                             (symbol->string (car rest))
                             "\"),\n"))))
  (list (string-append "Object symbols[] = {\n"
                       (iter the-symbols "")
                       "};")))

(define (make-constant exp)
  (cond ((null? exp) "nil()")
        ((number? exp)
         (string-append "number(" (number->string exp) ")"))
        ((symbol? exp)
         (string-append "symbols["
                        (number->string (symbol-index exp))
                        "]"))
        ((string? exp)
         (string-append "string(\""
                        exp
                        "\")"))
        ((pair? exp)
         (string-append "cons("
                        (make-constant (car exp))
                        ", "
                        (make-constant (cdr exp))
                        ")"))
        (else (error "invalid constant" exp))))

(define (make-numeric-constant exp)
  (if (number? exp)
      (number->string exp)
      (error "invalid numeric constant:" exp)))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (make-lexical-address f d) (cons f d))
(define (frame-number laddr) (car laddr))
(define (displacement-number laddr) (cdr laddr))

(define (extend-compiletime-env formals c-env)
  (cons formals c-env))

(define (find-variable var c-env)
  (define (env-loop n env)
    (define (scan i vars)
      (cond ((null? vars) (env-loop (+ n 1) (cdr env)))
            ((eq? var (car vars))
             (make-lexical-address n i))
            (else (scan (+ i 1) (cdr vars)))))
    (if (null? env)
        'not-found
        (scan 0 (car env))))
  (env-loop 0 c-env))

(define all-regs '(env proc val argl cont))

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))

(define (reference-label name)
  (string-append "label(&&" (symbol->string name) ")"))

(define (label-instruction name)
  (string-append (symbol->string name) ":"))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(cont) '() '("goto *c_label(cont);")))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
          (make-instruction-sequence '() '()
            `(,(string-append "goto " (symbol->string linkage) ";"))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(cont)
              instruction-sequence
              (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
    (make-instruction-sequence '() (list target)
      `(,(string-append (symbol->string target)
                       " = "
                       (make-constant exp)
                       ";")))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
    (make-instruction-sequence '() (list target)
      ;`((assign ,target (const ,(text-of-quotation exp))))
      `(,(string-append (symbol->string target)
                       " = "
                       (make-constant (text-of-quotation exp))
                       ";")))))

(define (compile-variable exp target linkage c-env)
  (let ((l-addr (find-variable exp c-env)))
    (if (eq? l-addr 'not-found)
        (end-with-linkage linkage
          (make-instruction-sequence '() (list target 'env)
            `("env = get_global_environment();"
              ,(string-append (symbol->string target)
                             " = lookup_variable_value("
                             (make-constant exp)
                             ", env);"))))

        (end-with-linkage linkage
          (make-instruction-sequence '(env) (list target)
            `(,(string-append (symbol->string target)
                           " = lexical_address_lookup("
                           (make-numeric-constant (frame-number l-addr))
                           ", "
                           (make-numeric-constant (displacement-number l-addr))
                           ", env);")))))))

(define (compile-assignment exp target linkage c-env)
  (let ((var (assignment-variable exp))
        (get-value-code
          (compile (assignment-value exp) 'val 'next c-env)))
    (let ((l-addr (find-variable var c-env)))
      (if (eq? l-addr 'not-found)
          (end-with-linkage linkage
            (append-instruction-sequences
              get-value-code
              (make-instruction-sequence '(val) (list target 'env)
                `("env = get_global_environment();"
                  ,(string-append "set_variable_value("
                                 (make-constant var)
                                 ", val, env);")
                  ,(string-append (symbol->string target)
                                 " = "
                                 (make-constant 'ok)
                                 ";")))))
          (end-with-linkage linkage
            (preserving '(env)
              get-value-code
              (make-instruction-sequence '(env val) (list target)
                `(,(string-append "lexical_address_set("
                                 (make-numeric-constant (frame-number l-addr))
                                 ", "
                                 (make-numeric-constant (displacement-number l-addr))
                                 ", val, env);")
                  ,(string-append (symbol->string target)
                                 " = "
                                 (make-constant 'ok)
                                 ";")))))))))

(define (compile-definition exp target linkage c-env)
  (let ((var (definition-variable exp))
        (get-value-code
          (compile (definition-value exp) 'val 'next c-env)))
    (end-with-linkage linkage
      (preserving '(env)
        get-value-code
        (make-instruction-sequence '(env val) (list target)
          `(,(string-append "define_variable("
                           (make-constant var)
                           ", val, env);")
            ,(string-append (symbol->string target)
                           " = "
                           (make-constant 'ok)
                           ";")))))))

(define (compile-if exp target linkage c-env)
  (let ((t-branch (make-label 'true_branch))
        (f-branch (make-label 'false_branch))
        (after-if (make-label 'after_if)))
    (let ((consequent-linkage
            (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next c-env))
            (c-code (compile (if-consequent exp) target consequent-linkage c-env))
            (a-code (compile (if-alternative exp) target linkage c-env)))
        (preserving '(env cont)
          p-code
          (append-instruction-sequences
            (make-instruction-sequence '(val) '()
              `(,(string-append "if(is_false(val)) goto "
                               (symbol->string f-branch)
                               ";")))
            (parallel-instruction-sequences
              (append-instruction-sequences
                (label-instruction t-branch)
                c-code)
              (append-instruction-sequences
                (label-instruction f-branch)
                a-code))
            (label-instruction after-if)))))))

(define (compile-sequence seq target linkage c-env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage c-env)
      (preserving '(env cont)
        (compile (first-exp seq) target 'next c-env)
        (compile-sequence (rest-exps seq) target linkage c-env))))

(define (compile-lambda exp target linkage c-env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after_lambda)))
    (let ((lambda-linkage (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
        (tack-on-instruction-sequence
          (end-with-linkage lambda-linkage
            (make-instruction-sequence '(env) (list target)
              `(,(string-append (symbol->string target)
                                " = make_compiled_procedure("
                                (reference-label proc-entry)
                                ", env);"))))
          (compile-lambda-body exp proc-entry c-env))
        (label-instruction after-lambda)))))

(define (compile-lambda-body exp proc-entry c-env)
  (let ((formals (lambda-parameters exp))
        (body (lambda-body exp)))
    (append-instruction-sequences
      (make-instruction-sequence '(env proc argl) '(env)
        `(,(label-instruction proc-entry)
          "env = compiled_procedure_env(proc);"
          ,(string-append "env = extend_environment("
                          (make-constant formals)
                          ", argl, env);")))
      (compile-sequence (if (contains-defines body)
                            (scan-out-defines body)
                            body)
                        'val
                        'return
                        (extend-compiletime-env formals c-env)))))

(define (scan-out-defines body)
  (define (initializations exprs)
    (cond ((null? exprs) '())
          ((definition? (car exprs))
           (cons (list (definition-variable (car exprs))
                       ''*unassigned*)
                 (initializations (cdr exprs))))
          (else initializations (cdr exprs))))
  (define (transform body)
    (map (lambda (exp)
           (if (definition? exp)
               (list 'set! (definition-variable exp) (definition-value exp))
               exp))
         body))
  (list (cons 'let (cons (initializations body) (transform body)))))

(define (contains-defines body)
  (cond ((null? body) false)
        ((definition? (car body))
         true)
        (else (contains-defines (cdr body)))))

(define (compile-application exp target linkage c-env)
  (let ((proc-code (compile (operator exp) 'proc 'next c-env))
        (operand-codes
          (map (lambda (operand) (compile operand 'val 'next c-env))
               (operands exp))))
    (preserving '(env cont)
      proc-code
      (preserving '(proc cont)
        (construct-arglist operand-codes)
        (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
          '("argl = nil();"))
        (let ((code-to-get-last-arg
                (append-instruction-sequences
                  (car operand-codes)
                  (make-instruction-sequence '(val) '(argl)
                    '("argl = cons(val, nil());")))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                code-to-get-last-arg
                (code-to-get-rest-args (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
          (preserving '(argl)
            (car operand-codes)
            (make-instruction-sequence '(val argl) '(argl)
              '("argl = cons(val, argl);")))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
          code-for-next-arg
          (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive_branch))
        (compiled-branch (make-label 'compiled_branch))
        (after-call (make-label 'after_call)))
    (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
        (make-instruction-sequence '(proc) '()
          `(,(string-append "if(is_primitive_procedure(proc)) goto "
                           (symbol->string primitive-branch)
                           ";")))
        (parallel-instruction-sequences
          (append-instruction-sequences
            (label-instruction compiled-branch)
            (compile-proc-appl target compiled-linkage))
          (append-instruction-sequences
            (label-instruction primitive-branch)
            (end-with-linkage linkage
              (make-instruction-sequence '(proc argl) (list target)
                `("gc();"
                  ,(string-append (symbol->string target)
                                  " = apply_primitive_procedure(proc, argl);"))))))
        (label-instruction after-call)))))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `(,(string-append "cont = "
                             (reference-label linkage)
                             ";")
             "val = compiled_procedure_entry(proc);"
             "goto *c_label(val);")))
        ((and (not (eq? target 'val)) (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc_return)))
           (make-instruction-sequence '(proc) all-regs
             `(,(string-append "cont = "
                               (reference-label proc-return)
                               ";")
               "val = compiled_procedure_entry(proc);"
               "goto *c_label(val);"
               ,(label-instruction proc-return)
               (string-append (symbol->string target)
                              " = val;")
               (string-append "goto "
                              (symbol->string linkage)
                              ";")))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc cont) all-regs
           `("val = compiled_procedure_entry(proc);"
             "goto *c_label(val);")))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE" target))))

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

(define (registers-needed s)
  (if (or (symbol? s) (string? s)) '() (car s)))
(define (registers-modified s)
  (if (or (symbol? s) (string? s)) '() (cadr s)))
(define (statements s)
  (if (or (symbol? s) (string? s)) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
      (list-union (registers-needed seq1)
                  (list-difference (registers-needed seq2)
                                   (registers-modified seq1)))
      (list-union (registers-modified seq1)
                  (registers-modified seq2))
      (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1) (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
              (make-instruction-sequence
                (list-union (list first-reg)
                            (registers-needed seq1))
                (list-difference (registers-modified seq1)
                                 (list first-reg))
                (append `(,(string-append "stack_push("
                                          (symbol->string first-reg)
                                          ");"))
                        (statements seq1)
                        `(,(string-append (symbol->string first-reg)
                                          " = stack_pop();"))))
              seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
    (registers-needed seq)
    (registers-modified seq)
    (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
    (list-union (registers-needed seq1)
                (registers-needed seq2))
    (list-union (registers-modified seq1)
                (registers-modified seq2))
    (append (statements seq1) (statements seq2))))

(define (ends-with? str suffix)
  (equal? suffix
          (substring str
                     (- (string-length str)
                        (string-length suffix))
                     (string-length str))))


(define program
  '(begin

    ; ------- evaluator -------

    (define (eval exp env)
     (cond ((self-evaluating? exp) exp)
           ((variable? exp) (lookup-variable-value exp env))
           ((quoted? exp) (text-of-quotation exp))
           ((assignment? exp) (eval-assignment exp env))
           ((definition? exp) (eval-definition exp env))
           ((if? exp) (eval-if exp env))
           ((lambda? exp)
            (make-procedure (lambda-parameters exp)
                            (lambda-body exp)
                            env))
           ((begin? exp) (eval-sequence (begin-actions exp) env))
           ;((cond? exp) (eval (cond->if exp) env))
           ((application? exp)
            (apply (eval (operator exp) env)
                   (list-of-values (operands exp) env)))
           (else (error "Unknown expression type -- EVAL" exp))))

    (define apply-in-underlying-scheme apply)

    (define (apply procedure arguments)
      (cond ((primitive-procedure? procedure)
             (apply-primitive-procedure procedure arguments))
            ((compound-procedure? procedure)
             (eval-sequence
               (procedure-body procedure)
               (extend-environment
                 (procedure-parameters procedure)
                 arguments
                 (procedure-environment procedure))))
            (else (error "Unknown procedure type -- APPLY" procedure))))

    (define (list-of-values exps env)
      (if (no-operands? exps)
          '()
          (cons (eval (first-operand exps) env)
                (list-of-values (rest-operands exps) env))))

    (define (eval-if exp env)
      (if (true? (eval (if-predicate exp) env))
          (eval (if-consequent exp) env)
          (eval (if-alternative exp) env)))

    (define (eval-sequence exps env)
      (cond ((last-exp? exps) (eval (first-exp exps) env))
            (else (eval (first-exp exps) env)
                  (eval-sequence (rest-exps exps) env))))

    (define (eval-assignment exp env)
      (set-variable-value! (assignment-variable exp)
                           (eval (assignment-value exp) env)
                           env)
      'ok)

    (define (eval-definition exp env)
      (define-variable! (definition-variable exp)
                        (eval (definition-value exp) env)
                        env)
      'ok)

    (define (self-evaluating? exp)
      (cond ((number? exp) true)
            ((string? exp) true)
            (else false)))

    (define (variable? exp) (symbol? exp))

    (define (quoted? exp) (tagged-list? exp 'quote))
    (define (text-of-quotation exp) (cadr exp))

    (define (tagged-list? exp tag)
      (if (pair? exp)
          (eq? (car exp) tag)
          false))

    (define (assignment? exp) (tagged-list? exp 'set!))
    (define (assignment-variable exp) (cadr exp))
    (define (assignment-value exp) (caddr exp))

    (define (definition? exp) (tagged-list? exp 'define))
    (define (definition-variable exp)
      (if (symbol? (cadr exp))
          (cadr exp)
          (caadr exp)))
    (define (definition-value exp)
      (if (symbol? (cadr exp))
          (caddr exp)
          (make-lambda (cdadr exp)    ; formal paramaters
                       (cddr exp))))  ; body

    (define (lambda? exp) (tagged-list? exp 'lambda))
    (define (lambda-parameters exp) (cadr exp))
    (define (lambda-body exp) (cddr exp))
    (define (make-lambda parameters body)
      (cons 'lambda (cons parameters body)))

    (define (if? exp) (tagged-list? exp 'if))
    (define (if-predicate exp) (cadr exp))
    (define (if-consequent exp) (caddr exp))
    (define (if-alternative exp)
      (if (null? (cdddr exp))
          'false
          (cadddr exp)))
    (define (make-if predicate consequent alternative)
      (list 'if predicate consequent alternative))

    (define (begin? exp) (tagged-list? exp 'begin))
    (define (begin-actions exp) (cdr exp))
    (define (last-exp? seq) (null? (cdr seq)))
    (define (first-exp seq) (car seq))
    (define (rest-exps seq) (cdr seq))
    (define (sequence->exp seq)
      (cond ((null? seq) seq)
            ((last-exp? seq) (first-exp seq))
            (else (make-begin seq))))
    (define (make-begin seq) (cons 'begin seq))

    (define (application? exp) (pair? exp))
    (define (operator exp) (car exp))
    (define (operands exp) (cdr exp))
    (define (no-operands? ops) (null? ops))
    (define (first-operand ops) (car ops))
    (define (rest-operands ops) (cdr ops))

    ;(define (cond? exp) (tagged-list? exp 'cond))
    ;(define (cond-clauses exp) (cdr exp))
    ;(define (cond-else-clause? clause)
    ;  (eq? (cond-predicate clause) 'else))
    ;(define (cond-predicate clause) (car clause))
    ;(define (cond-actions clause) (cdr clause))
    ;(define (cond->if exp)
    ;  (expand-clauses (cond-clauses exp)))

    ;(define (expand-clauses clauses)
    ;  (if (null? clauses)
    ;      'false
    ;      (let ((first (car clauses))
    ;            (rest (cdr clauses))
    ;        (if (cond-else-clause? first)
    ;            (if (null? rest)
    ;                (sequence->exp (cond-actions first))
    ;                (error "ELSE clause isn't last -- COND->IF" clauses)
    ;            (make-if (cond-predicate first)
    ;                     (sequence->exp (cond-actions first))
    ;                     (expand-clauses rest))

    (define (true? x) (not (eq? x false)))
    (define (false? x) (eq? x false))

    (define (make-procedure parameters body env)
      (list 'procedure parameters body env))
    (define (compound-procedure? p)
      (tagged-list? p 'procedure))
    (define (procedure-parameters p) (cadr p))
    (define (procedure-body p) (caddr p))
    (define (procedure-environment p) (cadddr p))

    (define (enclosing-environment env) (cdr env))
    (define (first-frame env) (car env))
    (define the-empty-environment '())
    (define (make-frame variables values) (cons variables values))
    (define (frame-variables frame) (car frame))
    (define (frame-values frame) (cdr frame))
    (define (add-binding-to-frame! var val frame)
      (set-car! frame (cons var (car frame)))
      (set-cdr! frame (cons val (cdr frame))))
    (define (extend-environment vars vals base-env)
      (if (= (length vars) (length vals))
          (cons (make-frame vars vals) base-env)
          (if (< (length vars) (length vals))
              (error "Too many arguments supplied" vars vals)
              (error "Too few arguments supplied" vars vals))))
    (define (lookup-variable-value var env)
      (define (env-loop env)
        (define (scan vars vals)
          (cond ((null? vars)
                 (env-loop (enclosing-environment env)))
                ((eq? var (car vars)) (car vals))
                (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable" var)
            (let ((frame (first-frame env)))
              (scan (frame-variables frame)
                    (frame-values frame)))))
      (env-loop env))
    (define (set-variable-value! var val env)
      (define (env-loop env)
        (define (scan vars vals)
          (cond ((null? vars)
                 (env-loop (enclosing-environment env)))
                ((eq? var (car vars)) (set-car! vals val))
                (else (scan (cdr vars) (cdr vals)))))
        (if (eq? env the-empty-environment)
            (error "Unbound variable -- SET!" var)
            (let ((frame (first-frame env)))
              (scan (frame-variables frame)
                    (frame-values frame)))))
      (env-loop env))
    (define (define-variable! var val env)
      (let ((frame (first-frame env)))
        (define (scan vars vals)
          (cond ((null? vars)
                 (add-binding-to-frame! var val frame))
                ((eq? var (car vars)) (set-car! vals val))
                (else (scan (cdr vars) (cdr vals)))))
        (scan (frame-variables frame)
              (frame-values frame))))

    (define (setup-environment)
      (let ((initial-env
              (extend-environment (primitive-procedure-names primitive-procedures)
                                  (primitive-procedure-objects primitive-procedures)
                                  the-empty-environment)))
        (define-variable! 'true true initial-env)
        (define-variable! 'false false initial-env)
        initial-env))

    (define (primitive-procedure? proc)
      (tagged-list? proc 'primitive))
    (define (primitive-implementation proc) (cadr proc))

    (define primitive-procedures
      (list (list 'car car)
            (list 'cdr cdr)
            (list 'cons cons)
            (list 'eq? eq?)
            (list 'equal? equal?)
            (list 'null? null?)
            (list 'number? number?)
            (list 'pair? pair?)
            (list 'symbol? symbol?)
            (list '= =)
            (list '< <)
            (list '> >)
            (list '+ +)
            (list '- -)
            (list '* *)
            (list '/ /)))

    (define (next n)
      (if (= n 2) 3 (+ n 2)))

    (define (primitive-procedure-names primitive-procedures)
      (map car primitive-procedures))

    (define (primitive-procedure-objects primitive-procedures)
      (map (lambda (proc) (list 'primitive (cadr proc)))
           primitive-procedures))

    (define (apply-primitive-procedure proc args)
      (apply-in-underlying-scheme
        (primitive-implementation proc) args))

    (define input-prompt ";;; M-C-Eval input:")
    (define output-prompt ";;; M-C-Eval value:")

    (define (driver-loop)
      (prompt-for-input input-prompt)
      (let ((input (read)))
        (let ((output (eval input the-global-environment)))
          (announce-output output-prompt)
          (user-print output)))
      (driver-loop))

    (define (prompt-for-input string)
      (newline) (newline) (display string) (newline))

    (define (announce-output string)
      (display string) (newline))

    (define (user-print object)
      (if (compound-procedure? object)
          (display (list 'compound-procedure
                         (procedure-parameters object)
                         (procedure-body object)
                         '<procedure-env>))
          (display object)))

    ; ------- utils -------
    (define (accumulate op initial sequence)
      (if (null? sequence)
          initial
          (op (car sequence)
              (accumulate op initial (cdr sequence)))))

    (define (map p sequence)
      (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

    (define (length sequence)
      (accumulate (lambda (x y) (+ y 1)) 0 sequence))

    ; ------- run -------
    (define the-global-environment (setup-environment))
    (driver-loop)))

'(define program '(define (count n)
                     (if (< n 1)
                         'ok
                         (count (- n 1)))))

'(define program '(begin
                     (define (sqr x)
                       (define (mul a b) (* a b))
                       (mul x x))
                     (display (sqr 5))
                     (newline)))

'(define program '(begin
                     (define (fib n)
                       (if (< n 2)
                           1
                           (+ (fib (- n 1)) (fib (- n 2)))))
                     (display (fib 32))
                     (newline)))

(define (pretty-print-code code)
  (for-each
    (lambda (instruction)
      (if (or (symbol? instruction)
              (and (string? instruction)
                   (ends-with? instruction ":")))
          (println instruction)
          (println "   " instruction)))
    code))

(define code (statements (compile program 'val 'next '())))

(pretty-print-code (compile-initialize-symbols))
(pretty-print-code code)

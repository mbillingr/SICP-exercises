(import (sicp utils))

(include "chapter5-vm.scm"
         "chapter5-assembler.scm")

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels active-labels)
      (update-insts! insts labels machine)
      insts)))

(define (extract-labels text receive)
  (let ((known-labels '()))
    (define (iter text receive)
      (if (null? text)
          (receive '() '() '())
          (iter (cdr text)
            (lambda (insts labels active-labels)
              (let ((next-inst (car text)))
                (if (symbol? next-inst)
                    (if (assoc next-inst known-labels)
                        (error "Ambiguous label -- ASSEMBLE" next-inst)
                        (begin
                          (set! known-labels
                                (cons (cons next-inst '())
                                      known-labels))
                          (receive insts
                                   (cons (make-label-entry next-inst insts)
                                         labels)
                                   (cons next-inst active-labels))))
                    (receive (cons (make-instruction next-inst active-labels)
                                   insts)
                             labels
                             '())))))))
    (iter text receive)))

(define (make-instruction text labels) (list '() labels text))
(define (instruction-text inst) (caddr inst))
(define (instruction-labels inst) (cadr inst))
(define (instruction-execution-proc inst) (car inst))
(define (set-instruction-execution-proc! inst proc) (set-car! inst proc))
(define (breakpoint? inst)
  (eq? 'breakpoint (cdddr inst)))
(define (instruction-set-breakpoint! inst)
  (set-cdr! (cddr inst) 'breakpoint))
(define (instruction-unset-breakpoint! inst)
  (set-cdr! (cddr inst) '()))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-count 0)
        (instruction-tracing false))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))
                  (list 'print-stack-statistics
                        (lambda () (stack 'print-statistics)))))
          (register-table
            (list (list 'pc pc)
                  (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register:" name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                ((breakpoint? (car insts))
                 (println "==>" (instruction-text (car insts)))
                 'pause)
                (else
                  (set! instruction-count (+ 1 instruction-count))
                  (if instruction-tracing
                      (println instruction-count
                               (instruction-labels (car insts))
                               (instruction-text (car insts))))
                  ((instruction-execution-proc (car insts)))
                  (execute)))))
      (define (proceed)
        (let ((insts (get-contents pc)))
          (set! instruction-count (+ 1 instruction-count))
          ((instruction-execution-proc (car insts)))
          (execute)))
      (define (set-breakpoint label n)
        (insert-breakpoint! n
                            (find-label label the-instruction-sequence)))
      (define (unset-breakpoint label n)
        (unset-breakpoint! n
                           (find-label label the-instruction-sequence)))
      (define (unset-all-breakpoints insts)
        (cond ((null? insts) 'done)
              (else
                (instruction-unset-breakpoint! (car insts))
                (unset-all-breakpoints (cdr insts)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'instruction-count) instruction-count)
              ((eq? message 'trace-on) (set! instruction-tracing true))
              ((eq? message 'trace-off) (set! instruction-tracing false))
              ((eq? message 'trace-register)
               (lambda (reg-name enable)
                 (((lookup-register reg-name) 'set-trace) enable)))
              ((eq? message 'set-breakpoint) set-breakpoint)
              ((eq? message 'unset-breakpoint) unset-breakpoint)
              ((eq? message 'unset-all-breakpoints)
               (unset-all-breakpoints the-instruction-sequence))
              ((eq? message 'proceed) (proceed))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (make-register name)
  (let ((contents '*unassigned*)
        (enable-trace false))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (if enable-trace
                   (println name ":" contents "<--" value))
               (set! contents value)))
            ((eq? message 'set-trace)
             (lambda (enable) (set! enable-trace enable)))
            (else
              (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (insert-breakpoint! n seq)
  (cond ((null? seq) (error "invalid breakpoint"))
        ((= 0 n)
         (instruction-set-breakpoint! (car seq)))
        (else (insert-breakpoint! (- n 1) (cdr seq)))))

(define (unset-breakpoint! n seq)
  (cond ((null? seq) (error "invalid breakpoint"))
        ((= 0 n)
         (instruction-unset-breakpoint! (car seq)))
        (else (unset-breakpoint! (- n 1) (cdr seq)))))

(define (find-label label insts)
  (cond ((null? insts)
         (error "Undefined label" label))
        ((contains? label (instruction-labels (car insts)))
         insts)
        (else
          (find-label label (cdr insts)))))

(define (contains? item seq)
  (cond ((null? seq) false)
        ((eq? item (car seq)) true)
        (else (contains? item (cdr seq)))))

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))

(define (cancel-breakpoint machine label n)
  ((machine 'unset-breakpoint) label n))

(define (cancel-all-breakpoints machine)
  (machine 'unset-all-breakpoints))

(define (proceed-machine machine)
  (machine 'proceed))

(define fac-machine
  (make-machine
    '(n val continue)
    (list (list '= =) (list '* *) (list '- -))
    '(controller
        (perform (op initialize-stack))
        (assign continue (label fact-done))
      fact-loop
        (test (op =) (reg n) (const 1))
        (branch (label base-case))
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-fact))
        (goto (label fact-loop))
      after-fact
        (restore n)
        (restore continue)
        (assign val (op *) (reg n) (reg val))
        (goto (reg continue))
      base-case
        (assign val (const 1))
        (goto (reg continue))
      fact-done)))


(set-breakpoint fac-machine 'after-fact 4)
(set-register-contents! fac-machine 'n 5)
(start fac-machine)

(println (get-register-contents fac-machine 'val))
(proceed-machine fac-machine)
(println (get-register-contents fac-machine 'val))
;(cancel-breakpoint fac-machine 'after-fact 4)
(cancel-all-breakpoints fac-machine)
(proceed-machine fac-machine)

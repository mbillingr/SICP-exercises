
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
          (if (null? insts)
              'done
              (begin
                (set! instruction-count (+ 1 instruction-count))
                (if instruction-tracing
                    (println instruction-count
                             (instruction-labels (car insts))
                             (instruction-text (car insts))))
                ((instruction-execution-proc (car insts)))
                (execute)))))
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
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

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

(define (compute-fac n)
  (set-register-contents! fac-machine 'n n)
  (start fac-machine)
  (get-register-contents fac-machine 'val))

(fac-machine 'trace-on)
(compute-fac 5)

(fac-machine 'trace-off)
(compute-fac 5)

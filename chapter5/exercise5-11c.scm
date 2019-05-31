
(import (sicp utils))

(include "chapter5-vm.scm"
         "chapter5-assembler.scm")

(define (make-register name)
  (let ((contents '*unassigned*)
        (stack (make-stack)))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            ((eq? message 'save) (push stack contents))
            ((eq? message 'restore) (set! contents (pop stack)))
            ((eq? message 'initialize-stack) (set! stack (make-stack)))
            (else
              (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value) ((register 'set) value))
(define (save-contents register) (register 'save))
(define (restore-contents register) (register 'restore))

(define (make-save inst machine pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (save-contents reg)
      (advance-pc pc))))

(define (make-restore inst machine pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (restore-contents reg)
      (advance-pc pc))))

(define (make-execution-procedure inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE" inst))))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((register-table
            (list (list 'pc pc)
                  (list 'flag flag))))
      (define (initialize-stack)
        (for-each (lambda (reg-entry)
                    (println "initializing register stack:" (car reg-entry))
                    ((cadr reg-entry) 'initialize-stack))
                  register-table))
      (let ((the-ops
              (list (list 'initialize-stack initialize-stack))))
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
                (else (error "Unknown request -- MACHINE" message))))
        dispatch))))


(define fib-machine
  (make-machine
    '(n val continue)
    (list (list '< <) (list '+ +) (list '- -))
    '(controller
        (perform (op initialize-stack))
        (assign continue (label fib-done))
      fib-loop
        (test (op <) (reg n) (const 2))
        (branch (label immediate-answer))
        (save continue)
        (assign continue (label afterfib-n-1))
        (save n)
        (assign n (op -) (reg n) (const 1))
        (goto (label fib-loop))
      afterfib-n-1
        (restore n)
        (assign n (op -) (reg n) (const 2))
        (assign continue (label afterfib-n-2))
        (save val)
        (goto (label fib-loop))
      afterfib-n-2
        (assign n (reg val))
        (restore val)
        ;(restore n)  ; this optimization would now cause an error
        (restore continue)
        (assign val (op +) (reg val) (reg n))
        (goto (reg continue))
      immediate-answer
        (assign val (reg n))
        (goto (reg continue))
      fib-done)))

(set-register-contents! fib-machine 'n 10)
(start fib-machine)
(println "fib 10 = " (get-register-contents fib-machine 'val))

(println (fib-machine 'operations))

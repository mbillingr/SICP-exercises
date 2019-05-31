
(import (sicp utils))

(include "chapter5-vm.scm"
         "chapter5-assembler.scm")


(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations))
        (meta (machine 'data-paths)))
    (for-each
      (lambda (inst)
        (set-instruction-execution-proc!
          inst
          (make-execution-procedure
            (instruction-text inst) labels machine
            pc flag stack ops meta)))
      insts)))

(define (make-execution-procedure inst labels machine pc flag stack ops meta)
  ((meta 'add-instruction) inst)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc meta))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc meta))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc meta))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc meta))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE" inst))))

(define (make-assign inst machine labels operations pc meta)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
            (cond ((operation-exp? value-exp)
                   ((meta 'add-source) (assign-reg-name inst) value-exp)
                   (make-operation-exp value-exp machine labels operations))
                  (else
                   ((meta 'add-source) (assign-reg-name inst) (car value-exp))
                   (make-primitive-exp (car value-exp) machine labels)))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (make-goto inst machine labels pc meta)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label labels (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register machine (register-exp-reg dest))))
             ((meta 'add-entry-point) (register-exp-reg dest))
             (lambda () (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE" inst)))))

(define (make-save inst machine stack pc meta)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    ((meta 'add-stack-register) (stack-inst-reg-name inst))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc meta)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    ((meta 'add-stack-register) (stack-inst-reg-name inst))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (data-paths (make-meta-info)))
    (let ((the-ops
            (list (list 'initialize-stack (lambda () (stack 'initialize)))))
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
              ((eq? message 'data-paths) data-paths)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (make-meta-info)
  (let ((unique-instructions '())
        (entry-points '())
        (stack-registers '())
        (sources '()))
    (define (dispatch msg)
      (cond ((eq? msg 'add-instruction)
             (lambda (inst) (set! unique-instructions
                                  (insert-sorted car
                                                 inst
                                                 unique-instructions))))
            ((eq? msg 'add-entry-point)
             (lambda (reg) (set! entry-points
                                 (insert-sorted (lambda (x) x)
                                                reg
                                                entry-points))))
            ((eq? msg 'add-stack-register)
             (lambda (reg) (set! stack-registers
                                 (insert-sorted (lambda (x) x)
                                                reg
                                                stack-registers))))
            ((eq? msg 'add-source)
             (lambda (reg src) ()
               (let ((entry (assoc reg sources)))
                 (if entry
                     (set-cdr! entry (insert-sorted car src (cdr entry)))
                     (set! sources (cons (list reg src)
                                         sources))))))
            ((eq? msg 'get-unique-instructions) unique-instructions)
            ((eq? msg 'get-entry-points) entry-points)
            ((eq? msg 'get-stack-registers) stack-registers)
            ((eq? msg 'get-sources) sources)))
    dispatch))

(define (insert-sorted key-proc item seq)
  (cond ((null? seq) (list item))
        ((equal? item (car seq))
         seq)
        ((< (key-proc item)
            (key-proc (car seq)))
         (cons item seq))
        (else
          (cons (car seq)
                (insert-sorted key-proc item (cdr seq))))))

(define fib-machine
  (make-machine
    '(n val continue)
    (list (list '< <) (list '+ +) (list '- -))
    '(controller
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

(let ((meta (fib-machine 'data-paths)))
  (println "==============================")
  (for-each println (meta 'get-unique-instructions))
  (println "==============================")
  (for-each println (meta 'get-entry-points))
  (println "==============================")
  (for-each println (meta 'get-stack-registers))
  (println "==============================")
  (for-each println (meta 'get-sources))
  (println "=============================="))

(set-register-contents! fib-machine 'n 10)
(start fib-machine)
(println "fib 10 = " (get-register-contents fib-machine 'val))

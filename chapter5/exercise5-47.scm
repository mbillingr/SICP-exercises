(import (sicp utils))

(include "chapter5-ec-machine2.scm")

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (compound-branch (make-label 'compound-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
        (make-instruction-sequence '(proc) '()
          `((test (op primitive-procedure?) (reg proc))
            (branch (label ,primitive-branch))))
        (parallel-instruction-sequences
          (append-instruction-sequences
            (make-instruction-sequence '(proc) '()
              `((test (op compound-procedure?) (reg proc))
                (branch (label ,compound-branch))))
            (parallel-instruction-sequences
              (append-instruction-sequences
                compiled-branch
                (compile-proc-appl target compiled-linkage))
              (append-instruction-sequences
                compound-branch
                (compile-compound-appl target compiled-linkage))))
          (append-instruction-sequences
            primitive-branch
            (end-with-linkage linkage
              (make-instruction-sequence '(proc argl) (list target)
                `((assign ,target
                          (op apply-primitive-procedure)
                          (reg proc)
                          (reg argl)))))))
        after-call))))

(define (compile-compound-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (save continue)  ; compound-apply expects continue on the stack
             (goto (reg compapp)))))
        ((and (not (eq? target 'val)) (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
             `((assign continue (label ,proc-return))
               (save continue)  ; compound-apply expects continue on the stack
               (goto (reg compapp))
               ,proc-return
               (assign ,target (reg val))
               (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
           `((save continue)  ; compound-apply expects continue on the stack
             (goto (reg compapp)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE" target))))

(compile-and-go
  '(define (f x) (g (g x))))

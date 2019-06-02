(import (sicp utils))

(include "chapter5-compiler.scm")

; The compiler uses right-to-left evaluation. This is determined in
; construct-arglist where the operand list is reversed, and in
; code-to-get-rest-args where the vals are consed to argl.

; This version preforms left-to-right evaluation.

(define (construct-arglist operand-codes)
  (if (null? operand-codes)
      (make-instruction-sequence '() '(argl)
        '((assign argl (const ()))))
      (let ((code-to-get-last-arg
              (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                  '((assign argl (op list) (reg val)))))))
        (if (null? (cdr operand-codes))
            code-to-get-last-arg
            (preserving '(env)
              code-to-get-last-arg
              (code-to-get-rest-args (cdr operand-codes)))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
          (preserving '(argl)
            (car operand-codes)
            (make-instruction-sequence '(val argl) '(argl)
              '((assign argl (op list) (reg argl))
                (assign argl (op append) (reg argl) (reg val)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
          code-for-next-arg
          (code-to-get-rest-args (cdr operand-codes))))))

; However, there is a performance cost: instead of consing, argl elements must
; be appended to preserve the correct order of arguments. Since append is O(n)
; and there are n arguments to append, we have introduced O(n^2) behavior to
; procudeure application! Let's see if we can do better...

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
          (map (lambda (operand) (compile operand 'val 'next))
               (operands exp))))
    (preserving '(env continue)
      proc-code
      (preserving '(proc continue exp)
        (construct-arglist operand-codes)
        (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (if (null? operand-codes)
      (make-instruction-sequence '() '(argl)
        '((assign argl (const ()))))
      (let ((code-to-get-last-arg
              (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl exp)
                  '((assign argl (op list) (reg val))
                    (assign exp (reg argl)))))))
        (if (null? (cdr operand-codes))
            code-to-get-last-arg
            (preserving '(env)
              code-to-get-last-arg
              (code-to-get-rest-args (cdr operand-codes)))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
          (preserving '(argl exp)
            (car operand-codes)
            (make-instruction-sequence '(val argl) '(argl)
              '((perform (op set-cdr!) (reg exp) (reg val))
                (assign exp (op cdr) (reg exp)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
          code-for-next-arg
          (code-to-get-rest-args (cdr operand-codes))))))

; The final implementation (ab)uses the exp register to store a pointer to the
; last cdr of argl, which can be mutated to construct the list in O(n). However,
; we still need twice as many operations as the right-left order evaluation.

(define code
  (compile
    '(+ 1 2 3 4)
    'val
    'next))

(println code)

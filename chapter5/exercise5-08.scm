
(import (sicp utils))

(include "chapter5-vm.scm"
         "chapter5-assembler.scm")


(define (extract-labels text receive)
  (let ((known-labels '()))
    (define (iter text receive)
      (if (null? text)
          (receive '() '())
          (iter (cdr text)
            (lambda (insts labels)
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
                                         labels))))
                    (receive (cons (make-instruction next-inst)
                                   insts)
                             labels)))))))
    (iter text receive)))


(define fac-machine
  (make-machine
    '(p c n)
    (list (list '> >) (list '* *) (list 'inc inc))
    '(  (assign p (const 1))
        (assign c (const 1))
      test-c
        (test (op >) (reg c) (reg n))
        (branch (label done))
        (assign p (op *) (reg c) (reg p))
        (assign c (op inc) (reg c))
        (goto (label test-c))
      ;done  ; uncommenting this causes an error
      done)))

(set-register-contents! fac-machine 'n 5)
(start fac-machine)
(println "5! = " (get-register-contents fac-machine 'p))

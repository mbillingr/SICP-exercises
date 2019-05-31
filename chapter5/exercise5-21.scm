(import (sicp utils))

(include "chapter5-vm.scm"
         "chapter5-assembler.scm")

(define recursive-count-leaves-machine
  (make-machine
    '(tree count continue)
    (list (list 'null? null?)
          (list 'pair? pair?)
          (list 'car car)
          (list 'cdr cdr)
          (list '+ +))
    '(controller
        (assign continue (label done))
      count-body
        (test (op null?) (reg tree))
        (branch (label empty-tree))
        (test (op pair?) (reg tree))
        (branch (label before-car))
      leaf
        (assign count (const 1))
        (goto (reg continue))
      before-car
        (save continue)
        (save tree)
        (assign tree (op car) (reg tree))
        (assign continue (label after-car))
        (goto (label count-body))
      after-car
        (restore tree)
        (save count)
        (assign tree (op cdr) (reg tree))
        (assign continue (label after-cdr))
        (goto (label count-body))
      after-cdr
        (assign tree (reg count))  ; abuse tree register to store temporary count
        (restore count)
        (restore continue)
        (assign count (op +) (reg tree) (reg count))
        (goto (reg continue))
      empty-tree
        (assign count (const 0))
        (goto (reg continue))
      done)))

(define explicit-count-leaves-machine
  (make-machine
    '(tree n tmp continue)
    (list (list 'null? null?)
          (list 'pair? pair?)
          (list 'car car)
          (list 'cdr cdr)
          (list '+ +))
    '(controller
        (assign continue (label done))
        (assign n (const 0))
      count-loop
        (test (op null?) (reg tree))
        (branch (label next))
        (test (op pair?) (reg tree))
        (branch (label before-car))
      leaf
        (assign n (op +) (reg n) (const 1))
        (goto (reg continue))
      before-car
        (save continue)
        (save tree)
        (assign tree (op car) (reg tree))
        (assign continue (label after-car))
        (goto (label count-loop))
      after-car
        (restore tree)
        (restore continue)
        (assign tree (op cdr) (reg tree))
        (goto (label count-loop))
      next
        (goto (reg continue))
      done)))


(define tree (cons (cons 1 (cons (cons 2 3) 4))
                   (cons (cons 5 (list 6 7 8))
                         (cons 9 10))))

(set-register-contents! recursive-count-leaves-machine 'tree tree)
(start recursive-count-leaves-machine)
(println (get-register-contents recursive-count-leaves-machine 'count))

(set-register-contents! explicit-count-leaves-machine 'tree tree)
(start explicit-count-leaves-machine)
(println (get-register-contents explicit-count-leaves-machine 'n))

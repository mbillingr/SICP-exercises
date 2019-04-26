(import (builtin core)
        (sicp utils))

(define numbers (list 1 2 3 4 5 6))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-tree tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree subtree)
             (sqr subtree)))
       tree))

(display "expected: (1 (4 (9 16) 25) (36 49))\n")

(display "  actual: ")
(display (square-tree
           (list 1
                 (list 2 (list 3 4) 5)
                 (list 6 7))))
(newline)


(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (sqr tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(display "expected: (1 (4 (9 16) 25) (36 49))\n")

(display "  actual: ")
(display (square-tree
           (list 1
                (list 2 (list 3 4) 5)
                (list 6 7))))
(newline)

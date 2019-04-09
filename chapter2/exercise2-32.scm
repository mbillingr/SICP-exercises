#!/usr/bin/env -S guile -s
!#

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

; set of all subsets
;
;  - the only subset of the empty set is the empty set
;
;  1. remove item A from the set
;  subsets of remaining set
;  plus
; TODO finish explanation :)
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (item)
                      (cons (car s) item))
                     rest)))))

(display "all sub sets of (1 2 3):\n")
(display (subsets (list 1 2 3)))
(newline)

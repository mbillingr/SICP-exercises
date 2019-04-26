(import (builtin core)
        (sicp utils))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

; set of all subsets
;
; Assume we have a function that returns all subset of a given set.
; We could use this function to return all subsets of our original
; set after removing one item.
; From this result we can construct the subsets of the original set.
; First, all subsets of the reduced set are subsets of the original set, too.
; Adding the removed item to all of these subsets gives us the remaining subsets.
;
; This gives us a recursive definition. Now we only need a termination condition,
; which is easily defined as:
;     The only subset of the empty set is the empty set.
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

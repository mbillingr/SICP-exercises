#!/usr/bin/env -S guile -s
!#

(define numbers (list 1 2 3 4 5 6))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (sqr (car things))
                    answer))))
  (iter items nil))

(display (square-list numbers))
(newline)
(display "This version of square-list returns the items in reverse because ")
(display "it builds the output by adding outer cons layers while cdr-ing ")
(display "down the input.\n")
(display "Reversing the arguments to cons can't work because lists need the ")
(display "items in the car and the remaining list in the cdr.\n")
(display "Thus, currently we can only build lists recursively; we need to ")
(display "build from the inside out, and inside is where the end of the list ")
(display "goes. An iterative implementation will require a method to mutate ")
(display "the last cdr of an existing list.\n")

#!/usr/bin/env -S guile -s
!#

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

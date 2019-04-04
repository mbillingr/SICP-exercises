#!/usr/bin/env -S guile -s
!#

(define (compose f g)
  (lambda (x) (f (g x))))

(display ((compose sqr inc) 6))
(newline)

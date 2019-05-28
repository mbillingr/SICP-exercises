(import (sicp utils))
(include "chapter4-query.scm")

(assert! '(rule (append-to-form () ?y ?y)))
(assert! '(rule (append-to-form (?u . ?v) ?y (?u . ?z))
                (append-to-form ?v ?y ?z)))

(assert! '(rule (reverse () ())))
(assert! '(rule (reverse (?a . ?b) ?z)
                (and (reverse ?b ?c)
                     (append-to-form ?c (?a) ?z))))

(display-query '(reverse (1 2 3 4 5 6) ?z))

; I have not found any solution that works with both forms
; (reverse ?z (1 2 3 4 5 6)) and (reverse (1 2 3 4 5 6) ?z)
; without falling into infinite recursion in one of them.
; The loop detector from last exercise doesn't help either.

(query-driver-loop)

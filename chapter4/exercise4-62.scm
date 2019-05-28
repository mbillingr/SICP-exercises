(import (sicp utils))
(include "chapter4-query.scm")

(assert! '(rule (last-pair (?x) (?x))))

(assert! '(rule (last-pair (?car . ?cdr) (?x))
                (last-pair ?cdr (?x))))

(display-query '(last-pair (3) ?x))
(display-query '(last-pair (1 2 3) ?x))
(display-query '(last-pair (2 ?x) (3)))

; This leads to a stack overflow. Presumably, because there is an infinite
; number of possible lists that satisfy the condition that the last element is 3.
;(display-query '(last-pair ?x (3)))

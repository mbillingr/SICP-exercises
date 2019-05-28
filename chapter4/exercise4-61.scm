(import (sicp utils))
(include "chapter4-query.scm")

(assert! '(rule (?x next-to ?y in (?x ?y . ?u))))

(assert! '(rule (?x next-to ?y in (?v .?z))
                (?x next-to ?y in ?z)))

(display-query '(?x next-to ?y in (1 (2 3) 4)))
; => (?x ?y) = (1 (2 3)), ((2 3) 4))

(display-query '(?x next-to 1 in (2 1 3 1)))
; => ?x = 2, 3

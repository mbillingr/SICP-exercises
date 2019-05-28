(import (sicp utils))
(include "chapter4-query.scm")
(include "chapter4-sample-db.scm")

(assert! '(rule (original-outranked-by ?staff-person ?boss)
                (or (supervisor ?staff-person ?boss)
                    (and (supervisor ?staff-person ?middle-manager)
                         (outranked-by ?middle-manager ?boss)))))

(assert! '(rule (outranked-by ?staff-person ?boss)
                (or (supervisor ?staff-person ?boss)
                    (and (outranked-by ?middle-manager ?boss)
                         (supervisor ?staff-person
                                     ?middle-manager)))))

; Q: Why does the new definition cause an infinite loop after producing the answer?
; A: First it finds Ben's supervisor in the first or clause. Then it recursively
;    enters outranked by without constraints, going infinite...
;    The original rule first constrains the ?middle-manager to be Ben's
;    supervisor, and thus the recursion of outranked-by is constrained to a
;    chain of supervisors that terminates if there is no loop in the assertions.

;(display-query '(outranked-by (Bitdiddle Ben) ?who))

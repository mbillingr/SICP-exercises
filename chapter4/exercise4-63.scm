(import (sicp utils))
(include "chapter4-query.scm")

(assert! '(son Adam Cain))
(assert! '(son Cain Enoch))
(assert! '(son Enoch Irad))
(assert! '(son Irad Mehujael))
(assert! '(son Mehujael Methushael))
(assert! '(son Methushael Lamech))
(assert! '(wife Lamech Ada))
(assert! '(son Ada Jabal))
(assert! '(son Ada Jubal))

(assert! '(rule (grandson ?g ?s)
                (and (son ?g ?f)
                     (son ?f ?s))))

(assert! '(rule (son ?m ?s)
                (and (wife ?m ?w)
                     (son ?w ?s))))

; uh, nice! we can add rules that augment the assertions transparently for queries.
; e.g. no assertion says who Lamech's sons are but the rule lets us deduct them
; as though they were explicitly defined.

(display-query '(grandson Cain ?name))
(display-query '(son Lamech ?name))
(display-query '(grandson Methushael ?name))

(import (sicp utils))
(include "chapter4-query.scm")

(include "chapter4-query-loop-detector.scm")

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

(assert! '(rule (last-pair (?x) (?x))))

(assert! '(rule (last-pair (?car . ?cdr) (?x))
                (last-pair ?cdr (?x))))

(assert! '(rule ((great grandson) ?gg ?s)
                (and (son ?gg ?g)
                     (grandson ?g ?s))))
(assert! '(rule ((great . ?rel) ?x ?z)
                (and (last-pair ?rel (grandson))
                     (son ?x ?y)
                     (?rel ?y ?z))))

(display-query '((grandson) Adam ?name))
(display-query '((great grandson) Adam ?name))
(display-query '((great great grandson) Adam ?name))
(display-query '((great great great grandson) Adam ?name))
(display-query '(?rel ?x Irad))

(display-query '((great grandson) ?g ?ggs))

; We don't find anything bejond (great great grandson).
; I suspect it has to do with my enter-query function in the
; loop detector, that only looks at the first frame of the stream.
(display-query '(?rel ?x Jubal))

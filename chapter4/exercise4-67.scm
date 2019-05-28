(import (sicp utils))
(include "chapter4-query.scm")
(include "chapter4-sample-db.scm")

(assert! '(rule (original-outranked-by ?staff-person ?boss)
                (or (supervisor ?staff-person ?boss)
                    (and (supervisor ?staff-person ?middle-manager)
                         (original-outranked-by ?middle-manager ?boss)))))

(assert! '(rule (outranked-by ?staff-person ?boss)
                (or (supervisor ?staff-person ?boss)
                    (and (outranked-by ?middle-manager ?boss)
                         (supervisor ?staff-person
                                     ?middle-manager)))))

; Theoretical implementation:
;    Assumption: we get an infinite recursion if we enter a query that has been
;                entered before, and both have the same information
;                (var pattern, frames).
;    By tracking the stream of frames in which a query is currently evaluated
;    we should be able to determine if a query should be skipped.
;    If a query is skipped we return an empty frame.

; Practical implementation:
;    Track the history of arguments passed to simple-query, ignoring the
;    rule application id.

(include "chapter4-query-loop-detector.scm")

(display-query '(outranked-by ?p ?who))
(display-query '(outranked-by (Bitdiddle Ben) ?who))

(assert! '(married Minnie Mickey))
(assert! '(rule (married ?x ?y) (married ?y ?x)))
(display-query '(married Mickey ?who))

; It does not help in this case, though:
(assert! '(son Ben Max))
(assert! '(son Max Timetraveler))
(assert! '(son Timetraveler Ben))

(assert! '(rule (descendant ?ancestor ?person)
                (or (son ?ancestor ?person)
                    (and (son ?ancestor ?ason)
                         (descendant ?ason ?person)))))

;(display-query '(descendant Ben ?who))

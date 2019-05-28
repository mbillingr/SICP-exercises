(import (sicp utils))
(include "chapter4-query.scm")
(include "chapter4-sample-db.scm")

(assert! '(meeting accounting (Monday 9am)))
(assert! '(meeting administration (Monday 10am)))
(assert! '(meeting computer (Wednesday 3pm)))
(assert! '(meeting administration (Friday 1pm)))
(assert! '(meeting whole-company (Wednesday 4pm)))

; a)
(display-query '(meeting ?division (Friday ?when)))

; b)
(assert! '(rule (meeting-time ?person ?day-and-time)
                (or (meeting whole-company ?day-and-time)
                    (and (job ?person (?division . ?r))
                         (meeting ?division ?day-and-time)))))

; c)
(display-query '(meeting-time (Hacker Alyssa P) (Wednesday ?time)))

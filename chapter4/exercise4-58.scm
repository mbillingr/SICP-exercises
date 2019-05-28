(import (sicp utils))
(include "chapter4-query.scm")
(include "chapter4-sample-db.scm")

(assert! '(rule (big-shot ?person ?division)
                (and (job ?person (?division . ?job))
                     (or (and (supervisor ?person ?boss)
                              (not (job ?boss (?division . ?bjob))))
                         (not (supervisor ?person ?boss))))))  ; people without supervisor are big shots too


(display-query '(big-shot ?who ?division))

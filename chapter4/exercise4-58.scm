
'(rule (big-shot ?person ?division)
       (and (job ?person (?division . ?job))
            (supervisor ?person ?boss)
            (not (job ?boss (?division . ?bjob)))))

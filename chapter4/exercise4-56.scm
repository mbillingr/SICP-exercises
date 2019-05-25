; a) the names of all people supervised by Ben Bitdiddle together with
;    their addresses
'(and (supervisor ?name (Bitdiddle Ben))
      (adress ?name ?address))

; b) all people whose salary is lass than Ben Bitdiddle's, together with their
;    salary and Ben's salary
'(and (salary (Bitdiddle Ben) ?ben's)
      (and (salary ?name ?amount)
           (lisp-value < ?amount ?ben's)))

; c) all people who are supervised by someone not in the computer division,
;    together with the supervisor's name and job
'(and (supervisor ?name ?sup)
      (not (job ?sup (computer . ?job))))

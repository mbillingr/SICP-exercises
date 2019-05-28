(import (sicp utils))
(include "chapter4-query.scm")
(include "chapter4-sample-db.scm")

; a) all people supervised by Ben Bitdiddle
(display-query '(supervisor ?x (Bitdiddle Ben)))

; b) the names and jobs of all people in the accounting division
(display-query '(job ?name (accounting . ?job)))

; c) the names and and addresses of all people who live in Slumerville
(display-query '(address ?name (Slumerville . ?address)))

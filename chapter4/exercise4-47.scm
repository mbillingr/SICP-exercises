(import (builtin core)
        (sicp utils))

(include "chapter4-amb.scm")

(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))

; This does not work because it expects another verb before parsing the
; prepositional phrase (or, put differently, it tries to parse the verb again).

(driver-loop)

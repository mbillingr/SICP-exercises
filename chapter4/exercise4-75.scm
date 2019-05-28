(import (sicp utils))
(include "chapter4-query.scm")
(include "chapter4-sample-db.scm")

; the unique form in this exercise conflicts with the
; unique form I introduced in exercise 4.66.

(define (uniquely-asserted contents frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let ((result (qeval (car contents) (singleton-stream frame))))
        ;(println result)
        (cond ((stream-null? result)
               the-empty-stream)
              ((stream-null? (stream-cdr result))
               result)
              (else the-empty-stream))))
    frame-stream))
(put 'unique 'qeval uniquely-asserted)

(display-query '(unique (job ?x (computer wizard))))
(display-query '(unique (job ?x (computer programmer))))

(assert! '(rule (supervise-one ?boss)
                (and (supervisor ?p ?boss)
                     (unique (supervisor ?q ?boss)))))

(display-query '(supervise-one ?who))

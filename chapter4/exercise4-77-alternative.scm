(import (sicp utils))
(include "chapter4-query.scm")

; This hackish solution simply applies conjoin twice.
; It will certainly couse trouble if there are side-effects in a  lisp-value
; query. I'm not sure it it works correctly in all other cases.
; Due to its simplicity this is certainly a solution to consider...

(put 'and 'qeval (lambda (conjuncts frame-stream)
                   (conjoin conjuncts
                            (conjoin conjuncts
                                     frame-stream))))

; ------- test it -------

(include "chapter4-sample-db.scm")

(display-query '(and (supervisor ?x ?y)
                     (not (job ?x (computer programmer)))))

(display-query '(and (not (job ?x (computer programmer)))
                     (supervisor ?x ?y)))

(import (sicp utils))
(include "chapter4-query.scm")
(include "chapter4-sample-db.scm")

(display-query '(wheel ?who))

; Q: Why is Oliver Warbucks is listed 4 times?
; A: We have the following chains of person -> middle-manager -> boss that
;    satisfy the wheel condition (-> means supervises):
;        Oliver -> Ben -> Alyssa
;        Oliver -> Ben -> Cy
;        Oliver -> Ben -> Lem
;        Ben -> Alyssa -> Luis
;        Oliver -> Eben -> Robert
;    See? 4x Oliver. A rule merely filters the stream of frames, that's why we
;    get all of them.

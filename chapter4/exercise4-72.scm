(import (sicp utils))
(include "chapter4-query.scm")

; Why inteleaving works better:
;   1. In the extreme case of infinite streams we might never see
;      the effect of one of the streams.
;   2. Two streams are likely to produce more diverse filtering,
;      so there might be a performance gain.

(import (sicp utils))
(include "chapter4-query.scm")

; The let binding ensures that THE-ASSERTIONS is evaluated before
; passing it to cons-stream.
; Cons-stream is a special form that delays the second argument, so
; without the let binding we would not extend the stream with an
; assertion. Instead, we would forget the previous stream and
; replace it with the definition of an infinite stream repeating
; only the latest assertion.

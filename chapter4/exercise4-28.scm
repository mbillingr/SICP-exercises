(import (builtin core)
        (sicp utils))

(include "chapter4-lazy.scm")

; Exercise 4.28: eval uses actual-value rather than eval
;                to evaluate the operator before passing it to apply, in order
;                to force the value of the operator. Give an example that
;                demonstrates the need for this forcing.
;
; If the operator was dynamically constructed (e.g. passed as a function
; argument) eval would return a thunk. However, apply expects an actual object
; (primitive or compound procedure). Forcing the operator gives us that object.

(import (builtin core)
        (sicp utils))

(include "chapter4-core.scm")

; Luis's map procedure fails because he uses the host system's map procedure
; which expects an operation of the host system's procedure type. It does not
; know what to do with m-eval procedures.

(driver-loop)

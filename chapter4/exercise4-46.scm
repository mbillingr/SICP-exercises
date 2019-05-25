(import (builtin core)
        (sicp utils))

(include "chapter4-amb.scm")

; If the arguments were not evaluated from left to right there would be trouble
; because they would not fetch the unparsed words from the global variable in
; the correct order.

(driver-loop)

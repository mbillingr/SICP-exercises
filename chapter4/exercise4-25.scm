(import (builtin core)
        (sicp utils))

; The function will enter infinite recursion (and eventually overflow the stack
; because the recursive call is not in a tail-call position).

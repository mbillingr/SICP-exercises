(import (builtin core)
        (sicp utils))

(include "chapter4-lazy.scm")

(eval '(begin
         (define count 0)
         (define (id x) (set! count (+ count 1)) x)
         (define w (id (id 10))))
      the-global-environment)

(driver-loop)
; ;;; L-Eval input:
; count
; ;;; L-Eval value:
; 1  ; the outer call to id has been evaluated but not the inner call because the return value has not yet been accessed
; ;;; L-Eval input:
; w
; ;;; L-Eval value:
; 10  ; the value is passed through both calls to id
; ;;; L-Eval input:
; count
; ;;; L-Eval value:
; 2   ; both calls to id have been evaluated

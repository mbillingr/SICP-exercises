(import (builtin core)
        (sicp utils))

; The halting problem...

; assume the procedure correctly determines if (p a) would return or not
(define (halts? p a) false)

; this function will run forever if (p p) halts but it it will halt if (p p) runs forever...
(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

(define (run-forever) (run-forever))


; let's evaluate (try try) using the substitution model:
;     (try try) => (if (halts? try try) (run-forever) 'halted)
;
; assuming (halts? try try) evaluates to true:
;     (try try) => (run-forever)
; (try try) would not halt, which is inconsistent with (halts? try try) => true
;
; assuming (halts? try try) evaluates to false:
;     (try try) => 'halted
; (try try) would halt, which is inconsistent with (halts? try try) => false

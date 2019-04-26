(import (builtin core)
        (sicp utils))

(define data (list 1 (list 2 (list 3 4))))

; result printed by interpreter
; expected: (1 (2 (3 4)))
(display data) (newline)

; box and pointer structure
;
; [.  .]
;  |  |
;  1  [.  0]
;      |
;     [.  .]
;      |  |
;      2  [.  0]
;          |
;          [.  .]
;           |  |
;           3  [.  0]
;               |
;               4

; interpretation as tree
;
; (1 (2 (3 4)))
; |
; +-- 1
; |
; +-- (2 (3 4)))
;     |
;     +-- 2
;     |
;     +-- (3 4)
;         |
;         +-- 3
;         |
;         +-- 4

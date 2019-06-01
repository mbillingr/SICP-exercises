
; (op a1 a2)
;                  | (f 'x 'y) | ((f) 'x 'y) | (f (g 'x) y) | (f (g 'x) 'y) |
; -----------------+-----------+-------------+--------------+---------------+
;   (save env)     |           |      *      |              |               |
;   (eval op)      |     -     |      -      |       -      |        -      |
;   (restore env)  |           |      *      |              |               |
;   (save proc)    |           |             |       *      |        *      |
;   (save env)     |           |             |       *      |               |
;   (save argl)    |           |             |       *      |        *      |
;   (eval a1)      |     -     |      -      |       -      |        -      |
;   (restore argl) |           |             |       *      |        *      |
;   (restore env)  |           |             |       *      |               |
;   (save argl)    |           |             |              |               |
;   (eval a2)      |     -     |      -      |       -      |        -      |
;   (restore argl) |           |             |              |               |
;   (restore proc) |           |             |       *      |        *      |
;
;     * means required

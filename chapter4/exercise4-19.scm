(import (builtin core)
        (sicp utils))

(include "chapter4-core.scm")

(let ((a 1))
  (define (f x)
   (define b (+ a x))
   (define a 5)
   (+ a b))
  (f 10))

; Ben: sequential b := 1 + 10 = 11, a := 5, result: 16
; Alyssa: b := *undef* + 10 = *error*
; Eva: a := 5, b := 5 + 10 = 15, result: 20

; Ben's strategy is error prone because the result depends on the ordering of the definitions.

; Alyssa's strategy is better because it causes an explicit error in ambiguous situations.

; Eva's solution is interesting. The book states that in principle it is correct, but I think
; it also makes code more difficult to understand. Imagine there are many more lines between
; the definitions of a and b. When reading from top to bottom you may not realize that there
; is a definition of a in the same scope and you would expect the code to define b as 11.

; Thus, I support Alyssa's viewpoint.

; I could imagine at least two ways to implement Eva's way as an extension of Alyssa's:
;     A) brute force: try reordering the definitions until there is no error or
;                     all permutations have been tested.
;     B) algorithmic: put those definitions first that do not depend on any
;                     other definitions. then sequentially add those definitions
;                     whose dependencies have been satisfied.
;                     I'm sure this can be solved with some smart graph-theoretical algorithm.
; Both approaches cannot deal with dependency cycles and would signal an error.

(import (builtin core)
        (sicp utils))

(include "chapter4-lazy.scm")

; a) I guess eval forces each expression in begin to be "looked at" and calls to the native display procedure are enforced.

; b) original: (p1 1) => (1 2), (p2 1) => 1. In the latter case, the assignment is never evaluated.
;    changed: (p1 1) => (1 2), (p2 1) => (1 2). The change would force e to be evaluated.

; c) The change causes more stuff to be evaluated. a) does not rely on anything staying unevaluated, so the change would not affect the example.

; d) I think that if we want to use a lazy evaluator it should be as lazy as
;    possible. After all, the purpose of lazyness is to avoid unnecessary
;    computations. If programmers want to go crazy with side effects let them
;    be explicit about what should be evaluated.
;
;    lgessler at http://community.schemewiki.org/?sicp-ex-4.30 made a good point:
;       For d, I feel like the answer to this question hinges on whether this
;       claim is true:
;           All items in a sequence except for the last one are only there
;           for the side effects they produce.
;       This seems like it should be true, because it's easy to see that the
;       value of an expression in non-final position in a sequence is discarded.
;       If this is true, then I think we must prefer Cy's method, as the only
;       sane reason for inclusion of a non-final sequence item would be for its
;       side-effects.

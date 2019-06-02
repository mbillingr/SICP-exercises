(import (sicp utils))

(include "chapter5-compiler.scm")

(define code
  (compile
    '(define (f x)
       (+ x (g (+ x 2))))
    'val
    'next))

(println code)

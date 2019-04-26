(import (builtin core)
        (sicp utils))

(define numbers (list 1 2 3 4 5 6))

(define (for-each proc items)
  (if (null? items)
      nil
      (begin (proc (car items))
             (for-each proc (cdr items)))))


(for-each (lambda (x)
            (display x)
            (newline))
          (list 57 321 88))

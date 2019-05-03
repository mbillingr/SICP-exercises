
;;   This is a very crude implementation of a set using lists
;;   It relies on eq? to deterimne object identity
;;
;;   Todo: make the comparison operator an argument to make-set?
;;   Todo: is it feasible to make a generic ordered/unordered implementation?
(define-library (sicp utils set)
  (export make-set)

  (import (builtin core))

  (begin
    (define (make-set)
      (define data '())
      (define (insert k)
        (if (eq? 'not-found (lookup k))
            (set! data (cons k data))))
      (define (lookup k)
        (define (loop rest)
          (cond ((null? rest) 'not-found)
                ((eq? k (car rest)) (car rest))
                (else (loop (cdr rest)))))
        (loop data))
      (define (dispatch m k)
        (cond ((eq? m 'lookup) (lookup k))
              ((eq? m 'insert) (insert k))
              (else (error "Unknown request -- SET" m))))
      dispatch)))

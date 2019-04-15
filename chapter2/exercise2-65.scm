#!/usr/bin/env -S guile -s
!#

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
                (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n left-size 1)))
            (let ((this-entry (car non-left-elts))
                  (right-result
                    (partial-tree
                      (cdr non-left-elts)
                      right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                      (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (intersection-set set1 set2)
  (list->tree
    (intersection-ordered-set (tree->list set1)
                              (tree->list set2))))

(define (union-set set1 set2)
  (list->tree
    (union-ordered-set (tree->list set1)
                       (tree->list set2))))

; this is intersection-set from exercise 2.62 (sets as ordered lists)
(define (intersection-ordered-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-ordered-set (cdr set1)
                                               (cdr set2))))
              ((< x1 x2)
               (intersection-ordered-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-ordered-set set1 (cdr set2)))))))

; this is union-set from exercise 2.62 (sets as ordered lists)
(define (union-ordered-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1))
                (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1
                         (union-ordered-set (cdr set1)
                                            (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-ordered-set (cdr set1) set2)))
                  ((< x2 x1)
                   (cons x2 (union-ordered-set set1 (cdr set2)))))))))

(define a (list->tree '(1 2 3 4)))
(define b (list->tree '(3 4 5 6)))

(println (tree->list a) (tree->list b))
(println "intersection:" (tree->list (intersection-set a b)))
(println "       union:" (tree->list (union-set a b)))

; now we need to show O(n) behavior...

(define (make-set n n_max)
  (define (iter n set)
    (if (= 0 n)
        set
        (iter (- n 1)
              (adjoin-set (random n_max) set))))
  (iter n '()))

(define (measure f n)
  (display "n = ")
  (display n)
  (display ": ")
  (let ((x (make-set n (* n 2)))
        (y (make-set n (* n 2))))
    (timeit (lambda () (f x y)))))

(println "\nintersection")
(measure intersection-set 10)
(measure intersection-set 100)
(measure intersection-set 1000)
;(measure intersection-set 10000)

(println "\nunion")
(measure union-set 10)
(measure union-set 100)
(measure union-set 1000)
;(measure union-set 10000)

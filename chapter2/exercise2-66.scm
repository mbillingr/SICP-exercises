#!/usr/bin/env -S guile -s
!#

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (make-record key value) (cons key value))
(define (key record) (car record))
(define (value record) (cdr record))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (car set-of-records)))
         (car set-of-records))
        ((< given-key (key (car set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (car set-of-records)))
         (lookup given-key (right-branch set-of-records)))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define dataset (make-tree (make-record 4 "t'is da root")
                           (make-tree (make-record 2 'B)
                                      (make-tree (make-record 1 'A) nil nil)
                                      (make-tree (make-record 3 'C) nil nil))
                           (make-tree (make-record 6 'F)
                                      (make-tree (make-record 5 42) nil nil)
                                      (make-tree (make-record 7 'G) nil nil))))

(println (tree->list dataset))

(println (lookup 5 dataset))
(println (lookup 4 dataset))
(println (lookup 3 dataset))
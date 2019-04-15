#!/usr/bin/env -S guile -s
!#

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

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

; Q(a): Write a short paragraph explaining as clearly as you
;       can how partial-tree works. Draw the tree produced
;       by list->tree for the list (1 3 5 7 9 11).
;
; A: 1. Take the first half (i.e. (n-1)/2) of elements and recursively build the
;       left subtree with them.
;    2. Take the first of the remaining elements as the current entry.
;    3. Recursively build the right subtree with all elements that are left.
;    4. Construct the tree from the current entry, the left and the right subtrees.
;
;    (partial-tree '(1 3 5 7 9 11) 6)
;       (partial-tree '(1 3 ...) 2)
;         (partial-tree '(...) 0)
;         1
;         (partial-tree '(3 ...) 1)
;           (partial-tree '(...) 0)
;           3
;           (partial-tree '(...) 0)
;       5
;       (partial-tree '(7 9 11) 3)
;         (partial-tree '(7 ...) 1)
;           (partial-tree '(...) 0)
;           7
;           (partial-tree '(...) 0)
;         9
;         (partial-tree '(11) 1)
;           (partial-tree '(...) 0)
;           11
;           (partial-tree '() 0)
;
;           +--
;           |
;     +--1--+
;     |     |
;     |     +--3
;     |
;  5--+
;     |
;     |     +--7
;     |     |
;     +--9--+
;           |
;           +--11

(println (list->tree '(1 3 5 7 9 11)))

; Q(b): What is the order of growth in the number of steps re-
;       quired by list->tree to convert a list of n elements?
;
; A: The recursion visits each element 1, making it O(n).
;    cons and make-tree are called for each element, both are O(1).
;    The total order of growth in the number of steps is O(n).

(import (builtin core)
        (sicp utils))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                      (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; Q(a): Do the two procedures produce the same result for
;       every tree? If not, how do the results differ? What
;       lists do the two procedures produce for the trees in
;       Figure 2.16?
;
; A: Version 1 builds a list of the form ([...] is a list, + appends two lists)
;    [<left branch>] + [<entry> <right branch>]
;    where <left branch> and <right branch> are recursively expanded into lists.
;
;    Version 2 builds the list from the right. It first conses the rightmost
;    leaf to the result, then the entry above, and then descends into the
;    left branch of that entry.
;
;    I would expect both procedures to produce the same results.

(define a
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

(define b (make-tree 3
                     (make-tree 1 '() '())
                     (make-tree 7
                                (make-tree 5 '() '())
                                (make-tree 9
                                           '()
                                           (make-tree 11 '() '())))))

(define c
  (make-tree 5
             (make-tree 3
                        (make-tree 1 '() '())
                        '())
             (make-tree 9
                        (make-tree 7 '() '())
                        (make-tree 11 '() '()))))


(println (tree->list-1 a))
(println (tree->list-2 a))
(println (tree->list-1 b))
(println (tree->list-2 b))
(println (tree->list-1 c))
(println (tree->list-2 c))

; Q(b): Do the two procedures have the same order of growth
;       in the number of steps required to convert a balanced
;       tree with n elements to a list? If not, which one grows
;       more slowly?
;
;       Version 1 visits each node once, so it recurses in O(n).
;       It calls append on each node, which is O(l) - l is the number of nodes
;       in the left subtree. l grows linearly with the number of nodes n,
;       but the *average* l over all nodes is O(log n).
;       Thus, I conclude the total number of steps grows with O(n log n).
;
;       Version 2 visits each node once, which is O(n).
;       It calls only cons on each node, which is O(1).
;       Thus, I conclude the total number of steps grows with O(n).
;
;       Version 2 grows more slowly in number of steps required.
;       Furthermore, it tail-recurses into the left subtree which reduces
;       the space requirement.

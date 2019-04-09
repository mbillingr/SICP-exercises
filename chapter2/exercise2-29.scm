#!/usr/bin/env -S guile -s
!#

(define (make-mobile left right)
  (list left right))
(define (left-branch structure)
  (car structure))
(define (right-branch structure)
  (car (cdr structure)))

(define (make-branch length structure)
  (list length structure))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

(define (branch-weight branch)
  (total-weight (branch-structure branch)))

(define (total-weight structure)
  (if (not (pair? structure))
      structure
      (+ (branch-weight (left-branch structure))
         (branch-weight (right-branch structure)))))

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (balanced? structure)

  (if (not (pair? structure))
      #t
      (let ((left (left-branch structure))
            (right (right-branch structure)))
        (and (= (branch-torque left)
                (branch-torque right))
             (balanced? (branch-structure left))
             (balanced? (branch-structure right))))))

(define mobile (make-mobile (make-branch 10 5)
                            (make-branch 5 (make-mobile (make-branch 2 6)
                                                        (make-branch 3 4)))))

(define simple-balanced (make-mobile (make-branch 10 2) (make-branch 10 2)))
(define simple-unbalanced1 (make-mobile (make-branch 10 2) (make-branch 10 1)))
(define simple-unbalanced2 (make-mobile (make-branch 10 2) (make-branch 9 2)))

(display (balanced? simple-balanced)) (newline)
(display (balanced? simple-unbalanced1)) (newline)
(display (balanced? simple-unbalanced2)) (newline)
(display (total-weight mobile)) (newline)
(display (balanced? mobile)) (newline)


(define (make-mobile left right)
  (cons left right))
(define (left-branch structure)
  (car structure))
(define (right-branch structure)
  (cdr structure))

(define (make-branch length structure)
  (cons length structure))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))

; we can change the representation of mobile and branch (from list to pair)
; and don't have to change the remaining program AT ALL.

(define mobile (make-mobile (make-branch 10 5)
                 (make-branch 5 (make-mobile (make-branch 2 6)
                                             (make-branch 3 4)))))

(define simple-balanced (make-mobile (make-branch 10 2) (make-branch 10 2)))
(define simple-unbalanced1 (make-mobile (make-branch 10 2) (make-branch 10 1)))
(define simple-unbalanced2 (make-mobile (make-branch 10 2) (make-branch 9 2)))

(display (balanced? simple-balanced)) (newline)
(display (balanced? simple-unbalanced1)) (newline)
(display (balanced? simple-unbalanced2)) (newline)
(display (total-weight mobile)) (newline)
(display (balanced? mobile)) (newline)

#!/usr/bin/env -S guile -s
!#

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(define A (list (list 0 1 0)
                (list 0 0 1)
                (list 1 0 0)))

(define B (list (list 1 2 3)
                (list 4 5 6)
                (list 7 8 9)))

(define ex (list 1 0 0))
(define ey (list 0 1 0))
(define ez (list 0 0 1))
(define v (list 3 2 1))

(display (dot-product v v))
(newline)

(display (matrix-*-vector A v))
(newline)

(display (transpose A))
(newline)

(display (matrix-*-matrix A B))
(newline)

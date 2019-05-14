(import (builtin core)
        (sicp utils)
        (sicp utils prime))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (println x))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (partial-sums s)
  (define sum (add-streams s (cons-stream 0 sum)))
  sum)

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) S1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream s2car (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream s1car
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2)))))))))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

; ===========================

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))


(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

; qualitative answer:
;    After inserting the first element, elements are taken alternatingly from
;    the first row and from the remaining table. This means that about half the
;    elements of any given position in the stream are from the first row.
;    To take elements from the remaining table, we take the top left elemnt,
;    and continue alternating from the next row and the remaining subtable.
;    Thus, about a quarter of the elements before any given position come from
;    the second row.
;    In consequence, rows are inserted in the stream with sparsity increasing
;    by a factor of 2 per row.
;    A precise statement is made difficult by the fact that the first two items
;    of each row are taken subsequently. I think the analysis would be simpler
;    if in (pairs...) the arguments to (interleave s1 s2) were swapped.


; empiric answer:
;    The following rule was derived by staring at a table denoting the insertion
;    order of the first 80 elements.

(define (nth i j)  ; i, j, and the result are 0-based
  (cond ((= j i)
         (- (power 2 (+ i 1)) 1))
        ((= j (+ i 1))
         (+ (nth i i)
            (power 2 i)))
        ((> j (+ i 1))
         (+ (nth i (+ i 1))
            (* (- j i 1) (power 2 (+ i 1)))))
        (else (error "invalid pair" (list i j)))))

; approximate answer: (i just realized the exercise says "approximate")
(define (nth-approx i j)
  (* (+ 1 (- j i))
     (power 2 (+ i 1))))

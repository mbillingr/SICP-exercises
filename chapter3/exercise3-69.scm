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

; ===========================

(define (stream-cadr stream)
  (stream-car (stream-cdr stream)))

(define (stream-caddr stream)
  (stream-car (stream-cdr (stream-cdr stream))))


(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (cons (stream-car s) x))
                  (pairs t (stream-cdr u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pythagorean (stream-filter (lambda (stu) (= (+ (sqr (car stu))
                                                       (sqr (cadr stu)))
                                                    (sqr (caddr stu))))
                                   (triples integers integers integers)))

(println (stream-ref pythagorean 0))
(println (stream-ref pythagorean 1))
(println (stream-ref pythagorean 2))
(println (stream-ref pythagorean 3))

(println "===================")
; the exercise says to generate pythagorean triples from a triple stream but
; generating them from a pair stream is much more efficient...

(define (triangle a b)
  (list (- (sqr b) (sqr a))
        (* 2 a b)
        (+ (sqr b) (sqr a))))

(define pythagorean-fast
  (stream-map (lambda (ij) (triangle (car ij) (cadr ij)))
              (stream-filter (lambda (ij) (< (car ij) (cadr ij)))
                             (pairs integers integers))))

(println (stream-ref pythagorean-fast 0))
(println (stream-ref pythagorean-fast 1))
(println (stream-ref pythagorean-fast 2))
(println (stream-ref pythagorean-fast 3))

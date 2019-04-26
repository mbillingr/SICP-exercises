(import (builtin core)
        (sicp utils))

(define (triangle row col)
    (cond ((= 0 col) 1)
          ((= row col) 1)
          (else (+ (triangle (- row 1)
                             (- col 1))
                   (triangle (- row 1)
                             col)))))

(define (show-row row col)
    (display (triangle row col))
    (if (= row col)
        -1
        (show-row row (+ 1 col))))

(define (show-triangle row)
    (show-row row 0)
    (newline)
    (if (= row 0)
        -1
        (show-triangle (- row 1))))

(show-triangle 4)

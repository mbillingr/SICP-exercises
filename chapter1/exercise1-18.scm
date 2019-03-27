(define (mul-iter a x y) 
  (cond ((= y 0) a)
	((even? y) (mul-iter a (double x) (halve y)))
	(else (mul-iter (+ a x) x (- y 1)))))		  
		  
(define (double x) (+ x x))
(define (halve x) (/ x 2))
		 
(define (mul x y) (mul-iter 0 x y))
		
(mul 3 6)

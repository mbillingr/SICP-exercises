(import (builtin core)
        (sicp utils))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (member item x)
  (cond ((null? x) false)
        ((equal? item (car x)) x)
        (else (member item (cdr x)))))

(define (loop n body)
  (if (= n 0)
      '(done)
      (let ((result (body n)))
        (if (eq? 'ok (car result))
            result
            (loop (- n 1) body)))))

(define (multiple-dwelling)
  (cdr (loop
         5
         (lambda (baker)
           (if (not (= baker 5))
               (loop
                 5
                 (lambda (cooper)
                   (if (not (= cooper 1))
                       (loop
                         5
                         (lambda (fletcher)
                           (if (and (not (= fletcher 5))
                                    (not (= fletcher 1))
                                    (not (= (abs (- fletcher cooper)) 1)))
                               (loop
                                 5
                                 (lambda (miller)
                                   (if (> miller cooper)
                                       (loop
                                         5
                                         (lambda (smith)
                                           (if (and (not (= (abs (- smith fletcher)) 1))
                                                    (distinct? (list baker cooper fletcher miller smith)))
                                               (list 'ok
                                                     (list 'baker baker)
                                                     (list 'cooper cooper)
                                                     (list 'fletcher fletcher)
                                                     (list 'miller miller)
                                                     (list 'smith smith))
                                               '(not-ok))))
                                       '(not-ok))))
                               '(not-ok))))
                       '(not-ok))))
               '(not-ok))))))


(println (multiple-dwelling))
(timeit multiple-dwelling)

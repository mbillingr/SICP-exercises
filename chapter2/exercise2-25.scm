#!/usr/bin/env -S guile -s
!#

; get 7 from each list

; (1 3 (5 7) 9) : cdr cdr car cdr car
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))

; ((7)) : car car
(car (car (list (list 7))))

; (1 (2 (3 (4 (5 (6 7)))))) : cdr car cdr car cdr car cdr car cdr car cdr car
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

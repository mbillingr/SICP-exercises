(import (builtin core)
        (sicp utils))

(define (make-account balance password)
  (let ((wrong-count 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount)))
    (define (wrong-password . args)
      "Incorrect password")
    (define (call-the-cops . args)
      "Please wait for the cops to arrive")
    (define (lock-account . args)
      (set! password 'unhackable-invalid-password)
      (call-the-cops))
    (define (dispatch p m)
      (cond ((eq? p password)
             (set! wrong-count 0)
             (cond ((eq? m 'withdraw) withdraw)
                   ((eq? m 'deposit) deposit)
                   (else (error "Unknown request -- MAKE-ACCOUNT" m))))
            (else
              (set! wrong-count (+ wrong-count 1))
              (if (> wrong-count 7)
                  lock-account
                  wrong-password))))
    dispatch))

(define acc (make-account 100 'secret-password))

(println ((acc 'secret-password 'withdraw) 40))
(println ((acc 'some-other-password 'deposit) 50))
(println ((acc 'some-other-password 'deposit) 50))
(println ((acc 'secret-password 'deposit) 5000))
(println ((acc 'wrong-password 'withdraw) 4000))
(println ((acc 'wrong-password 'withdraw) 4000))
(println ((acc 'wrong-password 'withdraw) 4000))
(println ((acc 'wrong-password 'withdraw) 4000))
(println ((acc 'wrong-password 'withdraw) 4000))
(println ((acc 'wrong-password 'withdraw) 4000))
(println ((acc 'wrong-password 'withdraw) 4000))
(println ((acc 'wrong-password 'withdraw) 4000))
(println ((acc 'secret-password 'withdraw) 4000))
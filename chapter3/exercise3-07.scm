(import (builtin core)
        (sicp utils))

(define (make-raw-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define (make-account-access account password)
  (define (wrong-password . args)
    "Incorrect password")
  (define (dispatch p m)
    (if (eq? p password)
        (if (eq? m 'get-inner-account)
            account
            (account m))
        wrong-password))
  dispatch)

(define (make-account balance password)
  (make-account-access (make-raw-account balance)
                       password))

(define (make-joint-account account-access old-pw new-pw)
  (make-account-access (account-access old-pw
                                       'get-inner-account)
                       new-pw))

(define acc (make-account 100 'secret-password))

(println ((acc 'secret-password 'withdraw) 40))
(println ((acc 'some-other-password 'deposit) 50))

; create another access to acc
(define acc2 (make-joint-account acc 'secret-password '123))

(println ((acc '123 'withdraw) 40))  ; the old access should not work with the new pw
(println ((acc2 'secret-password 'withdraw) 40))  ; the new access should not work with the old pw
(println ((acc2 '123 'withdraw) 40))  ; this should work

(println ((acc 'secret-password 'withdraw) 20))  ; the balance should have been changed through acc2

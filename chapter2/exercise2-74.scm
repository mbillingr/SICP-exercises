(import (builtin core)
        (sicp utils))

(define (try-get-and-call a b . args)
  (let ((result (get a b)))
    (and result (apply result args))))

(define (get-file company)
  (try-get-and-call 'get-file company))

(define (get-record employee-id file)
  (try-get-and-call 'get-record (type-tag file) employee-id (contents file)))

(define (get-salary record)
  (try-get-and-call 'get-salary (type-tag record) (contents record)))

(define (find-employee-record employee-id files)
  (if (null? files)
      #f
      (let ((record (get-record employee-id (car files))))
        (or record (find-employee-record employee-id (cdr files))))))

(define (install-sample-company)
  ;; internal procedures and data
  (define (make-key name)
    (make-record 'name name))
  (define (make-data address salary)
    (list (make-record 'adress address)
          (make-record 'salary salary)))

  (define personell '())
  (define (add-record name address salary)
    (set! personell
      (adjoin-set (make-record (make-key name)
                               (make-data address salary))
                  personell)))

  (add-record "John Smith" "Someway 42" 12345)
  (add-record "J. R. Hotzenplotz" "Roadblock 666" 0)

  (define (get-record name personell)
    (lookup (make-key name) personell))

  (define (get-salary record)
    ; assume every record has a salary
    (if (eq? 'salary (key (car record)))
        (value (car record))
        (get-salary (cdr record))))

  ;; public interface
  (define (tag-file x) (attach-tag-cond 'sample-company-personell x))
  (define (tag-record x) (attach-tag-cond 'sample-company-personell-record x))
  (put 'get-file 'sample-company-personell (lambda () (tag-file personell)))
  (put 'get-record 'sample-company-personell (lambda (id file) (tag-record (get-record id file))))
  (put 'get-salary 'sample-company-personell-record get-salary))

(define (install-other-company)
  ;; internal procedures and data

  ;; this company does simply track the employees' salaries in a single function
  (define (get-record name)
    (cond ((equal? "Homer" name) "Donuts")
          ((equal? "Marge" name) 230)
          ((equal? "Bart" name) 10)
          ((equal? "Lisa" name) 50)
          (else #f)))

  ;; pass through, because the record is the salary
  (define (get-salary salary) salary)

  ;; public interface
  (define (tag-file x) (attach-tag-cond 'other-company-personell x))
  (define (tag-record x) (attach-tag-cond 'other-company-personell-record x))
  (put 'get-file 'other-company-personell (lambda () (tag-file '())))
  (put 'get-record 'other-company-personell (lambda (id file) (tag-record (get-record id))))
  (put 'get-salary 'other-company-personell-record get-salary))


(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (attach-tag-cond type-tag contents)
  (and contents (attach-tag type-tag contents)))

(define (make-record key value) (cons key value))
(define (key record) (car record))
(define (value record) (cdr record))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define (adjoin-set x set)
  (if (lookup x set)
      set
      (cons x set)))

(install-sample-company)
(install-other-company)

(define companies (list (get-file 'sample-company-personell)
                        (get-file 'other-company-personell)))
(println (find-employee-record "John Smith" companies))
(println (find-employee-record "Marge" companies))

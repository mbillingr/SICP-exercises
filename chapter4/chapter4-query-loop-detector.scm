(define QEVAL-HISTORY '())

(define (simple-query query-pattern frame-stream)
  (let ((instances (enter-query query-pattern frame-stream)))
    (cond ((eq? instances 'loop-detected)
           the-empty-stream)
          (else
            (push-history! instances)
            (let ((result (stream-flatmap
                            (lambda (frame)
                              (stream-append
                                (find-assertions query-pattern frame)
                                (apply-rules query-pattern frame)))
                            frame-stream)))
              (pop-history! instances)
              result)))))

; does it really work looking only at the first frame?
(define (enter-query query-pattern frame-stream)
  (let ((instances (instantiate query-pattern
                                (if (null? frame-stream)
                                    the-empty-stream
                                    (stream-car frame-stream))
                                (lambda (v f) (canonical-name v)))))
    (if (find-history instances)
        'loop-detected
        instances)))

(define (canonical-name v)
  (if (number? (cadr v))
      (caddr v)
      (cadr v)))

(define (find-history x)
  (define (iter hist)
    (cond ((null? hist) false)
          ((equal? x (car hist)) true)
          (else (iter (cdr hist)))))
  (iter QEVAL-HISTORY))

(define (push-history! x)
  (set! QEVAL-HISTORY (cons x QEVAL-HISTORY))
  'ok)

(define (pop-history! x)
  (set! QEVAL-HISTORY (cdr QEVAL-HISTORY)))
  

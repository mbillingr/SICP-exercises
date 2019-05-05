(import (builtin core)
        (sicp utils))

(define (logical-or a b) (if (= a 0) b a))
(define (logical-and a b) (if (= a 0) a b))
(define (logical-not s) (if (= s 0) 1 0))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate x1 x2 output)
  (define (and-action-procedure)
    (println "AND gate -- inputs" (get-signal x1) (get-signal x2))
    (let ((new-value (logical-and (get-signal x1) (get-signal x2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (println "AND gate -- setting output to" new-value)
                     (set-signal! output new-value)))))
  (add-action! x1 and-action-procedure)
  (add-action! x2 and-action-procedure)
  'ok)

(define (or-gate x1 x2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal x1) (get-signal x2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! x1 or-action-procedure)
  (add-action! x2 or-action-procedure)
  'ok)

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (= signal-value new-value)
          'done
          (begin (set! signal-value new-value)
                 (call-each action-procedures))))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (println name
                          (current-time the-agenda)
                          "New-value ="
                          (get-signal wire)))))

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (cons (make-new-time-segment time action)
                             segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

; replace queue (fifo) with a normal list (lifo)
(define (make-queue) (list '()))

(define (empty-queue? queue)
  (null? (car queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (car queue))))

(define (insert-queue! queue item)
  (set-car! queue (cons item (car queue)))
  queue)

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-car! queue (cdr (car queue)))
              queue)))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define output (make-wire))

(probe 'output output)

(and-gate input-1 input-2 output)

(propagate)
(println "start")
(set-signal! input-1 0)
(set-signal! input-2 1)
(println "A")
(propagate)

(println "B")

(set-signal! input-1 1)
(set-signal! input-2 0)
(println "C")
(propagate)

(println "end")

; normal output (with queue):
;   output 0 New-value = 0
;   output 6 New-value = 1
;   output 6 New-value = 0

; With a LIFO queue the following happens:
;   Whenever a signal change happens the corresponding actions are triggered
;   after a delay. Because the inputs are read in the beginning only the last
;   queued action has the correct combinations of inputs.
;   If we evaluate the last action first, the correct output is overwritten by
;   the other actions that may have wrong combinations of inputs.

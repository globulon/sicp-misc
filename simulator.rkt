#lang scheme
(require racket/mpair)
(require "queue.rkt")

(define (call-each procedures)
  (if (empty? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (make-wire) 
  (let ((signal-value 0) (actions '()))
    (define (set-signal-value! v)
      (if (not (= signal-value v))
          (begin (set! signal-value v)
                 (call-each actions))
          'ok))
    (define (get-signal-value) 
      signal-value)
    (define (accept-action-procedure! action)
      (set! actions (cons (action actions)))
      (action))
    (define (dispatch m)
      (cond ((= m 'set-signal) set-signal-value!)
            ((= m 'get-signal) get-signal-value)
            ((= m 'add-action) accept-action-procedure!)))
    dispatch))

(define (get-signal wire) 
  ((wire 'get-signal)))

(define (set-signal! wire value)
  ((wire 'set-signal) value))

(define (add-action! wire action) 
  ((wire 'add-action) action))

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time segment)
  (car segment))

(define (segment-queue segment)
  (cdr segment))

(define (make-agenda) (mlist 0))

(define the-agenda (make-agenda))

(define (empty-agenda? agenda) 
  (empty? (segments-agenda agenda)))

(define (first-agenda-item agenda) 
  (if (empty? agenda)
      (error "cannot remove element from empty agenda")
      (let ((segment (first-segment agenda)))
        (let ((item (front-queue (segment-queue segment))))
          (set-current-time! agenda (segment-time segment))
          item))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (cond ((empty-queue? q)
           (set-segments! agenda (rest-segments agenda))))))

(define (add-to-agenda! time action agenda) 
  (define (before-all? segments)
    (or (empty? segments)
        (< time (segment-time (mcar segments)))))
  (define (make-new-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (cond ((before-all? segments)
           (mcons (make-new-segment time action) segments))
          ((= time (segment-time (mcar segments)))
           (insert-queue! (segment-queue (mcar segments)) action)
            segments)
          (else 
           (mcons (mcar segments) (add-to-segments! (mcdr segments))))))
  (let ((segments (segments-agenda agenda)))
    (set-segments! agenda (add-to-segments! segments))))

(define (current-time agenda) (mcar agenda))

(define (set-current-time! agenda time) 
  (set-mcar! agenda time))

(define (segments-agenda agenda) (mcdr agenda))

(define (set-segments! agenda segments)
  (set-mcdr! agenda segments))

(define (first-segment agenda)
  (mcar (segments-agenda agenda)))

(define (rest-segments agenda)
  (mcdr (segments-agenda agenda)))

(define (after-delay timeout action) 
  (add-to-agenda! (+ timeout (current-time the-agenda)) 
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (logical-not value)
  (cond ((= 0 value) 1)
        ((= 1 value) 0)
        (else (error "unexpected value"))))

(define inverter-delay 5)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= 0 s1) (= 0 s2))
         0)
        ((and (= 0 s1) (= 1 s2))
         0)
        ((and (= 1 s1) (= 0 s2))
         0)
        ((and (= 1 s1) (= 1 s2))
         1)
        (else (error "invalid signal"))))

(define and-gate-delay 5)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((and (= 0 s1) (= 0 s2))
         0)
        ((and (= 0 s1) (= 1 s2))
         1)
        ((and (= 1 s1) (= 0 s2))
         1)
        ((and (= 1 s1) (= 1 s2))
         1)
        (else (error "invalid signal"))))


(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) 
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder a s sum c2)
    (half-adder b c-in s c1)
    (or-gate c1 c2 c-out)
    'ok))

(define (or-gate-special a1 a2 output)
  (let ((b1 (make-wire))
        (b2 (make-wire))
        (c (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c)
    (inverter c output)
    'ok))

(define (ripple-carry-adder a b s c-out)
  (let ((c-in (make-wire)))
    (if (empty? (cdr a))
        (set-signal! c-in 0)
        (ripple-carry-adder (cdr a) (cdr b) (cdr s) c-in))
    (full-adder (car a) (car b) c-in (car s) c-out)))
  
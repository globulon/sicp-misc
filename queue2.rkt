#lang r5rs
;;set-car! and set-cdr! are respectively replaced 
;;by set-mcar! and set-mcdr!
;;cons is replaced by mcons as we are dealing with 
;;[m]utable lists

 (define (error reason . args)
      (display "Error: ")
      (display reason)
      (for-each (lambda (arg) 
                  (display " ")
		  (write arg))
		args)
      (newline)
      (scheme-report-environment -1))

(define (make-queue) (cons '() '()))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue) 
  (if (empty-queue? queue)
      (error "front cannot be peeked on empty queue")
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue) 
  (cond ((empty-queue? queue)
         (error "front cannot be removed on empty queue"))
        (else 
         (set-front-ptr! queue (cdr (front-ptr queue))))))
         
(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item ) (set-cdr! queue item))

(define q (make-queue))
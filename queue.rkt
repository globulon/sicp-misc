#lang scheme
;;set-car! and set-cdr! are respectively replaced 
;;by set-mcar! and set-mcdr!
;;cons is replaced by mcons as we are dealing with 
;;[m]utable lists

(define (make-queue) (mcons '() '()))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue) 
  (if (empty-queue? queue)
      (error "front cannot be peeked on empty queue")
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue) 
  (cond ((empty-queue? queue)
         (error "front cannot be removed on empty queue"))
        (else 
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue)))
         
(define (front-ptr queue) (mcar queue))

(define (rear-ptr queue) (mcdr queue))

(define (set-front-ptr! queue item) (set-mcar! queue item))

(define (set-rear-ptr! queue item ) (set-mcdr! queue item))

(define q 
  ((lambda ()
    (let ((a-q (make-queue)))
      (insert-queue! a-q 'a)
      (insert-queue! a-q 'b)
      a-q))))

(define (println-queue queue)
  (display (mcar queue)))

(provide (all-defined-out))

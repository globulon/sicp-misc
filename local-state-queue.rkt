#lang scheme
;;ex 3-22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-q?) (null? front-ptr))
    (define (insert-q! item)
      (let ((new-pair (mcons item '())))
        (cond ((empty-q?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               dispatch)
              (else 
               (set-mcdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               dispatch))))
    (define (delete-q!)
      (cond ((empty-q?) 
             (error "cannot remove from empty queue"))
            (else
             (set! front-ptr (mcdr front-ptr))
             dispatch)))
    (define (print)
      (display front-ptr))
    (define (dispatch m)
      (cond ((equal? m 'insert-q!) insert-q!)
            ((equal? m 'delete-q!) delete-q!)
            ((equal? m 'empty-q?) empty-q?)
            ((equal? m 'print) print)
            (else (error "unknown primitive" m))))
    dispatch))

(define (insert-queue! q item) 
  ((q 'insert-q!) item))

(define (delete-queue! q) 
  ((q 'delete-q!)))

(define (empty-queue? q) 
  ((q 'empty-q?)))

(define (print-q q)
  ((q 'print)))

(define q 
  ((lambda ()
    (let ((a-q (make-queue)))
      (insert-queue! a-q 'a)
      (insert-queue! a-q 'b)
      a-q))))

#lang scheme

(define (make-deque)
  (mcons '() '()))

(define (front-ptr dq)
  (mcar dq))

(define (rear-ptr dq)
  (mcdr dq))

(define (empty-deque? dq)
  (null? (rear-ptr dq)))

(define (front-deque dq)
  (if (empty-deque? dq)
      (error "cannot get front from empty deque")
      (mcar (front-ptr dq))))

(define (rear-deque dq) 
  (if (empty-deque? dq)
    (error "cannot get front from empty deque")
    (mcar (rear-ptr dq))))

(define (make-empty-link)
  (mcons '() '()))

(define (make-cell item)
  (mcons item (make-empty-link)))

(define (link-to-next cell next)
  (set-mcdr! (mcdr cell) next))

(define (link-to-previous cell previous)
  (set-mcar! (mcdr cell) previous))

(define (detach-from-next! cell)
  (if (not (empty? cell))
      (set-mcdr! (mcdr cell) '())
      '()))

(define (detach-from-previous! cell)
  (if (not (empty? cell))
      (set-mcar! (mcdr cell) '())
      '()))

(define (next-cell cell)
  (if (empty? cell)
      '()
      (mcdr (mcdr cell))))

(define (previous-cell cell)
    (if (empty? cell)
      '()
      (mcar (mcdr cell))))


(define (bind previous next)
  (link-to-next previous next)
  (link-to-previous next previous))

(define (front-insert-deque! dq item)
  (let ((new-cell (make-cell item)))
    (cond ((empty-deque? dq)
           (set-mcar! dq new-cell)
           (set-mcdr! dq new-cell)
           dq)
          (else
           (bind new-cell (front-ptr dq))
           (set-mcar! dq new-cell)
           dq))))

(define (front-delete-deque! dq) 
  (cond ((empty-deque? dq)
         (error "cannot remove from empty dequeue"))
        ((eq? (front-ptr dq) (rear-ptr dq))
         (set-mcar! dq '())
         (set-mcdr! dq '())
         dq)
      (else
       (let ((new-front-ptr (next-cell (front-ptr dq))))
         (detach-from-next! (front-ptr dq))
         (detach-from-previous! new-front-ptr)
         (set-mcar! dq new-front-ptr)
         dq))))

(define (rear-insert-deque! dq item)
  (let ((new-cell (make-cell item)))
    (cond ((empty-deque? dq)
           (set-mcar! dq new-cell)
           (set-mcdr! dq new-cell))
          (else
           (bind (rear-ptr dq) new-cell)
           (set-mcdr! dq new-cell)))
    dq))
  
(define (rear-delete-deque! dq)
  (cond ((empty-deque? dq)
         (error "cannot remove from empty dequeue"))
        ((eq? (front-ptr dq) (rear-ptr dq))
         (set-mcar! dq '())
         (set-mcdr! dq '())
         dq)
        (else
         (let ((new-rear-ptr (previous-cell (rear-ptr dq))))
           (detach-from-previous! (rear-ptr dq))
           (detach-from-next! new-rear-ptr)
           (set-mcdr! dq new-rear-ptr)
           dq))))

(define (to-list dq)
  (define (to-list-iter next acc)
    (if (empty? next)
        (reverse acc)
        (let ((next-item (mcar next)))
          (if (member next-item acc)
              (to-list-iter (next-cell next) 
                            acc)
              (to-list-iter (next-cell next) 
                            (cons next-item acc))))))
  (to-list-iter (front-ptr dq) '()))

(define (print-deque dq)
  (display (to-list dq)))

(define dq 
  ((lambda ()
    (let ((new-dq (make-deque)))
      (front-insert-deque! new-dq 'b)
      (front-insert-deque! new-dq 'a)
      new-dq))))

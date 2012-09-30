#lang racket
(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

(define x (mcons 'a (mcons 'b (mcons 'c '()))))

(define (bad-count-pair x)
  (if (not (pair? x))
      0
      (+ (bad-count-pair (car x))
         (bad-count-pair (cdr x))
         1)))


(define (count-pair x)
  (define (iter-count-pair z accumulated)
    (cond (((not (pair? z)) 0)
           ((member (car )))))
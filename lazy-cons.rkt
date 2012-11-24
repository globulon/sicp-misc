#lang racket
;part 4.2.3
(define (cons x y)
  (lambda (f) (f x y)))

(define (car l)
  (l (lambda (x y) x)))

(define (cdr l)
  (l (lambda (x y) y)))


(define (list-ref xs index)
  (cond ((empty? xs)
         (error "Out of range index"))
        ((= 0 index)
         (car xs))
        (else 
         (list-ref (cdr xs) (- index 1)))))

(define (l-map f xs)
  (if (empty? xs)
      '()
      (cons (f (car xs)) (l-map f (cdr xs)))))

(define (scale xs n)
  (if (empty? xs)
      '()
      (cons (* n (car xs))
            (scale (cdr xs) n))))

(define (add-list xs ys)
  (cond ((empty? xs) ys)
        ((empty? ys) xs)
        (else 
         (cons (+ (car xs) (car ys))
               (add-list (cdr xs) (cdr ys))))))

(define ones (cons 1 ones))

(define integers (cons 1 (add-list integers ones)))
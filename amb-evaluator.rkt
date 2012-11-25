#lang racket
(define (amb) '())

(define (require p)
  (if (not p) 
      (amb)
      'ok))

(define (an-element-of items)
  (require (not (empty? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ 1 n))))


;4-35
(define (square x) (* x x))

(define (an-integer-between low high)
  (require (< low high))
  (amb low (an-integer-between (+ 1 low) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (square k) (+ (square i) (square j))))
        (list i j k)))))


(define (all-pythagorean-triples-from a)
  (let ((k (an-integer-starting-from a)))
    (let ((j (an-integer-between a k)))
      (let ((i (an-integer-between a j)))
        (require (= (square k) (+ (square i) (square j))))
        (list i j k)))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;4-40
(define (multiple-dwelling-2)
  (let ((cooper (amb 2 3 4 5))
        (miller (amb 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 2 3 4)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((baker (amb 1 2 3 4 5)))
          (require
           (distinct? (list baker cooper fletcher miller smith)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))

;4-41
(define (reduce x0 f xs)
  (define (iter acc remaining)
    (if (empty? remaining)
        acc
        (iter (f acc (car remaining))
              (cdr remaining))))
  (iter x0 xs))

(define (flatten l)
  (reduce '() append l))

(define (miller-levels miller)
  (define (greater-than n)
    (lambda (x) (> x n)))
  (lambda (level) 
    (let ((filtered (filter (greater-than level) miller)))
      (map (lambda (x) (list level x)) filtered))))

(define (not-adjacent-levels target-levels)
  (define (not-adjacent n)
    (lambda (x) (not (= 1 (abs (- x n))))))
  (lambda (levels)
    (let ((filtered (filter (not-adjacent (car levels)) target-levels)))
      (map (lambda (x) (cons x levels)) filtered))))

(define (combine items ls)
  (flatten (map (lambda (item)
         (map (lambda (l) (cons item l)) ls))
       items)))

(define (multiple-dwelling-3)
  (let ((cooper (list 2 3 4 5)))
    (let ((cm (flatten (map (miller-levels (list 3 4 5)) cooper))))
      (let ((fcm (flatten (map (not-adjacent-levels (list 2 3 4)) cm))))
        (let ((sfcm (flatten (map (not-adjacent-levels (list 1 2 3 4 5)) fcm))))
          (let ((bsfcm (combine (list 1 2 3 4) sfcm)))
            (map (lambda (result)
                    (list (list 'baker (car result))
                          (list 'cooper (cadddr result))
                          (list 'fletcher (caddr result))
                          (list 'miller (car (cdr (cdddr result))))
                          (list 'smith (cadr result))))
                 (filter distinct? bsfcm))))))))
    
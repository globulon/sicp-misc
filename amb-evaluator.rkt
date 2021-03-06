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

;4-43
(define (schoolgirls-chart)
  (define (xor a b)
    (or (and a (not b)) (and (not a) b)))
  (let ((kitty (amb 1 2 3 4 5))
        (betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan  (amb 1 2 3 4 5))
        (mary  (amb 1 2 3 4 5)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= ethel 5) (= joan 3)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= betty 1) (= mary 4)))
    (require
     (distinct? (list kitty betty ethel joan mary)))
    (list (list 'kitty kitty)
          (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'mary mary))))
    
;4-43
(define (yachts)
  (define (father-of girl configs)
    (filter (lambda (list) (eq? girl (caddr list))) configs))
  (let ((moor-daughter (amb 'mary-ann 'gabby 'rosalind))
        (hall-daugther (amb 'mary-ann 'gabby 'lorna ))
        (downing-daughter (amb 'mary-ann 'gabby 'lorna 'rosalind))
        (parker-daughter (amb 'gabby 'lorna 'rosalind)))
    (let ((moor (list 'moore 'lorna moor-daughter))
          (hall (list 'hall 'rosalind hall-daugther))
          (downing (list 'downing 'melissa downing-daughter))
          (parker (list 'parker 'mary-ann parker-daughter)))
      (let ((configs (list moor hall downing parker)))
        (let ((gabby-father (father-of 'gabby configs)))
          (require (eq? (cadr gabby-father) (caddr parker)))
          (require (distinct? (map caddr configs)))
            configs)))))

;4-44
(define (queens)
  (define (safe-col? queen positions)
    (let ((col (cadr queen))
          (cols (map cadr queen)))
      (empty? (filter (lambda (x) (= x col)) cols))))
  (define (safe-diagonal? queen positions)
    (let ((row (car queen)) (col (cadr queen)))
      (empty? 
       (filter
        (lambda (position)
          (= 1 (abs (/ (- row (car position))
                       (- col (cadr position))))))
        positions))))
  (define (safe? queen positions)
    (if (safe-col? queen positions)
        (safe-diagonal? queen positions)
        #f))
  (let ((q1 (list 1 (amb 1 2 3 4 5 6 7 8))))
    (let ((q2 (list 2 (amb 1 2 3 4 5 6 7 8))))
      (require (safe? q2 (list q2)))
      (let ((q3 (list 3 (amb 1 2 3 4 5 6 7 8))))
        (require (safe? q3 (list q1 q2)))
        (let ((q4 (list 4 (amb 1 2 3 4 5 6 7 8))))
          (require (safe? q4 (list q1 q2 q3)))
          (let ((q5 (list 5 (amb 1 2 3 4 5 6 7 8))))
            (require (safe? q5 (list q1 q2 q3 q4)))
            (let ((q6 (list 6 (amb 1 2 3 4 5 6 7 8))))
              (require (safe? q6 (list q1 q2 q3 q4 q5)))
              (let ((q7 (list 7 (amb 1 2 3 4 5 6 7 8))))
                (require (safe? q7 (list q1 q2 q3 q4 q5 q6)))
                (let ((q8 (list 8 (amb 1 2 3 4 5 6 7 8))))
                  (require (safe? q8 (list q1 q2 q3 q4 q5 q6 q7)))
                  (list q1 q2 q3 q4 q5 q6 q7 q8))))))))))
                  
          
    
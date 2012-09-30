#lang racket
(define (variable? x) (symbol? x))

(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (=number? var num)
  (and (number? var) (= var num)))

(define (make-sum x y) 
  (cond ((=number? x 0) y)
        ((=number? y 0) x)
        ((and (number? x) (number? y)) (+ x y))
        ( else (list '+ x y))))
  
(define (make-product x y) 
  (cond ((=number? x 0) 0)
        ((=number? y 0) 0)
        ((=number? x 1) y)
        ((=number? y 1) x)
        ((and (number? x) (number? y)) (* x y)) 
        (else (list '* x y))))

(define (op exp)
  (car exp))

(define (is? operation expr) (eq? operation (op expr)))

(define (sum? expr) (is? '+ expr))

(define (product? expr) (is? '* expr))

(define (addend sum-expr) 
  (cadr sum-expr))

(define (augend sum-expr) 
  (let ((rest  (cddr sum-expr)))
    (if (= 1 (length rest))
        (car rest)
        (cons '+ rest))))

(define (multiplier prod-expr) (cadr prod-expr))

(define (multiplicand prod-expr) 
  (let ((rest (cddr prod-expr)))
    (if (= 1 (length rest))
        (car rest )
        (cons '* rest))))

(define (make-exponentiation base exponent)
  (cond ((= 0 exponent) 1)
        ((= 1 exponent) base)
        ((and (number? base)(= 0 base)) 0)
        ((and (number? base)(= 1 base)) 1)
        ( else (list '** base exponent))))

(define (exponentiation? expression)
  (is? '** expression))

(define (base expression)
  (cadr expression))
              
(define (exponent expression)
  (caddr expression))

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr) 
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum 
          (make-product (multiplier expr)
                        (deriv (multiplicand expr) var))
          (make-product (deriv (multiplier expr) var)
                        (multiplicand expr))))
        ((exponentiation? expr)
         (make-product
           (make-product 
            (exponent expr)
            (make-exponentiation (base expr) (- (exponent expr) 1)))
           (deriv (base expr) var)))
        (else (display "unknown expression type"))))
  

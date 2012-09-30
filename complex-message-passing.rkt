#lang racket
(define (square x) (* x x))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Badly typed data")))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Badly typed data")))

(define (rectangular? datum)
  (eq? (type-tag datum) 'rectangular))

(define (polar? datum)
  (eq? (type-tag datum) 'polar))

(define (apply-generic op arg) (arg op))

(define (real-part z)
  (apply-generic 'real-part z))

(define (imag-part z)
  (apply-generic 'imag-part z))

(define (magnitude z)
  (apply-generic 'imag-part z))

(define (angle-part z)
  (apply-generic 'angle-part z))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((= op 'real-part ) x)
          ((= op 'imag-part ) y)
          ((= op 'magnitude ) (sqrt (+ (square x) (square y))))
          ((= op 'angle ) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang r theta)
  (define (dispatch op)
    (cond ((= op 'real-part ) (* r (cos theta)))
          ((= op 'imag-part ) (* r (sin theta)))
          ((= op 'magnitude ) r)
          ((= op 'angle ) theta)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

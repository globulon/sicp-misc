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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular 
              (cons x y)))

(define (make-from-mag-ang-rectangular r t)
  (attach-tag 'rectangular 
              (cons (* r (cos t)) (* r (sin t)))))

(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z) 
  (sqrt (+ (square (real-part z)) (square (imag-part z)) )))

(define (angle-rectangular z)
  (atan (imag-part z) (real-part z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar 
              (cons (sqrt (+ (square x) (square y))) 
                    (atan y x))))

(define (make-from-mag-ang-polar r t)
  (attach-tag 'polar (cons (r t))))

(define (real-part-polar z )
  (* (magnitude z) (cos (angle z))))

(define (imag-part-polar z )
  (* (magnitude z) (sin (angle z))))

(define (magnitude-polar z)
  (car z))

(define (angle-polar z)
  (cdr z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get op args) '())

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply op (map contents args))
          (error "No method for these types" 
                 (list op args))))))

(define (real-part z)
  (apply-generic 'real-part z))

(define (imag-part z)
  (apply-generic 'imag-part z))

(define (magnitude z)
  (apply-generic 'imag-part z))

(define (angle-part z)
  (apply-generic 'angle-part z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r t)
  ((get 'make-from-nag-ang 'polar) r t))

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



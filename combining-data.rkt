#lang racket
;Exercise 2.78
(define (get op type-tags) '())
(define (put op type item) '())

(define (put-coercion t1 t2 t1->t2) '())
(define (get-coercion t1 t2) '())

(define types '((scheme-number 0) (rational 1) (real 2) (complex 3)))

(define (type-index t)
  (define (type-index-iter to-do)
    (cond ((empty? to-do) (error "type not found"))
          ((eq? t (caar to-do)) (cadar to-do))
          (else (type-index-iter(cdr to-do)))))
  (type-index-iter types))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply op (map contents args))
          (if (= 2 (length args))
              (let ((t1 (car type-tags))
                    (t2 (cadr type-tags))
                    (val1 (car args))
                    (val2 (cadr args)))
                (cond ((< t1 t2) (apply-generic op (raise t1) t2))
                      ((> t1 t2) (apply-generic op t1 (raise t2)))
                      ((= t1 t2) (error "No method for type" (list op t1)))))
                (error "No method found for this types" 
                     (list op args)))))))
    
(define (square x) (* x x))

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
  (sqrt (+ (square (real-part-rectangular z)) (square (imag-part-rectangular z)) )))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))

(define (equality-rectangle z1 z2)
  (let ((r real-part-rectangular) (i imag-part-rectangular))
    (and (= (r z1) (r z2))
         (= (i z1) (i z2)))))

(define (=zero?-rectangle z)
  (and (= 0 (real-part-rectangular z))
       (= 0 (imag-part-rectangular z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar 
              (list (sqrt (+ (square x) (square y))) 
                    (atan y x))))

(define (make-from-mag-ang-polar r t)
  (attach-tag 'polar (list r t)))

(define (real-part-polar z )
  (* (magnitude z) (cos (angle z))))

(define (imag-part-polar z )
  (* (magnitude z) (sin (angle z))))

(define (magnitude-polar z)
  (car z))

(define (angle-polar z)
  (cdr z))

(define (equality-polar z1 z2)
  (let ((r magnitude-polar) (t angle-polar))
    (and (= (r z1) (r z2))
         (= (t z1) (t z2)))))

(define (=zero?-polar z) (= 0 (magnitude-polar z)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag contents)
  (cond ((number? contents) 'scheme-number)
        ((symbol? contents) 'scheme-symbol)
        ((pair? contents) (car contents))
        (else (error "Invalid input content"))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((symbol? contents) datum)
        ((pair? contents) (cdr contents))
        (else (error "Invalid input content"))))


(define (install-equality)
  (define (equality-complex z1 z2)
    (apply-generic 'equality? z1 z2))
  (put 'equality? '(scheme-number scheme-number) =)
  (put 'equality? '(rectangular rectangular) equality-rectangle)
  (put 'equality? '(polar polar) equality-polar)
  (put 'equality? '(complex complex) equality-complex))
  

(define (equality? number1 number2)
  (apply-generic 'equality?  number1 number2))

(define (install-zero?)
  (define (=zero?-complex z1 z2)
    (apply-generic '=zero? z1 z2))
  (put '=zero? '(scheme-number scheme-number) (lambda(x) (= 0 x)))
  (put '=zero? '(rectangular rectangular) =zero?-rectangle)
  (put '=zero? '(polar polar) =zero?-polar)
  (put '=zero? '(complex complex) =zero?-complex))



(define (scheme-number->complex n)
  (make-from-real-imag (contents n) 0))

(define (make-rat n d)
  (attach-tag 'rational (list n d)))

(define (make-real n)
  (attach-tag 'real (list n)))

(define (numer rat)
  (car (contents rat)))
(define (denum rat)
  (cadr (contents rat)))

(define (install-raises)
  (put 'raise '(scheme-number)
       (lambda(n)
         (if (exact? n) 
             (make-rat n 1)
             (make-real 0.0))))
  (put 'raise '(rational)
       (lambda (n)
         (make-real (/ (* 1.0 (numer n)) (denum)))))
  (put 'raise '(real)
       (lambda(r)
         (make-from-real-imag (contents r) 0.0))))

(define (raise value)
  (apply-generic 'raise value))

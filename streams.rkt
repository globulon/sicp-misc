#lang scheme
(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! already-run? #t)
            (set! result (proc))
            result)
          result))))
  
;(define (delay exp) 
;  (memo-proc (lambda () exp)))

;(define (force delayed)
 ; (delayed))

;(define (cons-stream a b)
;  (cons a (delay b)))

(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))]))

(define (stream-car s) (car s))

(define (stream-cadr s)
  (stream-car (stream-cdr s)))

(define (stream-cdr s) (force (cdr s)))

(define (empty-stream? s) (empty? s))

(define the-empty-stream '())

(define (stream-ref s n)
  (cond ((empty-stream? s) the-empty-stream)
        ((= n 0) (stream-car s))
        (else (stream-ref (stream-cdr s) (- n 1)))))

(define (stream-map f . streams)
  (if (empty-stream? (car streams))
      the-empty-stream
      (cons-stream
       (apply f (map stream-car streams))
       (apply stream-map 
              (cons f (map stream-cdr streams))))))

(define (stream-for-each proc s)
  (if (empty-stream? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
  (newline)
  (display x))

(define (show-stream s n)
  (display-stream (take n s)))

(define (display-stream s)
  (stream-for-each display-line s))

(define  (show x)
  (display-line x)
  x)

(define (stream-filter predicate s)
  (cond ((empty-stream? s) the-empty-stream)
        ((predicate (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter predicate 
                                     (stream-cdr s))))
        (else
         (stream-filter predicate (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ 1 low) high))))

(define a-stream (cons-stream 1 (cons-stream 2 (cons-stream 3 the-empty-stream))))

(define (divisible? n)
  (lambda (value)
    (zero? (remainder value n))))

(define (not-divisible? n)
  (lambda (value)
    (not (zero? (remainder value n)))))
  

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ 1 n))))

(define integers (integers-starting-from 0))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ b a))))

(define fibs (fibgen 0 1))

(define (take n s)
  (cond ((empty-stream? s) the-empty-stream)
        ((zero? n) the-empty-stream)
        (else (cons-stream (stream-car s)
                           (take (- n 1) (stream-cdr s))))))

(define (sieve stream)
  (cons-stream (stream-car stream)
               (sieve (stream-filter (not-divisible? (stream-car stream))
                                     (stream-cdr stream)))))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define factorials (cons-stream 1 (mul-streams (integers-starting-from 2) factorials)))

(define (partial-sums s)
    (cons-stream (stream-car s) 
                 (add-streams (stream-cdr s) 
                              (partial-sums s))))

(define (merge-stream s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        ((< (stream-car s1) (stream-car s2))
         (cons-stream (stream-car s1)
                      (merge-stream (stream-cdr s1) s2)))
        ((> (stream-car s1) (stream-car s2))
         (cons-stream (stream-car s2)
                      (merge-stream s1 (stream-cdr s2))))
        (else 
         (cons-stream (stream-car s1)
                      (merge-stream (stream-cdr s1) (stream-cdr s2))))))

(define S (cons-stream 1 
                       (merge-stream (scale-stream S 2) 
                                     (merge-stream (scale-stream S 3) 
                                                   (scale-stream S 5)))))

(define (expand num den radix)
  (cons-stream (quotient (* num radix) den)
               (expand (remainder (* num radix) den) den radix)))

(define (div-stream s1 s2)
  (stream-map / s1 s2))

(define (integrate-series s)
  (mul-streams s 
               (div-stream ones 
                           (integers-starting-from 1))))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (stream-map * 
                             (scale-stream ones -1) 
                             (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (add-streams (scale-stream (stream-cdr s1)(stream-car s2))
                                         (scale-stream (stream-cdr s2)(stream-car s1)))
                            (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))

(define (invert-unit-series s)
  (cons-stream 1 
               (scale-stream (mul-series (stream-cdr s)
                                         (invert-unit-series s))
                             -1)))

(define (div-series s1 s2)
  (if (zero? (stream-car s2))
      (error "cannot divide with denominator constant null")
      (let ((factor (/ stream-car s2)))
        (scale-stream (mul-series s1 
                                  (invert-unit-series (scale-stream s2 
                                                                    factor)))
                      factor))))
(define (sqrt-improve guess x)
  (define (average a b)
    (/ (+ a b) 2.0))
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x))           
                             guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ 2.0 n)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1.0)) 4))

(define (euler-transform s)
  (define (square x) (* x x))
  (let ((sn-1 (stream-ref s 0))
        (sn   (stream-ref s 1))
        (sn+1 (stream-ref s 2)))
    (let ((num (square (- sn+1 sn))))
      (let ((denum (+ (- sn-1 (* 2 sn)) sn+1)))
        (cons-stream (- sn+1 (/ num denum))
                     (euler-transform (stream-cdr s)))))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (stream-limit s tolerance)
  (define (stream-cadr sub-s)
    (stream-car (stream-cdr sub-s)))
  (define (delta sub-s)
    (let ((first (stream-car sub-s))
          (second (stream-cadr sub-s)))
      (abs (- second first))))
  (define (iter-stream sub-s)
    (if (< (delta sub-s) tolerance)
        (stream-cadr sub-s)
        (iter-stream (stream-cdr sub-s))))
  (iter-stream s))


(define (ln-summands n)
  (cons-stream (/ 1.0 n) 
               (stream-map - (ln-summands (+ 1.0 n)))))

(define ln-2
  (accelerated-sequence euler-transform
                        (partial-sums (ln-summands 1.0))))

(define (interleave s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
  
(define (pairs s t)
  (cons-stream 
   (list (stream-car s) (stream-car t))
   (interleave 
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave-all s1 s2 s3)
  (cond ((empty-stream? s1)
         (interleave s2 s3))
        ((empty-stream? s2)
         (interleave s1 s3))
        ((empty-stream? s3)
         (interleave s1 s2))
        (else 
         (cons-stream 
          (stream-car s1)
          (cons-stream
           (stream-car s2)
           (cons-stream
            (stream-car s3)
            (interleave-all
             (stream-cdr s1)
             (stream-cdr s2)
             (stream-cdr s3))))))))

(define (full-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave 
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (stream-map (lambda (x)
                  (list x (stream-car t)))
                (stream-cdr s))
    (full-pairs (stream-cdr s) (stream-cdr t)))))

(define (invalid-pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x)) t)
   (invalid-pairs (stream-cdr s) (stream-cdr t))))
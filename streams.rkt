#lang scheme
(require srfi/27)

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! already-run? #t)
            (set! result (proc))
            result)
          result))))
  
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
  
(define (repeat n)
    (cons-stream n (repeat n)))

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

(define (square x) (* x x))                 

 
(define (triples s t u)
  (cons-stream 
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (pair) (cons (stream-car s) pair))
                (pairs t (stream-cdr u)))
    (triples (stream-cdr s) 
            (stream-cdr t) 
            (stream-cdr u)))))

(define (pythagore? triple)
  (zero? (- (square (caddr triple))
            (+ (square (car triple))
               (square (cadr triple))))))


(define pythagorean-triples
  (stream-filter (lambda (triple) (pythagore? triple))
                 (triples integers integers integers)))

(define (merge-weighted s1 s2 weight)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        ((<= (weight (stream-car s1)) (weight (stream-car s2)))
         (cons-stream (stream-car s1)
                      (merge-weighted (stream-cdr s1) s2 weight)))
        (else
         (cons-stream (stream-car s2)
                      (merge-weighted s1 (stream-cdr s2) weight)))))

(define (weighted-pairs s t weight)
  (cons-stream 
   (list (stream-car s) (stream-car t))
   (merge-weighted 
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define sorted-a
  (weighted-pairs integers 
                  integers 
                  (lambda (pair) (+ (car pair) 
                                    (cadr pair)))))

(define (div? m n)
  (zero? (remainder m n)))

(define (not-div? m . ns)
  (cond ((empty? ns) #t)
        ((div? m (car ns)) false)
        (else (apply not-div? (cons m (cdr ns))))))


(define sort-b
  (stream-filter
   (lambda (pair) 
     (let ((predicate (lambda (value) 
                        (not-div? value 2 3 5))))
       (and (predicate (car pair)) (predicate (cadr pair)))))
   (weighted-pairs integers
                   integers
                   (lambda (pair)
                     (let ((i (car pair))
                           (j (cadr pair)))
                       (+ (* 2 i) (* 3 j) (* 5 i j)))))))

(define (cube x) (* x x x))

(define (sum-cubes pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (cube i) (cube j))))

(define ordered-cubes
  (weighted-pairs integers
                  integers
                  sum-cubes))

(define (pick-ramanujan-nums s)
  (let ((first (stream-car s))
        (second (stream-car (stream-cdr s))))
    (if (= (sum-cubes first) (sum-cubes second))
        (cons-stream (sum-cubes first)
                     (pick-ramanujan-nums (stream-cdr (stream-cdr s))))
        (pick-ramanujan-nums (stream-cdr s)))))

(define (sum-squares pair)
  (+ (square (car pair))
     (square (cadr pair))))

(define ordered-squares
  (weighted-pairs integers
                  integers
                  sum-squares))

(define (pick-squares s)
  (let ((first (stream-car s))
        (second (stream-car (stream-cdr s)))
        (third (stream-car (stream-cdr (stream-cdr s)))))
    (if (apply = (map sum-squares (list first second third)))
        (cons-stream (list (sum-squares first) first second third)
                     (pick-squares (stream-cdr (stream-cdr (stream-cdr s)))))
        (pick-squares (stream-cdr s)))))

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)
        
(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1.0 C))
                           v0
                           dt))))

(define (sign-change-detector previous current)
  (cond ((and (> previous 0) (< current 0))
         -1)
        ((and (< previous 0) (> current 0))
         1)
        (else 0)))

(define (zero-crossings)
  (lambda (sensed-data)
    (stream-map sign-change-detector 
                sensed-data 
                (cons-stream 0 sensed-data))))

;(define (make-zero-crossings input-stream last-value)
;  (cons-stream 
;   (sign-change-detector (stream-car input-stream) last-value)
;   (make-zero-crossings (stream-cdr input-stream) (stream-car input-stream))))

;(define (smooth-make-zero-crossings input-stream last-value last-avg)
;  (let ((avg (/ (+ (stream-car input-stream)) 2)))
;    (cons-stream (sign-change-detector avg last avg)
;                 (smooth-make-zero-crossings (stream-cdr input-stream)
;                                             (stream-car input-stream)
;                                             avg))))

(define (smooth s)
  (let ((avg (/ (+ (stream-car s) (stream-car (stream-cdr s))) 2)))
    (cons-stream avg 
                 (smooth (stream-cdr s)))))

(define (make-zero-crossings smooth)
  (lambda (s)
    (stream-map sign-change-detector 
                (smooth s)
                (smooth (cons 0 s)))))


(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (integral-2 delayed-integrand initial-value dt)
  (define int 
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (if (empty-stream? integrand)
                       the-empty-stream
                       (integral-2 (delay (stream-cdr integrand))
                                   (+ (* dt (stream-car integrand)))
                                   dt)))))
  int)

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
  y)

(define (solve-2nd-2 f a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ (* -1 C))))
    (define diL (add-streams (scale-stream vC (/ 1 L))
                             (scale-stream iL (/ (* -1 R) L))))
    (stream-map cons vC iL))) 

(define rlc-circuit ((RLC 1.0 1.0 0.2 0.1) 10 0.0))

(define (rand-update x)
  (let ((a (expt 2 32))
        (c 1103515245)
        (m 12345))
    (modulo (+ (* a x) c) m)))

(define random-init 137)

(define random-numbers 
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define (random-stream requests)
  (define (react value order)
    (if (eq? 'update order)
        (rand-update value)
        order))
  (cons-stream random-init
               (stream-map react (random-stream requests) requests)))
        
(define (map-successive f s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream (f (stream-car s) (stream-car (stream-cdr s)))
                   (map-successive f (stream-cdr (stream-cdr s))))))

(define cesaro-stream
  (map-successive (lambda (a b) (= (gcd a b) 1))
                  random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream (/ passed (+ passed failed))
                 (monte-carlo (stream-cdr experiment-stream)
                              passed
                              failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1.0) failed)
      (next passed (+ 1.0 failed))))


(define pi 
  (stream-map (lambda (stats) (sqrt (/ 6.0 stats)))
              (monte-carlo cesaro-stream 0.0 0.0)))  

(define (random-in-range interval)
  (let ((low (car interval))
        (high (cadr interval)))
  (let ((range (- high low)))
    (+ low (* (random-real) range)))))

(define (estimate-integral predicate x1 y1 x2 y2)
  (define random-coordinates 
    (stream-map (lambda (ranges) (map random-in-range ranges))
                (repeat (list (list x1 x2) (list y1 y2)))))
  (define trials 
    (stream-map predicate random-coordinates))
  (monte-carlo trials 0.0 0.0))
 
(define (make-circle-predicate x1 y1 x2 y2)
  (define cx (/ (+ x1 x2) 2.0))
  (define cy (/ (+ y1 y2) 2.0))
  (define rr (square (/ (- x2 x1) 2.0)))
  (lambda (coordinates)
    (let ((x (car coordinates))
          (y (cadr coordinates)))
      (let ((xx (square (- x cx)))
            (yy (square (- y cy))))
        (<= (+ xx yy) rr)))))

(define (estimate-circle-integral x1 y1 x2 y2)
  (estimate-integral (make-circle-predicate x1 y1 x2 y2)
                     x1 y1 x2 y2))

(define (estimate-pi n)
  ( * 4 (stream-ref (estimate-circle-integral -1.0 -1.0 1.0 1.0) n)))
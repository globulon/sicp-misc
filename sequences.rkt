(define (filter predicate seq)
  (cond ((null? seq) '())
        ((predicate (car seq)) 
         (cons (car seq) 
               (filter predicate (cdr seq))))
        ( else 
          (filter predicate (cdr seq)))))

(define (accumulate operation zero seq)
  (if (null? seq) 
      zero
      (operation 
       (car seq) 
       (accumulate operation zero (cdr seq)))))

(define (fold-right operation zero seq)
  (accumulate operation zero seq))

(define (fold-left op initial-value items)
  (define (iter result rest)
    (if (null? rest)
        result 
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial-value items))

(define (reverse-right seq)
  (fold-right
   (lambda (x y) (append y (list x)))
   '() 
   seq))

(define (reverse-left seq)
  (fold-left (lambda (x y) (cons y x)) '() seq))


(define (map-high p seq)
  (accumulate (lambda (x y) (cons (p x) y)) '() seq))

(define (length-high seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

(define (append-high seq1 seq2)
  (accumulate cons seq2 seq1))

(define (horner-eval x coefficients)
  (define (horner-eval-partial this-coeff higher-terms)
    (+ this-coeff (* x higher-terms)))
  (accumulate horner-eval-partial
              0
              coefficients))

(define (count-leaves tree)
  (accumulate 
   + 
   0 (map 
      (lambda (x) (if (pair? x) (count-leaves x) 1)) 
      tree)))
         
(define (accumulate-n op from-zero seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op from-zero (map car seqs))
            (accumulate-n op from-zero (map cdr seqs)))))



(define a-matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define (product-dot v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (product-dot x v)) m))

(define (transpose matrix)
  (accumulate-n cons '() matrix))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-line) (matrix-*-vector cols m-line))  m)))


(define (enumerate-interval a b)
  (define (iter result a b)
    (if (< b a)
        result
        (iter (cons b result) a (- b 1))))
  (iter '() a b))

(define (flat-map proc seq)
  (fold-right append '() (map proc seq)))

(define (combine a b)
  (flat-map (lambda (i) 
              (map (lambda (j) (list i j)) 
                   (enumerate-interval a i))) 
            (enumerate-interval a b)))

(define (isprime? n)
  (define (square x) (* x x))
  (define (iter current)
    (cond ((> (square current) n) #t)
          ((= (remainder n current) 0) #f)
          (else (iter (+ current 2)))))
  (if ( = 0 (remainder n 2))
      #f
      (iter 3)))
      
(define (prime-sum? pair)
  (isprime? (+ (car pair) (cadr pair))))

(define (make-sum-pair pair)
  (list (list (car pair) (cadr pair)) (+ (car pair) (cadr pair))))


(define (filtered-primes a b)
  (map make-sum-pair 
       (filter prime-sum? (combine a b))))


(define (remove item from-list)
  (filter (lambda (x) (not (= item x))) from-list))

(define (permutations s)
  (if (null? s)
      '()
      (flat-map (lambda (x) 
                 (map (lambda (y) 
                        (display (cons x y))
                        (cons x y)) 
                      (permutations (remove x s))))
               s)))

(define (permutations-on s)
  (if (null? s)
      (list '())
      (flat-map (lambda (x) 
                      (map (lambda (y) (cons x y))
                      (permutations-on (remove x s))))
               s)))

(define (unique-pairs n)
  (flat-map (lambda (x) 
              (map (lambda (y) (list x y))
                   (enumerate-interval 1 x)))
            (enumerate-interval 1 n)))

(define (triples n)
  (flat-map (lambda (x)
              (map (lambda (y) (cons x y))
                        (unique-pairs x)))
            (enumerate-interval 1 n)))
                        
  
(define (matching-triples n s) 
  (filter (lambda (triple) 
            (sum-match (car triple) 
                       (cadr triple) 
                       (caddr triple)
                       s))
          (triples n)))
  
(define (sum-match x y z s)
  (= s (+ x y z)))

(define (adjoin-position new-row k rest-of-queens)
  (cons (list k new-row) rest-of-queens))

(define (unsafe? current position)
  (or (= (cadr current) (cadr position))
      (= 1 (abs (/  (- (car  current) (car position))
                    (- (cadr current) (cadr position)))))))

(define (safe? positions)
  (define (check remaining position)
    (cond ((null? remaining) #t)
          ((unsafe? (car remaining) position) #f)
          ( else (check (cdr remaining) position))))
  (check (cdr positions) (car positions)))
        
;Cheated no need of k in the safe? method
(define (queens board-size)
  (define (queen-cols k)
    (if (= 0 k)
        (list '())
        (filter (lambda (positions) (safe? positions))
                (flat-map 
                 (lambda (rest-of-queens)
                   (map 
                    (lambda (new-row)
                      (adjoin-position new-row k rest-of-queens))
                    (enumerate-interval 1 board-size)))
                 (queen-cols (- k 1))))))
  (queen-cols board-size))




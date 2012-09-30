(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        ( else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set) 
      set
      (cons x set)))

(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((element-of-set? (car s1) s2) 
         (cons (car s1) (intersection-set (cdr s1) s2)))
        ( else (intersection-set (cdr s1) s2))))

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((element-of-set? (car s1) s2) (union-set (cdr s1) s2))
        ( else (cons (car s1) (union-set (cdr s1) s2)))))

(define (element-of-ordered-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        ( else (element-of-ordered-set? x (cdr set)))))

(define (intersection-of-ordered-set s1 s2)
  (if (or (null? s1) (null? s2)) 
      '()
      (let ((x1 (car s1)) (x2 (car s2)))
        (cond ((= x1 x2) (cons x1 (intersection-of-ordered-set 
                                   (cdr s1) (cdr s2))))
              ((< x1 x2) (intersection-of-ordered-set (cdr s1) s2))
              ((> x1 x2) (intersection-of-ordered-set s1 (cdr s2)))))))

(define (adjoin-ordered-set x set)
    (cond ((null? set) (list x))
          ((= x (car set)) set)
          ((< x (car set)) (cons x set))
          ((> x (car set)) (cons (car set) 
                                 (adjoin-ordered-set x (cdr set))))))

(define (union-ordered-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else (let ((x1 (car s1)) (x2 (car s2)))
                (cond ((= x1 x2) 
                       (cons x1 (union-ordered-set (cdr s1) 
                                                   (cdr s2))))
                      ((< x1 x2) (cons x1 (union-ordered-set (cdr s1) s2)))
                      ((> x1 x2) (cons x2 (union-ordered-set s1 (cdr s2)))))))))
                

              
                
                                     

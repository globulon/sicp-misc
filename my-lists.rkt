(define (scale-list items factor)
  (if (null? items) 
      '()
      (cons (* factor (car items))
            (scale-list (cdr items) factor))))

(define (my-map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (my-for-each proc items)
  (if (null? items) 
      #t 
      (let()
        (proc (car items))
        (my-for-each proc (cdr items)))))

(define (count-leaves item)
  (cond ((null? item) 0)
        ((not (pair? item)) 1)
        (else (+ (count-leaves (car item))
                 (count-leaves (cdr item))))))

(define (deep-reverse list)
  (define (iter accumulated current)
    (if (null? current)
        accumulated
        (let ((first (car current)))
          (if (pair? first)
              (iter (cons (deep-reverse first) accumulated) (cdr current))
              (iter (cons first accumulated) (cdr current))))))
  (iter '() list))

(define (fringe item)
  (if (null? item) '()
      (let ((first (car item)))
        (if (pair? first) 
            (append (fringe first) (fringe(cdr item)))
            (cons first (fringe (cdr item)))))))

(define (fringe-iter item)
  (define (iter accumulated current)
      (cond ((null? current) accumulated)
            ((not (pair? (car current))) (iter (cons (car current) accumulated) (cdr current)))
            (else (iter (append (iter '() (car current)) accumulated) (cdr current)))))
  (iter '() item))
             
      

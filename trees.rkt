(define (scale-tree-first tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
              (scale-tree (cdr tree) factor)))))

(define (square-tree-direct tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree-direct (car tree))
              (square-tree-direct (cdr tree))))))

(define (tree-map proc tree)
  (define (item-map item)
    (cond ((null? item) '())
          ((not (pair? item)) (proc item))
          (else (map item-map item))))
  (map item-map tree))

(define (scale-tree tree factor) 
  (tree-map (lambda (x) (* factor x)) tree))

(define (square-tree tree) 
  (tree-map (lambda (x) (* x x)) tree))

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))



